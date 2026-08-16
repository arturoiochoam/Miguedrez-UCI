;;; -*- encoding: utf-8 -*-
;;;
;;; ai.lisp
;;; Evaluation and PVS search.
;;; gamallo, February 11, 2007
;;; UCI port and cleanup by Arthur Matheus, 2026.
;;; 0.95.5 cleanups by Claude Code.
;;;

;; Copyright 2007, 2008, 2009 Manuel Felipe Gamallo Rivero.
;; Copyright 2026 Arthur Matheus (UCI port and further releases).

;;     This file is part of Miguedrez.

;;     This program is free software: you can redistribute it and/or modify
;;     it under the terms of the GNU General Public License as published by
;;     the Free Software Foundation, either version 2 of the License, or
;;     (at your option) any later version.

;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;     GNU General Public License for more details.

;;     You should have received a copy of the GNU General Public License
;;     along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package #:miguedrez)


;; The project-wide optimization policy is proclaimed globally in packages.lisp
;; (plan §0 rule 4). The hot functions below add a local (declare (optimize
;; (speed 3) (safety 2))) where they need higher safety than the global default.


;;;; Piece-square evaluation tables

(defvar +knight-bishop-pos-weigh+
  (load-time-value
   (make-array 8 :element-type 'fixnum
                 :initial-contents '(10 10 30 60 60 30 10 10))
   t))

(defvar +rook-pos-weigh+
  (load-time-value
   (make-array 8 :element-type 'fixnum
                 :initial-contents '(60 30 30 10 10 30 30 60))
   t))

(defvar +pawn-pos-v-weigh+
  (load-time-value
   (make-array 8 :element-type 'fixnum
                 :initial-contents '(0 0 5 10 30 100 250 900))
   t))

(defvar +pawn-pos-h-weigh+
  (load-time-value
   (make-array 8 :element-type 'fixnum
                 :initial-contents '(2 5 10 20 20 10 5 2))
   t))

(defconstant +mate-score+ 1000000)
(defconstant +quiescence-depth+ 4)
(defconstant +tt-mate-threshold+ 900000)

(defconstant +hash-min-mb+ 1)
(defconstant +hash-max-mb+ 4096)
(defconstant +hash-default-mb+ 64)
(defconstant +tt-entry-bytes-estimate+ 256
  "Conservative TT entry-size estimate used to map Hash MB to table slots.")
(defconstant +tt-cluster-size+ 4)
(defconstant +tt-generation-mask+ #xff)

(defconstant +history-max+ 32767)
(defconstant +history-bonus-base+ 64)
(defconstant +qsearch-see-prune-threshold+ -120)
(defconstant +max-search-ply+ 128)

(defconstant +search-stop-poll-interval+ 1024)
(defconstant +tt-exact-bonus+ 16)
(defconstant +tt-lower-bonus+ 8)
(defconstant +tt-pv-bonus+ 12)

;; Negamax search-shaping constants (wired into the search in later gates).
(defconstant +null-move-reduction+ 3
  "Depth reduction R applied by verified null-move pruning.")
(defconstant +null-move-min-depth+ 4
  "Minimum remaining depth at which a null move may be attempted.")
(defconstant +lmr-min-depth+ 3
  "Minimum remaining depth at which late-move reductions apply.")
(defconstant +lmr-full-depth-moves+ 2
  "Number of leading moves searched at full depth before LMR engages.")
(defconstant +maximum-check-extensions+ 8
  "Cap on the number of one-ply check extensions along a single line.")
(defconstant +check-extension-cap-ratio+ 2
  "Divisor applied to *SEARCH-ROOT-DEPTH* to derive the effective per-line
check-extension cap, so a shallow requested depth is not given the same
absolute extension budget as a deep one.  See +MAXIMUM-CHECK-EXTENSIONS+.")
(defconstant +check-extension-cap-floor+ 4
  "Minimum effective check-extension cap regardless of requested root depth.")

(declaim (type (simple-array fixnum (8))
               +knight-bishop-pos-weigh+
               +rook-pos-weigh+
               +pawn-pos-v-weigh+
               +pawn-pos-h-weigh+))


;;;; Transposition table

(defstruct tt-entry
  "A single transposition-table entry keyed by the Zobrist board key."
  (key 0 :type (unsigned-byte 64))
  (depth 0 :type fixnum)
  (score 0 :type fixnum)
  (flag 'exact :type symbol)
  (move nil :type (or null move))
  (generation 0 :type fixnum)
  (pv nil :type boolean))


(defvar *tt-hash-mb* +hash-default-mb+
  "Configured Hash size in MB from UCI setoption.")

(defvar *tt-mask* 0
  "Current transposition-table index mask (TT-SIZE - 1).")

(defvar *tt-table*
  (let ((cluster (make-array +tt-cluster-size+ :initial-element nil)))
    (make-array 1 :initial-element cluster))
  "Fixed-size clustered transposition table (each slot is one cluster).")

(defvar *tt-cluster-count* 1
  "Number of clusters in the TT (power-of-two).")

(defvar *tt-generation* 0
  "Current TT generation (root-search age marker).")

(deftype history-score () '(signed-byte 32))

(defvar *history*
  (make-array '(2 64 64)
              :element-type 'history-score
              :initial-element 0)
  "Butterfly history heuristic: side x from x to.")

(defvar *killers*
  (make-array (list +max-search-ply+ 2)
              :initial-element nil)
  "Two quiet killer moves per ply.")

(defvar *countermoves*
  (make-array '(64 16)
              :initial-element nil)
  "Countermove table indexed by previous destination and piece class.")

(defvar *search-undo-pool*
  (let ((pool (make-array +max-search-ply+)))
    (dotimes (i +max-search-ply+ pool)
      (setf (aref pool i) (make-move-undo))))
  "One pre-allocated MOVE-UNDO record per ply, reused by PVS-SEARCH/
QUIESCENCE-SEARCH for every real move played during search (as opposed to
*KING-VALID-UNDO*, which pools the same idea for the legality-check path
only). Safe because PLY strictly increases by 1 per recursive call in both
functions (they share one ply counter), so at most one live user of a given
ply's slot exists at any moment -- exactly *KING-VALID-UNDO*'s existing
single-in-flight invariant, just indexed by ply instead of a single shared
slot, since real search nests many plies deep simultaneously. EXECUTE-MOVE
already documents that a reused record is safe: it resets CASTLED/
EP-CAPTURED-POS unconditionally before use, and every slot UNMAKE-MOVE
reads unconditionally (not gated behind CASTLED) is itself written
unconditionally. ROOK-FROM/ROOK-TO/ROOK-PIECE are the one exception to
\"every slot is written unconditionally\" -- they are only set inside the
castling branch -- but UNMAKE-MOVE only ever reads them from inside
`(when (move-undo-castled undo) ...)`, and that same castling branch is
exactly where they are (freshly) set, so a stale value from a pool slot's
prior, unrelated use is never reachable. This pool relies on that
guarantee, so EXECUTE-MOVE/UNMAKE-MOVE need no changes.")

(declaim (type simple-vector *tt-table* *search-undo-pool*))
(declaim (type (simple-array history-score (2 64 64)) *history*))
(declaim (type (simple-array t (* 2)) *killers*))
(declaim (type (simple-array t (64 16)) *countermoves*))

(declaim (inline search-undo-for-ply))
(defun search-undo-for-ply (ply)
  "Return the pooled MOVE-UNDO record for PLY, or a fresh one if PLY runs
past the pool (a rare deep-quiescence edge case the pool wasn't sized for
-- correctness never depends on pooling, only speed does)."
  (declare (type fixnum ply) (optimize (speed 3) (safety 2)))
  (if (< ply +max-search-ply+)
      (aref *search-undo-pool* ply)
      (make-move-undo)))

(defvar *search-null-undo-pool*
  (let ((pool (make-array +max-search-ply+)))
    (dotimes (i +max-search-ply+ pool)
      (setf (aref pool i) (make-null-undo))))
  "Same pooling idea as *SEARCH-UNDO-POOL*, for MAKE-NULL-MOVE's null-undo
records (verified null-move pruning's pass move).")
(declaim (type simple-vector *search-null-undo-pool*))

(declaim (inline search-null-undo-for-ply))
(defun search-null-undo-for-ply (ply)
  (declare (type fixnum ply) (optimize (speed 3) (safety 2)))
  (if (< ply +max-search-ply+)
      (aref *search-null-undo-pool* ply)
      (make-null-undo)))

;; *SEARCH-NODES*/*QSEARCH-NODES*/*SEARCH-SELDEPTH*/*SEARCH-ROOT-DEPTH* (all
;; defined below, in the time-management section) are SETF/INCF'd from the
;; hottest code in the engine -- every PVS-SEARCH/QUIESCENCE-SEARCH node --
;; and *TT-MASK*/*TT-CLUSTER-COUNT*/*TT-GENERATION*/*TT-HASH-MB* above are read
;; on every TT probe/store. SBCL's type inference is documented as weak for
;; SETF-reassigned specials, so declare them explicitly here, before any of them
;; are used, rather than relying on inference or scattered ad hoc (THE FIXNUM ...)
;; assertions at individual call sites.
(declaim (type fixnum *tt-mask* *tt-cluster-count* *tt-generation*
               *tt-hash-mb* *search-nodes* *qsearch-nodes* *search-seldepth*
               *search-root-depth*))

(defun side-index (color)
  (declare (type symbol color) (optimize (speed 3) (safety 2)))
  (if (eq color 'white) 0 1))

(defun piece-class-index (piece)
  "Return a compact class index for PIECE used by countermove table."
  (declare (type symbol piece) (optimize (speed 3) (safety 2)))
  (case piece
    ((pb pn) 1)
    ((cb cn) 2)
    ((ab an) 3)
    ((tb tn) 4)
    ((db dn) 5)
    ((rb rn) 6)
    (t 0)))

(defun same-move-p (a b)
  (declare (type (or null move) a b) (optimize (speed 3) (safety 2)))
  (and a b (equal-moves a b)))

(defun move-quiet-p (board move)
  "True if MOVE is neither a capture nor a promotion."
  (declare (type board board) (type move move) (optimize (speed 3) (safety 2)))
  (and (null (move-promotion move))
       (not (capture-move-p board move))))

(defun history-score (color move)
  (declare (type symbol color) (type move move) (optimize (speed 3) (safety 2)))
  (let ((side (side-index color))
        (from (move-origin-index move))
        (to (move-target-index move)))
    (aref *history* side from to)))

(defun bounded-history-update (color move bonus)
  "Self-correcting bounded history update.

history += bonus - history * abs(bonus) / HISTORY-MAX"
  (declare (type symbol color) (type move move) (type fixnum bonus)
           (optimize (speed 3) (safety 2)))
  (let* ((side (side-index color))
         (from (move-origin-index move))
         (to (move-target-index move))
         (current (aref *history* side from to))
         (abs-bonus (abs bonus))
         (delta (- bonus (truncate (* current abs-bonus) +history-max+)))
         (updated (+ current delta)))
    (setf (aref *history* side from to)
          (cond ((> updated +history-max+) +history-max+)
                ((< updated (- +history-max+)) (- +history-max+))
                (t updated)))))

(defun clear-ordering-heuristics ()
  "Reset ordering tables at the start of a new game/search family."
    (declare (optimize (speed 3) (safety 2)))
(dotimes (side 2)
    (dotimes (from 64)
      (dotimes (to 64)
        (setf (aref *history* side from to) 0))))
  (dotimes (ply +max-search-ply+)
    (setf (aref *killers* ply 0) nil)
    (setf (aref *killers* ply 1) nil))
  (dotimes (to 64)
    (dotimes (piece 16)
      (setf (aref *countermoves* to piece) nil)))
  (values))

(defun push-killer (ply move board)
  "Insert MOVE into killer table for PLY if it is a quiet non-duplicate."
  (declare (type fixnum ply) (type move move) (type board board)
           (optimize (speed 3) (safety 2)))
  (when (and (>= ply 0)
             (< ply +max-search-ply+)
             (move-quiet-p board move))
    (let ((k1 (aref *killers* ply 0))
          (k2 (aref *killers* ply 1)))
      (unless (or (same-move-p move k1)
                  (same-move-p move k2))
        (setf (aref *killers* ply 1) k1)
        (setf (aref *killers* ply 0) move)))))

(defun countermove-slot (board prev-move)
  "Return (VALUES PREV-TO PIECE-IDX) indexing *COUNTERMOVES* for PREV-MOVE, or
NIL if PREV-MOVE's piece has no countermove slot (PIECE-CLASS-INDEX <= 0)."
  (declare (type board board) (type move prev-move)
           (optimize (speed 3) (safety 2)))
  (let* ((prev-to (move-target-index prev-move))
         (piece (board-pos (board-board board) (move-to prev-move)))
         (piece-idx (piece-class-index piece)))
    (when (> piece-idx 0)
      (values prev-to piece-idx))))

(defun store-countermove (board prev-move move)
  "Store MOVE as countermove to PREV-MOVE when both are available/quiet."
  (declare (type board board) (type (or null move) prev-move) (type move move)
           (optimize (speed 3) (safety 2)))
  (when (and prev-move
             (move-quiet-p board move))
    (multiple-value-bind (prev-to piece-idx) (countermove-slot board prev-move)
      (when prev-to
        (setf (aref *countermoves* prev-to piece-idx) move)))))

(defun countermove-for (board prev-move)
  (declare (type board board) (type (or null move) prev-move)
           (optimize (speed 3) (safety 2)))
  (when prev-move
    (multiple-value-bind (prev-to piece-idx) (countermove-slot board prev-move)
      (when prev-to
        (aref *countermoves* prev-to piece-idx)))))

(defun history-bonus-for-depth (depth)
  (declare (type fixnum depth) (optimize (speed 3) (safety 2)))
  (the fixnum (min +history-max+ (+ +history-bonus-base+ (* depth depth)))))

(defun score-to-tt (score ply)
  "Normalize mate scores before storing to TT (ply-relative)."
  (declare (type fixnum score ply) (optimize (speed 3) (safety 2)))
  (cond ((>= score +tt-mate-threshold+) (+ score ply))
        ((<= score (- +tt-mate-threshold+)) (- score ply))
        (t score)))

(defun score-from-tt (score ply)
  "Restore normalized mate scores when probing TT."
  (declare (type fixnum score ply) (optimize (speed 3) (safety 2)))
  (cond ((>= score +tt-mate-threshold+) (- score ply))
        ((<= score (- +tt-mate-threshold+)) (+ score ply))
        (t score)))

(defun tt-advance-generation ()
  (declare (optimize (speed 3) (safety 2)))
  (setf *tt-generation*
        (logand (1+ *tt-generation*) +tt-generation-mask+))
  *tt-generation*)

(defun tt-entry-age (entry)
  (declare (type tt-entry entry) (optimize (speed 3) (safety 2)))
  (logand (- *tt-generation* (tt-entry-generation entry)) +tt-generation-mask+))

(defun tt-entry-replacement-score (entry)
  "Higher is better (prefer retaining deeper/newer/exact/pv entries)."
  (declare (type tt-entry entry) (optimize (speed 3) (safety 2)))
  (+ (* 32 (tt-entry-depth entry))
     (case (tt-entry-flag entry)
       (exact +tt-exact-bonus+)
       (lower +tt-lower-bonus+)
       (t 0))
     (if (tt-entry-pv entry) +tt-pv-bonus+ 0)
     (- (tt-entry-age entry))))

(defun capture-move-p (board move)
  (declare (type board board) (type move move) (optimize (speed 3) (safety 2)))
  (let* ((to-piece (board-pos (board-board board) (move-to move)))
         (color (board-side-to-move board)))
    (or (and (not (eq to-piece 'vv))
             (not (eq (piece-color to-piece) color)))
        (en-passant-move-p board move))))

(defun see-sign (board move)
  "Fast SEE approximation value in centipawns.

Positive means the capture is likely favorable; negative means likely unfavorable.
The current-architecture version avoids board copies and list allocation."
  (declare (type board board) (type move move) (optimize (speed 3) (safety 2)))
  (if (not (capture-move-p board move))
      0
      (let* ((from-piece (board-pos (board-board board) (move-from move)))
             (to-piece (board-pos (board-board board) (move-to move)))
             (attacker (piece-value from-piece))
             (victim (if (eq to-piece 'vv)
                         (piece-value 'pb)
                         (piece-value to-piece))))
        (declare (type fixnum attacker victim))
        (the fixnum (- victim attacker)))))

(defun move-in-list-p (move moves)
  (declare (type (or null move) move) (type list moves) (optimize (speed 3) (safety 2)))
  (and move (member move moves :test #'equal-moves)))

(defun remove-move-once (move moves)
  (declare (type (or null move) move) (type list moves) (optimize (speed 3) (safety 2)))
  (if (null move)
      moves
      (remove move moves :count 1 :test #'equal-moves)))

(defun sort-by-score (moves board)
  "Sort MOVES highest SCORE-MOVE first."
  (declare (type list moves) (type board board) (optimize (speed 3) (safety 2)))
  (sort moves #'> :key #'(lambda (mv) (score-move board mv))))

(defun classify-moves (board color legal tt-move prev-move ply)
  "Split LEGAL moves into ordered search stages.

Returns values:
  (tt legal-captures bad-captures killers countermove quiets)"
  (declare (type board board) (type symbol color) (type list legal)
           (type (or null move) tt-move prev-move)
           (type fixnum ply)
           (optimize (speed 3) (safety 2)))
  (let* ((tt-legal (and tt-move (move-in-list-p tt-move legal) tt-move))
         (remaining (remove-move-once tt-legal legal))
         (good-captures '())
         (bad-captures '())
         (quiets '())
         (killer1 (if (and (>= ply 0) (< ply +max-search-ply+))
                      (aref *killers* ply 0)
                      nil))
         (killer2 (if (and (>= ply 0) (< ply +max-search-ply+))
                      (aref *killers* ply 1)
                      nil))
         ;; Legal (already required) AND genuinely quiet in the CURRENT
         ;; position AND not already TT-LEGAL. The countermove heuristic is
         ;; a quiet-move ordering boost; STORE-COUNTERMOVE only requires the
         ;; move to have been quiet at the position it was recorded from,
         ;; not at this (transposed/later) one, so re-checking MOVE-QUIET-P
         ;; here is what keeps a since-turned-into-a-capture entry out of
         ;; its own preferential search slot. Without this, a countermove
         ;; that now coincides with TT-LEGAL or lands in good/bad-captures
         ;; below would be searched a second time via the explicit COUNTER
         ;; stage in RUN-SWEEP -- wasted work, and it inflates MOVE-COUNT
         ;; for every move ordered after it, distorting LMR's reduction
         ;; amount for the rest of the sweep.
         (counter (let ((c (countermove-for board prev-move)))
                    (and c (move-in-list-p c legal)
                         (not (same-move-p c tt-legal))
                         (move-quiet-p board c)
                         c)))
         (killer-list '()))
    (declare (type list remaining good-captures bad-captures quiets killer-list))
    (dolist (mv remaining)
      (cond ((or (move-promotion mv) (capture-move-p board mv))
             (if (plusp (see-sign board mv))
                 (push mv good-captures)
                 (push mv bad-captures)))
            (t
             (push mv quiets))))
    (when (move-in-list-p killer1 quiets)
      (push killer1 killer-list)
      (setf quiets (remove-move-once killer1 quiets)))
    (when (and (move-in-list-p killer2 quiets)
               (not (same-move-p killer2 killer1)))
      (push killer2 killer-list)
      (setf quiets (remove-move-once killer2 quiets)))
    ;; Extract the countermove from the quiet list so it is searched in its own
    ;; stage rather than again among the quiets. COUNTER is already gated above
    ;; by MOVE-IN-LIST-P against LEGAL (the same guard TT-LEGAL uses), so a
    ;; stale countermove-table entry from an unrelated branch of the search
    ;; (the table is only cleared at CLEAR-ORDERING-HEURISTICS boundaries, not
    ;; per-node) can never reach SEARCH-ONE/EXECUTE-MOVE here -- MOVE-QUIET-P
    ;; alone (checked only the destination square) was not a sufficient guard,
    ;; since a stale move can have an innocuous-looking destination while
    ;; still not being a legal move in the current position.
    (when (and (move-in-list-p counter quiets)
               (not (move-in-list-p counter killer-list)))
      (setf quiets (remove-move-once counter quiets)))
    (values tt-legal
            (sort-by-score good-captures board)
            (sort-by-score bad-captures board)
            (nreverse killer-list)
            counter
            (sort quiets #'> :key #'(lambda (mv) (history-score color mv))))))

(defun next-qsearch-moves (board legal)
  "Return ordered tactical moves for quiescence: good first, bad last."
  (declare (type board board) (type list legal)
           (optimize (speed 3) (safety 2)))
  (let ((good '()) (bad '()))
    (dolist (mv legal)
      (when (or (move-promotion mv) (capture-move-p board mv))
        (if (plusp (see-sign board mv))
            (push mv good)
            (push mv bad))))
    (nconc (sort-by-score good board) (sort-by-score bad board))))

(defun update-quiet-heuristics (board color move depth ply prev-move)
  (declare (type board board) (type symbol color) (type move move)
           (type fixnum depth ply) (type (or null move) prev-move)
           (optimize (speed 3) (safety 2)))
  (when (move-quiet-p board move)
    (let ((bonus (history-bonus-for-depth depth)))
      (declare (type fixnum bonus))
      (bounded-history-update color move bonus)
      (push-killer ply move board)
      (store-countermove board prev-move move))))

(defun tt-cluster-index (key)
  "Map KEY to a cluster index."
  (declare (type (unsigned-byte 64) key) (optimize (speed 3) (safety 2)))
  (the fixnum (logand key (the fixnum *tt-mask*))))

(defun tt-cluster-probe (cluster key)
  "Return matching entry from CLUSTER for KEY, else NIL."
  (declare (type simple-vector cluster) (type (unsigned-byte 64) key)
           (optimize (speed 3) (safety 2)))
  (dotimes (i +tt-cluster-size+ nil)
    (let ((entry (aref cluster i)))
      (when (and entry
                 (= (the (unsigned-byte 64) (tt-entry-key entry)) key))
        (return entry)))))

(defun tt-cluster-replacement-index (cluster)
  "Pick best replacement slot in CLUSTER.

Prefer empty slot, else lowest replacement score."
  (declare (type simple-vector cluster) (optimize (speed 3) (safety 2)))
  (let ((best-idx 0)
        (best-score most-positive-fixnum))
    (declare (type fixnum best-idx best-score))
    (dotimes (i +tt-cluster-size+ best-idx)
      (let ((entry (aref cluster i)))
        (if (null entry)
            (return i)
            (let ((score (tt-entry-replacement-score entry)))
              (when (< score best-score)
                (setf best-score score)
                (setf best-idx i))))))))

(defun tt-score-usable-p (score flag alpha beta)
  (declare (type fixnum score alpha beta) (type symbol flag)
           (optimize (speed 3) (safety 2)))
  (or (eq flag 'exact)
      (and (eq flag 'lower) (>= score beta))
      (and (eq flag 'upper) (<= score alpha))))

(defun tt-touch-generation (entry)
  (declare (type tt-entry entry) (optimize (speed 3) (safety 2)))
  (setf (tt-entry-generation entry) *tt-generation*)
  entry)

(defun normalize-hash-mb (hash-mb)
  "Clamp HASH-MB to the supported UCI range [1, 4096]."
  (declare (optimize (speed 3) (safety 2)))
  (let ((value (if (numberp hash-mb)
                   (truncate hash-mb)
                   +hash-default-mb+)))
    (declare (type integer value))
    (cond ((< value +hash-min-mb+) +hash-min-mb+)
          ((> value +hash-max-mb+) +hash-max-mb+)
          (t value))))

(defun hash-mb-to-tt-size (hash-mb)
  "Return a power-of-two TT size derived from HASH-MB."
  (declare (type integer hash-mb) (optimize (speed 3) (safety 2)))
  (let* ((bytes (* hash-mb 1024 1024))
         (target (max 1 (floor bytes +tt-entry-bytes-estimate+)))
         (pow2 (ash 1 (integer-length (1- target)))))
    (declare (type integer bytes target pow2))
    (the fixnum (max 1 pow2))))

(defun make-tt-cluster ()
    (declare (optimize (speed 3) (safety 2)))
(make-array +tt-cluster-size+ :initial-element nil))

(defun tt-resize (hash-mb)
  "Resize the clustered transposition table according to HASH-MB and clear it.

Returns the applied hash size in MB."
    (declare (optimize (speed 3) (safety 2)))
(let* ((normalized (normalize-hash-mb hash-mb))
         (entry-count (hash-mb-to-tt-size normalized))
         (cluster-count (max 1 (truncate entry-count +tt-cluster-size+)))
         (pow2-clusters (ash 1 (integer-length (1- cluster-count))))
         (new-mask (1- pow2-clusters))
         (new-table (make-array pow2-clusters :initial-element nil)))
    (dotimes (i pow2-clusters)
      (setf (aref new-table i) (make-tt-cluster)))
    (setf *tt-hash-mb* normalized)
    (setf *tt-cluster-count* pow2-clusters)
    (setf *tt-mask* new-mask)
    (setf *tt-table* new-table)
    (setf *tt-generation* 0)
    normalized))

(defun tt-current-hash-mb ()
  "Return the currently configured hash size in MB."
    (declare (optimize (speed 3) (safety 2)))
*tt-hash-mb*)



(defun tt-ensure-sized ()
  "Allocate the configured transposition table lazily before search use."
  (declare (optimize (speed 3) (safety 2)))
  (when (= *tt-cluster-count* 1)
    (tt-resize *tt-hash-mb*))
  (values))

(defun tt-clear ()
  "Clear the transposition table before a new root search."
    (declare (optimize (speed 3) (safety 2)))
(tt-ensure-sized)
  (dotimes (i *tt-cluster-count*)
    (fill (aref *tt-table* i) nil))
  (values))

(defun tt-hashfull ()
  "Return how full the transposition table is, in per-mille (0-1000) -- the
UCI `info hashfull` convention GUIs like Arena display. Samples up to 1000
clusters and counts slots occupied by an entry from the CURRENT search
generation: an entry tagged with a stale generation is effectively free
space, since TT-CLUSTER-REPLACEMENT-INDEX will overwrite it before an
empty slot elsewhere in the same cluster."
  (declare (optimize (speed 3) (safety 2)))
  (let* ((sample-clusters (min *tt-cluster-count* 1000))
         (sampled (* sample-clusters +tt-cluster-size+))
         (used 0))
    (declare (type fixnum sample-clusters sampled used))
    (dotimes (i sample-clusters)
      (let ((cluster (aref *tt-table* i)))
        (dotimes (j +tt-cluster-size+)
          (let ((entry (aref cluster j)))
            (when (and entry (= (the fixnum (tt-entry-generation entry))
                                 *tt-generation*))
              (incf used))))))
    (if (zerop sampled) 0 (the fixnum (floor (* used 1000) sampled)))))

(defun reset-search-state ()
  "Clear the TT and the move-ordering heuristics together. The two must
always be reset as a pair at a new-game/search-family boundary -- see
documentation/G14 for the bug where UCI's ucinewgame handler cleared only
the TT, leaving heuristics to leak across unrelated positions."
    (declare (optimize (speed 3) (safety 2)))
(tt-clear)
  (clear-ordering-heuristics)
  (values))

;;; Transposition-table hit/miss statistics for the current root search.
;;; Incremented with a generic (bignum-safe) INCF in the same style as
;;; *SEARCH-NODES* -- the overhead is negligible relative to the probe work and
;;; the counters can never overflow a fixnum silently on a long search.  Reset
;;; at the start of every root search (CHOOSE-MOVE / CHOOSE-MOVE-ITERATIVE).
;;;   probes    = total TT probes
;;;   hits      = probes that returned a usable score (cutoff/value)
;;;   move-hits = probes that found an entry but no usable score, yet still
;;;               yielded a hash move for ordering
;;;   misses    = probes that found no entry at all
;;; (hits + move-hits + misses) = probes, by construction.
(defvar *tt-probes* 0
  "Total TT probes issued in the current root search.")

(defvar *tt-hits* 0
  "TT probes that returned a usable score in the current root search.")

(defvar *tt-move-hits* 0
  "TT probes that found an entry (hash move) but no usable score.")

(defvar *tt-misses* 0
  "TT probes that found no matching entry in the current root search.")

(defun reset-tt-statistics ()
  "Zero the TT hit/miss counters at the start of a root search."
    (declare (optimize (speed 3) (safety 2)))
(setf *tt-probes* 0)
  (setf *tt-hits* 0)
  (setf *tt-move-hits* 0)
  (setf *tt-misses* 0)
  (values))

(defun tt-statistics ()
  "Return the current TT statistics as (VALUES probes hits move-hits misses
hit-rate), where HIT-RATE is the usable-score fraction in [0,1]."
    (declare (optimize (speed 3) (safety 2)))
(values *tt-probes* *tt-hits* *tt-move-hits* *tt-misses*
          (if (zerop *tt-probes*)
              0.0
              (/ (float *tt-hits*) (float *tt-probes*)))))

(declaim (ftype (function (board fixnum fixnum fixnum fixnum)
                          (values (member t nil) fixnum (or null move)))
                tt-probe))
(defun tt-probe (board depth alpha beta ply)
  "Probe TT cluster for BOARD at DEPTH and window [ALPHA,BETA].
Return (HIT SCORE MOVE), applying mate-score de-normalization by PLY.

A stored move is returned even when its score cannot be used for the current
window/depth, so the root search can still order that legal hash move first."
  (declare (type board board) (type fixnum depth alpha beta ply)
           (optimize (speed 3) (safety 2)))
  (incf *tt-probes*)
  (let* ((key (board-key board))
         (cluster (aref *tt-table* (tt-cluster-index key)))
         (entry (tt-cluster-probe cluster key)))
    (when entry
      (tt-touch-generation entry)
      (let ((move (tt-entry-move entry)))
        (when (>= (tt-entry-depth entry) depth)
          (let ((score (score-from-tt (tt-entry-score entry) ply))
                (flag (tt-entry-flag entry)))
            (declare (type fixnum score))
            (when (tt-score-usable-p score flag alpha beta)
              (incf *tt-hits*)
              (return-from tt-probe (values t score move)))))
        ;; Entry present but its score is unusable for this window/depth; it
        ;; still supplies a hash move for ordering.
        (incf *tt-move-hits*)
        (return-from tt-probe (values nil 0 move))))
    (incf *tt-misses*)
    (values nil 0 nil)))

(declaim (ftype (function (board fixnum fixnum symbol (or null move) fixnum boolean) (values)) tt-store))
(defun tt-store (board depth score flag move ply pv-node)
  "Store a search result for BOARD in its TT cluster.

Scores are normalized by PLY before storage. Replacement is based on
cluster policy (empty, key match, else weakest replacement score)."
  (declare (type board board) (type fixnum depth score ply)
           (type symbol flag) (type (or null move) move) (type boolean pv-node)
           (optimize (speed 3) (safety 2)))
  (let* ((key (board-key board))
         (cluster (aref *tt-table* (tt-cluster-index key)))
         (entry (tt-cluster-probe cluster key))
         (stored-score (score-to-tt score ply)))
    (declare (type fixnum stored-score))
    (if entry
        ;; Same key: replace when the new result is at least as useful.
        ;;
        ;; Exact-demotion guard: at EQUAL depth an EXACT entry is strictly
        ;; stronger than a LOWER/UPPER bound (it cuts for any window the bound
        ;; would cut for, plus the windows in between), so a non-exact bound
        ;; must not overwrite an existing exact entry -- that would lose
        ;; cutoffs for windows the exact score still served.  A deeper bound
        ;; always replaces a shallower one (it is usable at more probe depths).
        ;;
        ;; Hash-move preservation: a null-move cutoff stores MOVE = NIL; a real
        ;; hash move stored by an earlier iteration at this key is strictly more
        ;; useful for ordering than NIL, so keep the existing move when the new
        ;; one is NIL instead of clobbering it.
        (when (and (<= (tt-entry-depth entry) depth)
                   (or (eq flag 'exact)
                       (not (eq (tt-entry-flag entry) 'exact))
                       (> depth (tt-entry-depth entry))))
          (setf (tt-entry-depth entry) depth)
          (setf (tt-entry-score entry) stored-score)
          (setf (tt-entry-flag entry) flag)
          (setf (tt-entry-move entry) (or move (tt-entry-move entry)))
          (setf (tt-entry-pv entry) pv-node)
          (setf (tt-entry-generation entry) *tt-generation*))
        ;; New key: use empty slot or weakest entry.
        (let* ((slot (tt-cluster-replacement-index cluster))
               (new-entry (make-tt-entry :key key
                                         :depth depth
                                         :score stored-score
                                         :flag flag
                                         :move move
                                         :generation *tt-generation*
                                         :pv pv-node)))
          (declare (type fixnum slot))
          (setf (aref cluster slot) new-entry))))
  (values))

(defun tt-best-move (board)
  "Return TT move for BOARD from its cluster if present."
  (declare (type board board) (optimize (speed 3) (safety 2)))
  (let* ((key (board-key board))
         (cluster (aref *tt-table* (tt-cluster-index key)))
         (entry (tt-cluster-probe cluster key)))
    (when entry
      (tt-touch-generation entry)
      (tt-entry-move entry))))


;;;; Search control

(defvar *uci-search-deadline* nil
  "Absolute deadline in internal-time units, or NIL for no deadline.")

(defvar *uci-search-stop-requested* nil
  "Set by the UCI stop command to request an immediate search abort.")

(defvar *search-nodes* 0
  "Nodes visited in the current root search (main search plus quiescence).")

(defvar *qsearch-nodes* 0
  "Quiescence-search nodes visited in the current root search.

A strict subset of *SEARCH-NODES*: every quiescence node increments both, so
the main-search node count is (- *SEARCH-NODES* *QSEARCH-NODES*).  Reset with
*SEARCH-NODES* at the start of every root search; exposed so diagnostics can
attribute the node budget between the main tree and the quiescence tails.")

(defvar *search-seldepth* 0
  "Maximum reached ply in the current root search.")

(defvar *search-aborted* nil
  "True when the current root search was aborted by time or stop request.")

(defvar *search-root-depth* 1
  "Nominal iterative-deepening depth requested for the current root search.

Set at the start of every root search (CHOOSE-MOVE / CHOOSE-MOVE-ITERATIVE).
CHECK-EXTENSION scales its effective extension-count cap from this value so
that a shallow requested depth is not given the same absolute check-extension
budget as a deep one -- see +MAXIMUM-CHECK-EXTENSIONS+.")

(defun periodic-time-check-p ()
  (declare (optimize (speed 3) (safety 2)))
  (zerop (logand *search-nodes* (1- +search-stop-poll-interval+))))

(defun search-time-up-p ()
  "Return true if the search should stop now."
    (declare (optimize (speed 3) (safety 2)))
(or *uci-search-stop-requested*
      (and *uci-search-deadline*
           (>= (get-internal-real-time) *uci-search-deadline*))))

(defun search-elapsed-ms (start-time)
  "Return elapsed search time in milliseconds from START-TIME."
  (declare (type integer start-time) (optimize (speed 3) (safety 2)))
  (let ((ticks (- (get-internal-real-time) start-time)))
    (declare (type integer ticks))
    (max 1 (truncate (* 1000 (/ ticks internal-time-units-per-second))))))

(defun node-rate-nps (nodes elapsed-ms)
  "Return nodes-per-second computed from NODES and ELAPSED-MS."
  (declare (type integer nodes elapsed-ms) (optimize (speed 3) (safety 2)))
  (if (<= elapsed-ms 0)
      0
      (truncate (* nodes (/ 1000.0 elapsed-ms)))))


;;;; Material values

(defconstant +pawn-value+ 100)
(defconstant +knight-value+ 300)
(defconstant +bishop-value+ 300)
(defconstant +rook-value+ 500)
(defconstant +queen-value+ 900)
(defconstant +king-value+ 100000)


;;;; Public search API

(defun reset-search-counters ()
  "Zero the per-search node/depth counters and TT statistics shared by
CHOOSE-MOVE and CHOOSE-MOVE-ITERATIVE at the start of a new search."
    (declare (optimize (speed 3) (safety 2)))
(setf *search-nodes* 0)
  (setf *qsearch-nodes* 0)
  (setf *search-seldepth* 0)
  (setf *search-aborted* nil)
  (reset-tt-statistics)
  (values))

(defun choose-move (board color &optional (depth 3))
  "Return a legal move for COLOR on BOARD, or NIL if no legal move exists.

This is the non-UCI helper used by the console mode."
  ;; RESET-SEARCH-STATE (not a bare TT-CLEAR): the console game loop
  ;; (CLI-AJZ) calls this once per engine move across a whole game, so
  ;; leaving killer/history/countermove data from earlier, unrelated
  ;; positions unreset would reintroduce the exact cross-position leak
  ;; documentation/G14 fixed for the UCI ucinewgame path.
    (declare (optimize (speed 3) (safety 2)))
(reset-search-state)
  (reset-search-counters)
  (setf *search-root-depth* depth)
  ;; Also reset the UCI-facing time-control state: this function never sets
  ;; either, but nothing guarantees they are still NIL if a prior UCI search
  ;; ran earlier in the same process image (see the failure scenario noted
  ;; where a stale deadline/stop-request would abort the very first node).
  (setf *uci-search-deadline* nil)
  (setf *uci-search-stop-requested* nil)
  (multiple-value-bind (best-score best-move)
      (pvs-search board depth (the fixnum (- +mate-score+)) +mate-score+ 0)
    (declare (ignore best-score)
             (type (or null move) best-move))
    (or best-move (first (legal-moves board color)))))

(defun pv-line-from-tt-on-board (line-board color depth)
  "Reconstruct a principal variation on LINE-BOARD, mutating that scratch board."
  (declare (type board line-board) (type symbol color) (type fixnum depth)
           (optimize (speed 3) (safety 2)))
  (let ((side color)
        (pv '()))
    (declare (type list pv))
    (dotimes (ply depth (nreverse pv))
      (declare (ignorable ply))
      (let* ((tt-move (tt-best-move line-board))
             (legal (and tt-move
                         (member tt-move (legal-moves line-board side)
                                 :test #'equal-moves))))
        (unless legal
          (return (nreverse pv)))
        (push (move-to-uci tt-move) pv)
        (execute-move line-board tt-move)
        (setf side (board-side-to-move line-board))))))

(defun pv-line-from-tt (board color depth)
  "Reconstruct a principal variation line from TT moves.

Returns a list of UCI moves."
  (declare (type board board) (type symbol color) (type fixnum depth)
           (optimize (speed 3) (safety 2)))
  (pv-line-from-tt-on-board (duplicate-board board) color depth))

(defun pvs-search-root-moves (board moves depth alpha beta)
  "Search only MOVES at the root by playing each and calling unrestricted PVS."
  (declare (type board board) (type list moves) (type fixnum depth alpha beta)
           (optimize (speed 3) (safety 2)))
  (let ((best-move nil)
        (best-score (the fixnum (- +mate-score+))))
    (declare (type (or null move) best-move) (type fixnum best-score))
    (dolist (mv moves)
      (multiple-value-bind (board-after undo)
          (execute-move board mv (search-undo-for-ply 0))
        (declare (ignore board-after))
        (unwind-protect
             (let ((score (the fixnum
                               (- (the fixnum
                                       (pvs-search board (the fixnum (1- depth))
                                                   (- beta) (- alpha) 1
                                                   :pv-node-p t
                                                   :prev-move mv))))))
               (declare (type fixnum score))
               (unless *search-aborted*
                 (when (> score best-score)
                   (setf best-score score)
                   (setf best-move mv))
                 (when (> best-score alpha)
                   (setf alpha best-score))))
          (unmake-move board mv undo))
        (when *search-aborted*
          (return-from pvs-search-root-moves (values 0 nil)))
        (when (>= alpha beta)
          (return))))
    (values best-score best-move)))

(defun choose-move-iterative (board color max-depth &optional info-callback root-moves
                                                        (reset-uci-controls-p t))
  "Iterative-deepening root search up to MAX-DEPTH.

When INFO-CALLBACK is non-NIL, it is called as:
  (funcall INFO-CALLBACK depth seldepth score nodes elapsed-ms pv-list)
for each fully completed iteration.  ROOT-MOVES, when supplied, restricts the
root search to that legal move list (UCI `go searchmoves`).  RESET-UCI-CONTROLS-P
is true for direct callers so stale UCI stop/deadline state cannot leak in; the
UCI timed-search path passes NIL after installing its own deadline."
  (declare (type board board) (type symbol color) (type fixnum max-depth)
           (type boolean reset-uci-controls-p)
           (optimize (speed 3) (safety 2)))
  (when reset-uci-controls-p
    (setf *uci-search-deadline* nil)
    (setf *uci-search-stop-requested* nil))
  (let ((best-move nil)
        (best-score 0)
        (start-time (get-internal-real-time)))
    (declare (type (or null move) best-move) (type fixnum best-score)
             (type integer start-time))
    (tt-advance-generation)
    (reset-search-counters)
    (loop for depth fixnum from 1 to max-depth do
      (let ((score 0)
            (move nil))
        (declare (type fixnum score) (type (or null move) move))
        (setf *search-root-depth* depth)
        (multiple-value-setq (score move)
          (if root-moves
              (pvs-search-root-moves board root-moves depth
                                     (the fixnum (- +mate-score+)) +mate-score+)
              (pvs-search board depth (the fixnum (- +mate-score+)) +mate-score+ 0
                          :pv-node-p t)))
        (when *search-aborted*
          (return))
        (when move
          (setf best-move move)
          (setf best-score score))
        (when info-callback
          (let ((elapsed-ms (search-elapsed-ms start-time)))
            (funcall info-callback
                     depth
                     (max depth *search-seldepth*)
                     best-score
                     *search-nodes*
                     elapsed-ms
                     (if root-moves
                         (and best-move (cons (move-to-uci best-move)
                                              (let ((line-board (duplicate-board board)))
                                                (execute-move line-board best-move)
                                                (pv-line-from-tt-on-board
                                                 line-board
                                                 (board-side-to-move line-board)
                                                 (the fixnum (1- depth))))))
                         (pv-line-from-tt board color depth)))))))
    (values best-score (or best-move (first (or root-moves (legal-moves board color)))))))


;;;; Check detection for move ordering

(defun gives-check-p (board move)
  "Return true if MOVE gives a direct check to the opponent king.

Discovered checks are ignored: this is a fast, ordering-only approximation."
  (declare (type board board) (type move move) (optimize (speed 3) (safety 2)))
  (let* ((color (board-side-to-move board))
         (king-pos (if (eq color 'white)
                       (board-black-king-pos board)
                       (board-white-king-pos board)))
         (piece (board-pos (board-board board) (move-from move)))
         (to (move-to move))
         (king-row (pos-row king-pos))
         (king-col (pos-col king-pos))
         (to-row (pos-row to))
         (to-col (pos-col to)))
    (declare (type fixnum king-row king-col to-row to-col))
    (piece-gives-check-p board piece color to-row to-col king-row king-col
                         (pos-row (move-from move)) (pos-col (move-from move)))))

(defun clear-between-except-p (board row1 col1 row2 col2 except-row except-col)
  "True if the ray from (ROW1,COL1) to (ROW2,COL2) is clear, treating
(EXCEPT-ROW,EXCEPT-COL) as empty."
  (declare (type board board)
           (type fixnum row1 col1 row2 col2 except-row except-col)
           (optimize (speed 3) (safety 2)))
  (let ((dr (signum (- row2 row1)))
        (dc (signum (- col2 col1))))
    (declare (type fixnum dr dc))
    (do ((r (+ row1 dr) (+ r dr))
         (c (+ col1 dc) (+ c dc)))
        ((and (= r row2) (= c col2)) t)
      (declare (type fixnum r c))
      (unless (and (= r except-row) (= c except-col))
        (when (not (eq (aref (board-board board) r c) 'vv))
          (return nil))))))

(defun piece-gives-check-p (board piece color to-row to-col king-row king-col
                            from-row from-col)
  "True if PIECE placed at (TO-ROW,TO-COL) attacks the enemy king square.
(FROM-ROW,FROM-COL) is the piece's original square and is treated as empty."
  (declare (type board board)
           (type symbol piece color)
           (type fixnum to-row to-col king-row king-col from-row from-col)
           (optimize (speed 3) (safety 2)))
  (cond ((pawnp piece)
         (let ((dir (pawn-attack-direction color)))
           (declare (type fixnum dir))
           (and (= (- king-row to-row) dir)
                (= (abs (- king-col to-col)) 1))))
        ((or (eq piece 'cb) (eq piece 'cn))
         (let ((dr (abs (- king-row to-row)))
               (dc (abs (- king-col to-col))))
           (and (> dr 0)
                (> dc 0)
                (= (+ dr dc) 3))))
        ((or (eq piece 'ab) (eq piece 'an))
         (and (= (abs (- king-row to-row)) (abs (- king-col to-col)))
              (clear-between-except-p board to-row to-col king-row king-col
                                      from-row from-col)))
        ((or (eq piece 'tb) (eq piece 'tn))
         (and (or (= king-row to-row) (= king-col to-col))
              (clear-between-except-p board to-row to-col king-row king-col
                                      from-row from-col)))
        ((or (eq piece 'db) (eq piece 'dn))
         (and (or (= king-row to-row)
                  (= king-col to-col)
                  (= (abs (- king-row to-row)) (abs (- king-col to-col))))
              (clear-between-except-p board to-row to-col king-row king-col
                                      from-row from-col)))
        (t nil)))


;;;; Move ordering

(declaim (ftype (function (symbol) fixnum) piece-value))
(declaim (ftype (function (board move) fixnum) score-move))

(defun piece-value (piece)
  "Return the material value of PIECE, or 0 for an empty square."
  (declare (type symbol piece) (optimize (speed 3) (safety 2)))
  (the fixnum
       (case piece
         ((pb pn) +pawn-value+)
         ((cb cn) +knight-value+)
         ((ab an) +bishop-value+)
         ((tb tn) +rook-value+)
         ((db dn) +queen-value+)
         ((rb rn) +king-value+)
         (t 0))))

(defun score-move (board move)
  "Return an integer score for MOVE on BOARD; higher is better for side to move.

Captures use MVV-LVA, promotions receive piece bonuses and quiet checks get a
small bonus."
  (declare (type board board) (type move move) (optimize (speed 3) (safety 2)))
  (let* ((from-piece (board-pos (board-board board) (move-from move)))
         (to-piece (board-pos (board-board board) (move-to move)))
         (capturep (capture-move-p board move))
         (promotion (move-promotion move))
         (quietp (and (not capturep) (not promotion))))
    (the fixnum
         (+ (the fixnum
                (if capturep
                    (let ((victim (if (eq to-piece 'vv) 'pb to-piece)))
                      (- (the fixnum (* 10 (piece-value victim)))
                         (piece-value from-piece)))
                    0))
            (the fixnum
                (case promotion
                  ((q) (+ +queen-value+ 100))
                  ((r) (+ +rook-value+ 100))
                  ((b) (+ +bishop-value+ 100))
                  ((n) (+ +knight-value+ 100))
                  (t 0)))
            (the fixnum
                (if (and quietp (gives-check-p board move))
                    50
                    0))))))


;;;; Negamax search primitives (leaf evaluation, terminal scores, pruning
;;;; helpers).  These feed the negamax PVS and quiescence search below.

(declaim (inline static-eval-stm mated-score))

(defun static-eval-stm (board)
  "Static evaluation from the perspective of the side to move (negamax).
FEV is white-relative, so it is negated for a black mover.  This is the sole
leaf negation point of the negamax search."
  (declare (type board board) (optimize (speed 3) (safety 2)))
  (let ((score (the fixnum (fev board))))
    (if (eq (board-side-to-move board) 'white)
        score
        (the fixnum (- score)))))

(defun mated-score (ply)
  "Negamax mate score for the side to move being checkmated at PLY.  Always
negative -- the side to move is the mated side -- and closer to zero the
deeper the mate, so the search prefers the fastest mate."
  (declare (type fixnum ply) (optimize (speed 3) (safety 2)))
  (the fixnum (- ply +mate-score+)))

(defun terminal-no-move-score (board ply)
  "Score for a node with no legal moves: a negamax mate score when the side to
move is in check (checkmate), otherwise 0 (stalemate)."
  (declare (type board board) (type fixnum ply) (optimize (speed 3) (safety 2)))
  (if (checkp board (board-side-to-move board))
      (mated-score ply)
      0))

(defun side-has-non-pawn-material-p (board color)
  "Return true if COLOR has at least one knight, bishop, rook or queen on
BOARD.  Zugzwang guard for null-move pruning: a side with only pawns and its
king may be in zugzwang, where passing (a null move) is illegitimately good."
  (declare (type board board) (type symbol color) (optimize (speed 3) (safety 2)))
  (let ((knight (if (eq color 'white) 'cb 'cn))
        (bishop (if (eq color 'white) 'ab 'an))
        (rook   (if (eq color 'white) 'tb 'tn))
        (queen  (if (eq color 'white) 'db 'dn)))
    (dotimes (i 8)
      (dotimes (j 8)
        (let ((piece (aref (board-board board) i j)))
          (when (or (eq piece knight) (eq piece bishop)
                    (eq piece rook) (eq piece queen))
            (return-from side-has-non-pawn-material-p t)))))
    nil))

(defun may-try-null-move-p (allow-null pv-node-p depth in-check board color verify)
  "Eligibility test for a null-move reduction at the current node.

A null move is attempted only when null moves are still permitted (ALLOW-NULL is
nil inside a null child and its verification subtree), on a non-PV node, at
sufficient remaining DEPTH, when the side to move is not in check, and when that
side has non-pawn material (the zugzwang guard: a king-and-pawns side may be in
zugzwang, where passing is illegitimately good).  The final clause keeps the
degenerate depth-1 node out of the verified re-search, where reducing the depth
by one would leave nothing to search."
  (declare (type boolean allow-null pv-node-p in-check verify)
           (type fixnum depth) (type symbol color) (type board board)
           (optimize (speed 3) (safety 2)))
  (and allow-null
       (not pv-node-p)
       (>= depth +null-move-min-depth+)
       (not in-check)
       (side-has-non-pawn-material-p board color)
       (or (not verify) (> depth 1))))

(defvar *lmr-enabled* t
  "Master switch for late-move reductions, analogous to the null-move VERIFY
flag.  Bound to T in normal play; a diagnostic may bind it to NIL to search the
same binary with LMR disabled (a clean A/B isolation of the feature's node
effect).  Toggling it never affects correctness -- LMR only reshapes which quiet
subtrees are scouted shallow, and the mandatory full-depth re-search preserves
the result -- so it is a pure efficiency control.")

(declaim (type (simple-array fixnum (64 64)) *lmr-table*))
(defvar *lmr-table*
  (let ((table (make-array '(64 64) :element-type 'fixnum :initial-element 0)))
    (loop for d from 1 below 64 do
      (loop for m from 1 below 64 do
        (setf (aref table d m)
              (floor (+ 0.99d0
                        (/ (* (log (coerce d 'double-float))
                              (log (coerce m 'double-float)))
                           3.14d0))))))
    table)
  "Precomputed base late-move reductions indexed by [depth][move-number].
Obsidian-style formula floor(0.99 + ln(depth)*ln(move-number)/3.14).  A pure
function of the integer indices -- no PRNG, no float state carried between
calls -- so it is fully deterministic and does not perturb reproducibility.")

(defun lmr-reduction (depth move-number)
  "Late-move reduction for a quiet move searched at remaining DEPTH as the
MOVE-NUMBER-th move (1-based).  The base table value is clamped to
[1, max(1, depth-2)]: an eligible move is reduced by at least one ply but
never loses all of its remaining depth."
  (declare (type fixnum depth move-number) (optimize (speed 3) (safety 2)))
  (let* ((d (min (max depth 1) 63))
         (m (min (max move-number 1) 63))
         (base (aref *lmr-table* d m))
         (hi (max 1 (- depth 2))))
    (declare (type fixnum d m base hi))
    (the fixnum (max 1 (min base hi)))))

(defun lmr-candidate-p (depth move-number pv-node-p in-check gives-check
                        extension pawn-move-p)
  "Late-move-reduction eligibility for a QUIET move.

Captures, promotions, killers and the countermove are searched at full depth by
their own ordering stages and never reach this test -- so the caller only offers
moves from the quiet stage.  A quiet move is reduced when the node has depth to
spare (>= +LMR-MIN-DEPTH+), the move is late in the ordering
(> +LMR-FULL-DEPTH-MOVES+), the node is off the principal variation, neither the
node nor the move gives check, the move is not being extended, and the move is
not a pawn advance.

Pawn moves are excluded deliberately: a reduced search can bury a passer's
promotion threat below the horizon, and because the full-depth re-search only
fires when the reduced search beats alpha, an under-valued push might never be
re-examined.  Excluding every pawn move strictly subsumes the narrower
\"passed-pawn\" exclusion and needs no passed-pawn predicate.  Correctness never
depends on this test being exact: any reduced search that beats alpha is
re-searched at full depth before its score is trusted (see PVS-SEARCH)."
  (declare (type fixnum depth move-number extension)
           (type boolean pv-node-p in-check gives-check pawn-move-p)
           (optimize (speed 3) (safety 2)))
  (and (>= depth +lmr-min-depth+)
       (> move-number +lmr-full-depth-moves+)
       (not pv-node-p)
       (not in-check)
       (not gives-check)
       (= extension 0)
       (not pawn-move-p)))

(declaim (inline effective-check-extension-cap))
(defun effective-check-extension-cap (root-depth)
  "Per-line check-extension budget for a root search requesting ROOT-DEPTH
nominal plies: +MAXIMUM-CHECK-EXTENSIONS+ scaled down by
+CHECK-EXTENSION-CAP-RATIO+, clamped between +CHECK-EXTENSION-CAP-FLOOR+ and
+MAXIMUM-CHECK-EXTENSIONS+ itself.  See CHECK-EXTENSION for why."
  (declare (type fixnum root-depth) (optimize (speed 3) (safety 2)))
  (the fixnum
       (min +maximum-check-extensions+
            (max +check-extension-cap-floor+
                 (ceiling root-depth +check-extension-cap-ratio+)))))

(defun check-extension (node-in-check gives-check extension-count)
  "One-ply search extension for forcing moves.

Returns 1 -- extend the child search by a single ply -- when this line's
extension budget is not yet spent (EXTENSION-COUNT < effective cap) and the
move is forcing: either it GIVES-CHECK (computed from the real resulting
position, so direct and discovered checks both qualify) or it is an evasion of a
check on this node (NODE-IN-CHECK).  Otherwise returns 0.

The effective cap is +MAXIMUM-CHECK-EXTENSIONS+ scaled down by
*SEARCH-ROOT-DEPTH*, the nominal depth requested for the current root search
(never below +CHECK-EXTENSION-CAP-FLOOR+, never above
+MAXIMUM-CHECK-EXTENSIONS+ itself).  A flat absolute cap applied regardless of
requested depth lets forcing subsequences dominate almost the entire tree at
shallow practical depths (measured: 80-97% of nodes carrying extension-made
plies at depth 4-5, see documentation/G14_CHECK_EXTENSION_CAP.md) while barely
mattering at deep requested searches, where the same constant is a small
fraction of the nominal depth; scaling the budget to the request keeps that
fraction roughly stable instead.

At most one ply is added even when both conditions hold, and the cap bounds the
total extension along any single line, so a run of checks can deepen the search
but never without limit.  The added ply lands on DEPTH, not PLY, so mate-distance
scoring is unaffected."
  (declare (type boolean node-in-check gives-check)
           (type fixnum extension-count)
           (optimize (speed 3) (safety 2)))
  (if (and (< extension-count
              (effective-check-extension-cap (the fixnum *search-root-depth*)))
           (or node-in-check gives-check))
      1
      0))

;;;; Principal Variation Search (PVS)

;; PVS-SEARCH calls QUIESCENCE-SEARCH (defined further below, at depth <= 0)
;; before QUIESCENCE-SEARCH's own definition has been compiled. Every other
;; cross-function hot call in this file (TT-PROBE/TT-STORE/PIECE-VALUE/
;; SCORE-MOVE above) declares an FTYPE first so SBCL can check/trust the
;; signature at the call site instead of emitting an unspecialized full
;; call -- declare this one too, for the same reason.
(declaim (ftype (function (board fixnum fixnum fixnum fixnum)
                          (values fixnum (or null move)))
                quiescence-search))

(defun pvs-search (board depth alpha beta ply
                   &key (pv-node-p t) (allow-null t) (verify t)
                        (extension-count 0) prev-move)
  "Negamax principal-variation search with alpha-beta bounds, staged move
ordering and verified null-move pruning.

Every score is relative to the side to move: each child is searched through a
negated, swapped window and negated on return, so there is a single evaluation
perspective and no white/black split.  The colour is read from the board, never
passed.  The first move of a node is searched on the full window; the rest are
scouted with a null window and re-searched on the full window only when the
scout fails high inside (ALPHA, BETA) -- the PVS optimisation.

PV-NODE-P marks a principal-variation (full-window) node for TT replacement
bias; PREV-MOVE is the move that led here, used for counter-move ordering.
EXTENSION-COUNT is the number of one-ply check extensions already spent along
this line; it caps runaway extension (see CHECK-EXTENSION) and is threaded down
into every child, incremented by the ply this move was extended.

ALLOW-NULL and VERIFY drive verified null-move pruning (Tabibi & Netanyahu,
2002).  Before the ordered move sweep an eligible node tries a null move (a
pass) searched R+1 plies shallower; if that fails high the node is pruned --
but only when VERIFY is nil.  With VERIFY set the cut is withheld: the real
moves are searched one ply shallower with verification disabled below, and a
subsequent fail-low (the zugzwang signature) forces a full-depth re-search on
the original window.  This defeats the zugzwang blindness of plain null move
while keeping most of its saving.  ALLOW-NULL is nil inside a null child so
null moves are never chained.

Returns (SCORE BEST-MOVE) as fail-soft negamax bounds."
  (declare (type board board)
           (type fixnum depth alpha beta ply extension-count)
           (type boolean pv-node-p allow-null verify)
           (type (or null move) prev-move)
           (optimize (speed 3) (safety 2)))
  (incf *search-nodes*)
  (when (> ply *search-seldepth*)
    (setf *search-seldepth* ply))
  (when (and (periodic-time-check-p) (search-time-up-p))
    (setf *search-aborted* t)
    (return-from pvs-search (values 0 nil)))
  (when (drawp board)
    (let ((color (board-side-to-move board)))
      (if (checkp board color)
          (let ((legal (legal-moves board color)))
            (return-from pvs-search
              (if (null legal)
                  (values (terminal-no-move-score board ply) nil)
                  (values 0 nil))))
          (return-from pvs-search (values 0 nil)))))
  (cond ((<= depth 0)
         (quiescence-search board alpha beta +quiescence-depth+ ply))
        (t
         (multiple-value-bind (tt-hit tt-score tt-move)
             (tt-probe board depth alpha beta ply)
           (when tt-hit
             (return-from pvs-search (values tt-score tt-move)))
           (let* ((color (board-side-to-move board))
                  (orig-alpha alpha)
                  (orig-beta beta)
                  (in-check (checkp board color))
                  (legal (legal-moves board color))
                  (best-move nil)
                  (best-score (the fixnum (- +mate-score+)))
                  (first-move-p t)
                  (null-fail-high nil)
                  (effective-depth depth)
                  (effective-verify verify)
                  (store-depth depth)
                  (tt-first nil)
                  (good-captures '())
                  (bad-captures '())
                  (killers '())
                  (counter nil)
                  (quiets '())
                  (move-count 0))
             (declare (type fixnum orig-alpha orig-beta best-score
                            effective-depth store-depth move-count)
                      (type symbol color)
                      (type list legal good-captures bad-captures killers quiets)
                      (type (or null move) best-move tt-first counter)
                      (type boolean in-check first-move-p pv-node-p
                            allow-null verify effective-verify null-fail-high))
             (when (null legal)
               (return-from pvs-search
                 (values (terminal-no-move-score board ply) nil)))
             ;; ---- Verified null-move pruning (R = +null-move-reduction+).
             (when (may-try-null-move-p allow-null pv-node-p depth in-check
                                        board color verify)
               (multiple-value-bind (null-board null-undo)
                   (make-null-move board (search-null-undo-for-ply ply))
                 (declare (ignore null-board))
                 (let ((null-score
                         (the fixnum
                              (- (the fixnum
                                      (pvs-search board
                                                  (the fixnum
                                                       (max 0 (- depth +null-move-reduction+ 1)))
                                                  (- beta) (- (the fixnum (1- beta)))
                                                  (1+ ply)
                                                  :pv-node-p nil
                                                  :allow-null nil
                                                  :verify verify
                                                  :extension-count extension-count))))))
                   (declare (type fixnum null-score))
                   (unmake-null-move board null-undo)
                   (unless *search-aborted*
                     (when (>= null-score beta)
                       (if verify
                           ;; Withhold the cut: search the real moves one ply
                           ;; shallower with verification off below; a fail-low
                           ;; then exposes zugzwang and forces the re-search.
                           (progn
                             (setf effective-depth (the fixnum (1- depth)))
                             (setf effective-verify nil)
                             (setf null-fail-high t))
                           ;; No verification pending: trust the cut immediately.
                           (progn
                             (tt-store board depth null-score 'lower nil ply pv-node-p)
                             (return-from pvs-search (values null-score nil)))))))
                 (when *search-aborted*
                   (return-from pvs-search (values 0 nil)))))
             ;; ---- Ordered move sweep, re-runnable for the zugzwang re-search.
             (multiple-value-setq (tt-first good-captures bad-captures killers counter quiets)
               (classify-moves board color legal tt-move prev-move ply))
             (labels ((search-one (a-move loop-depth loop-verify quiet-p allow-lmr)
                        (declare (type move a-move) (type fixnum loop-depth)
                                 (type boolean loop-verify quiet-p allow-lmr))
                        (incf move-count)
                        ;; The pawn-advance test must read the from-square BEFORE
                        ;; the move is played -- afterwards that square is empty.
                        (let ((move-number move-count)
                              (pawn-move-p (and quiet-p
                                                (pawnp (board-pos (board-board board)
                                                                  (move-from a-move))))))
                          (declare (type fixnum move-number)
                                   (type boolean pawn-move-p))
                          (multiple-value-bind (board-after undo)
                              (execute-move board a-move (search-undo-for-ply ply))
                            (declare (ignore board-after))
                            (unwind-protect
                                 (let* ((val 0)
                                        ;; Post-move check status of the side now to move --
                                        ;; whether this move gives check.  Computed from the
                                        ;; real resulting position, so direct and discovered
                                        ;; checks both qualify.  Computed once and reused by
                                        ;; the check extension and the LMR eligibility test.
                                        (gives-check (checkp board (invert-color color)))
                                        ;; One-ply check extension: extend forcing moves
                                        ;; (this move gives check, or -- when this node is
                                        ;; itself in check -- any evasion), capped along the
                                        ;; line by EXTENSION-COUNT.  The ply lands on DEPTH,
                                        ;; not PLY, so mate distance is unaffected.
                                        (extension (check-extension in-check gives-check
                                                                    extension-count))
                                        (child-extension-count
                                          (the fixnum (+ extension-count extension)))
                                        (child-depth (the fixnum
                                                          (+ (the fixnum (1- loop-depth))
                                                             extension)))
                                        ;; Late-move reduction (quiets only).  A checking or
                                        ;; checked move is never reduced: it fails
                                        ;; LMR-CANDIDATE-P on GIVES-CHECK / IN-CHECK and on the
                                        ;; non-zero EXTENSION, so extension and reduction are
                                        ;; mutually exclusive.  ALLOW-LMR is nil during the
                                        ;; null-move verification sweep so that pass stays a
                                        ;; faithful confirmation of the cut (see RUN-SWEEP).
                                        (reduction
                                          (if (and quiet-p allow-lmr *lmr-enabled*
                                                   (lmr-candidate-p
                                                    loop-depth move-number pv-node-p
                                                    in-check gives-check
                                                    extension pawn-move-p))
                                              (lmr-reduction loop-depth move-number)
                                              0)))
                                   (declare (type fixnum val child-depth extension
                                                  child-extension-count reduction)
                                            (type boolean gives-check))
                                   (cond
                                     (first-move-p
                                      (setf val (the fixnum
                                                     (- (the fixnum
                                                             (pvs-search board child-depth
                                                                         (- beta) (- alpha)
                                                                         (1+ ply)
                                                                         :pv-node-p pv-node-p
                                                                         :allow-null t
                                                                         :verify loop-verify
                                                                         :extension-count child-extension-count
                                                                         :prev-move a-move)))))
                                      (setf first-move-p nil))
                                     (t
                                      ;; Null-window scout, reduced by REDUCTION plies for
                                      ;; late quiets (REDUCTION 0 => ordinary full-depth scout).
                                      (setf val (the fixnum
                                                     (- (the fixnum
                                                             (pvs-search board
                                                                         (the fixnum
                                                                              (max 0 (- child-depth reduction)))
                                                                         (- (the fixnum (1+ alpha)))
                                                                         (- alpha)
                                                                         (1+ ply)
                                                                         :pv-node-p nil
                                                                         :allow-null t
                                                                         :verify loop-verify
                                                                         :extension-count child-extension-count
                                                                         :prev-move a-move)))))
                                      ;; Mandatory full-depth zero-window re-search when a
                                      ;; reduced move beats alpha, before any widening.
                                      ;; Gated on (NOT *SEARCH-ABORTED*): the scout returns
                                      ;; VAL=0 on a time/stop abort, and when ALPHA<0 (common
                                      ;; in deep negamax) (> VAL ALPHA) is true for that 0,
                                      ;; which would otherwise launch a full-depth re-search
                                      ;; that also aborts -- up to two extra recursive
                                      ;; searches per in-flight move on the abort hot path
                                      ;; before the abort is re-checked below.  Correctness
                                      ;; is unchanged (the UNLESS *SEARCH-ABORTED* guard
                                      ;; below already discards the bogus VAL); this only
                                      ;; avoids the wasted searches.
                                      (when (and (not *search-aborted*)
                                                 (> reduction 0) (> val alpha))
                                        (setf val (the fixnum
                                                       (- (the fixnum
                                                               (pvs-search board child-depth
                                                                           (- (the fixnum (1+ alpha)))
                                                                           (- alpha)
                                                                           (1+ ply)
                                                                           :pv-node-p nil
                                                                           :allow-null t
                                                                           :verify loop-verify
                                                                           :extension-count child-extension-count
                                                                           :prev-move a-move))))))
                                      ;; Full-window PVS re-search when the score lands
                                      ;; inside (ALPHA, BETA).  Re-check the abort here too:
                                      ;; the LMR re-search just above may itself have aborted
                                      ;; and returned VAL=0, which again satisfies
                                      ;; (> VAL ALPHA) when ALPHA<0.
                                      (when (and (not *search-aborted*)
                                                 (> val alpha) (< val beta))
                                        (setf val (the fixnum
                                                       (- (the fixnum
                                                               (pvs-search board child-depth
                                                                           (- beta) (- alpha)
                                                                           (1+ ply)
                                                                           :pv-node-p pv-node-p
                                                                           :allow-null t
                                                                           :verify loop-verify
                                                                           :extension-count child-extension-count
                                                                           :prev-move a-move))))))))
                                   (unless *search-aborted*
                                     (when (> val best-score)
                                       (setf best-score val)
                                       (setf best-move a-move))
                                     (when (> best-score alpha)
                                       (setf alpha best-score))))
                              (unmake-move board a-move undo))
                            ;; Both UPDATE-QUIET-HEURISTICS (via MOVE-QUIET-P, which reads
                            ;; the destination square and side-to-move) and TT-STORE (via
                            ;; BOARD-KEY) must run on THIS node's own pre-move position, not
                            ;; the child's post-move one -- BOARD is a single mutated
                            ;; make/unmake struct, not a copy, so it still reflects A-MOVE
                            ;; having been played until UNMAKE-MOVE (the UNWIND-PROTECT
                            ;; cleanup above) restores it. Deferring both calls to here,
                            ;; after the cleanup has run, keeps them keyed and classified on
                            ;; the correct position. See documentation/G14 for the diagnostic
                            ;; that traced a wrong-key TT store to this exact ordering.
                            (when *search-aborted*
                              (return-from pvs-search (values 0 nil)))
                            (when (>= alpha beta)
                              (update-quiet-heuristics board color a-move loop-depth ply prev-move)
                              (tt-store board loop-depth best-score 'lower a-move ply pv-node-p)
                              (return-from pvs-search (values best-score best-move))))))
                      (run-sweep (loop-depth loop-verify allow-lmr)
                        (declare (type fixnum loop-depth)
                                 (type boolean loop-verify allow-lmr))
                        (setf first-move-p t)
                        (setf move-count 0)
                        (when tt-first
                          (search-one tt-first loop-depth loop-verify nil allow-lmr))
                        (dolist (mv good-captures)
                          (search-one mv loop-depth loop-verify nil allow-lmr))
                        (dolist (mv killers)
                          (unless (and counter (equal-moves mv counter))
                            (search-one mv loop-depth loop-verify nil allow-lmr)))
                        (when counter
                          (search-one counter loop-depth loop-verify nil allow-lmr))
                        (dolist (mv quiets)
                          (search-one mv loop-depth loop-verify t allow-lmr))
                        (dolist (mv bad-captures)
                          (search-one mv loop-depth loop-verify nil allow-lmr))))
               ;; First sweep at the (possibly null-reduced) effective depth.  LMR is
               ;; withheld when this is the null-move verification sweep (null-fail-high):
               ;; reducing a late quiet there could hide the move that confirms the cut
               ;; and force a needless full-depth zugzwang re-search.
               (setf store-depth effective-depth)
               (run-sweep effective-depth effective-verify (not null-fail-high))
               ;; Zugzwang guard: a verified null fail-high that the shallower
               ;; real search could not confirm (best < orig-beta) is re-searched
               ;; at full depth with verification on and the original window.  This is
               ;; an honest full-depth search, so LMR applies (its mandatory full-depth
               ;; re-search keeps it correct).
               (when (and null-fail-high
                          (< best-score orig-beta)
                          (not *search-aborted*))
                 (setf best-score (the fixnum (- +mate-score+)))
                 (setf best-move nil)
                 (setf alpha orig-alpha)
                 (setf store-depth depth)
                 (run-sweep depth t t)))
             (let ((flag (cond ((<= best-score orig-alpha) 'upper)
                               ((>= best-score orig-beta) 'lower)
                               (t 'exact))))
               (tt-store board store-depth best-score flag best-move ply pv-node-p)
               (values best-score best-move)))))))

(defun alpha-beta (board depth alpha beta)
  "Compatibility wrapper for tests/benchmarks: negamax search from root ply 0."
    (declare (optimize (speed 3) (safety 2)))
(pvs-search board depth alpha beta 0))


;;;; Quiescence search

(defun quiescence-search (board alpha beta depth ply)
  "Negamax quiescence search: extend along captures and promotions -- and all
legal moves while in check (check-evasion) -- to damp horizon effects.

Scores are relative to the side to move, matching PVS-SEARCH; the colour is
read from the board.  DEPTH counts down the quiescence budget: at DEPTH <= 0 a
quiet (not-in-check) node returns its stand-pat; in-check nodes ignore the
budget so a forced check sequence is always resolved.

Results are cached in the transposition table at depth 0.  The replacement
policy is depth-aware, so these shallow entries never shadow a deeper
main-search result for the same key, and a probe from the main search (which
requires entry-depth >= its own depth) never takes a quiescence bound as a
cutoff.

Returns (SCORE BEST-MOVE) as fail-soft negamax bounds."
  (declare (type board board)
           (type fixnum alpha beta depth ply)
           (optimize (speed 3) (safety 2)))
  (incf *search-nodes*)
  (incf *qsearch-nodes*)
  (when (> ply *search-seldepth*)
    (setf *search-seldepth* ply))
  (when (and (periodic-time-check-p) (search-time-up-p))
    (setf *search-aborted* t)
    (return-from quiescence-search (values 0 nil)))
  (let ((color (board-side-to-move board)))
    (when (drawp board)
      (if (checkp board color)
          (let ((legal (legal-moves board color)))
            (return-from quiescence-search
              (if (null legal)
                  (values (mated-score ply) nil)
                  (values 0 nil))))
          (return-from quiescence-search (values 0 nil))))
    (multiple-value-bind (tt-hit tt-score tt-move)
        (tt-probe board 0 alpha beta ply)
      (declare (ignore tt-move))
      (when tt-hit
        (return-from quiescence-search (values tt-score nil)))
      (let ((orig-alpha alpha)
            (in-check (checkp board color)))
        (declare (type fixnum orig-alpha) (type boolean in-check))
        (let ((best-move nil)
              (best-score (the fixnum (- +mate-score+)))
              (legal nil)
              (children nil))
          (declare (type (or null move) best-move) (type fixnum best-score)
                   (type list legal children))
          ;; Stand-pat is illegitimate while in check: the side must find an
          ;; evasion, so best-score stays at -mate until a move improves it.
          (unless in-check
            (let ((stand-pat (static-eval-stm board)))
              (declare (type fixnum stand-pat))
              (setf best-score stand-pat)
              (when (> stand-pat alpha)
                (setf alpha stand-pat))
              (when (or (>= alpha beta) (<= depth 0))
                (tt-store board 0 stand-pat 'lower nil ply nil)
                (return-from quiescence-search (values stand-pat nil)))))
          (setf legal (legal-moves board color))
          (when (null legal)
            (return-from quiescence-search
              (values (if in-check (mated-score ply) 0) nil)))
          (setf children (if in-check
                             legal
                             (next-qsearch-moves board legal)))
          (dolist (a-move children)
          (declare (type move a-move))
          ;; SEE-based pruning of clearly-losing captures; bypassed for
          ;; promotions and for every evasion while in check.
          (when (or in-check
                    (move-promotion a-move)
                    (> (see-sign board a-move) +qsearch-see-prune-threshold+))
            (multiple-value-bind (board-after undo)
                (execute-move board a-move (search-undo-for-ply ply))
              (declare (ignore board-after))
              (unwind-protect
                   (let ((val (the fixnum
                                     (- (the fixnum
                                             (quiescence-search board (- beta) (- alpha)
                                                                (1- depth) (1+ ply)))))))
                       (declare (type fixnum val))
                       (unless *search-aborted*
                         (when (> val best-score)
                           (setf best-score val)
                           (setf best-move a-move))
                         (when (> best-score alpha)
                           (setf alpha best-score))))
                  (unmake-move board a-move undo))
                ;; TT-STORE reads BOARD-KEY; it must run on this node's own
                ;; pre-move position, not A-MOVE's post-move one -- see the
                ;; matching comment in PVS-SEARCH's SEARCH-ONE for the full
                ;; explanation (BOARD is a single mutated make/unmake struct,
                ;; still reflecting A-MOVE until UNMAKE-MOVE above restores it).
                (when *search-aborted*
                  (return-from quiescence-search (values 0 nil)))
                (when (>= alpha beta)
                  (tt-store board 0 best-score 'lower a-move ply nil)
                  (return-from quiescence-search (values best-score best-move))))))
        (let ((flag (cond ((<= best-score orig-alpha) 'upper)
                          ((>= best-score beta) 'lower)
                          (t 'exact))))
          (tt-store board 0 best-score flag best-move ply nil))
        (values best-score best-move))))))


;;;; Static evaluation

(defconstant +knight-rim-edge-penalty+ 24)
(defconstant +knight-rim-near-penalty+ 10)
(defconstant +development-weight+ 8)
(defconstant +uncastled-king-penalty+ 8)
(defconstant +wing-pawn-push-penalty+ 18)
(defconstant +castled-bonus+ 8)
(defconstant +center-pawn-bonus+ 16)
(defconstant +minor-mobility-weight+ 2)
(defconstant +edge-bishop-penalty+ 14)
(defconstant +low-bishop-mobility-penalty+ 8)
(defconstant +blocked-central-pawn-penalty+ 24)

;; Minor-piece safety: a knight or bishop that has crossed into the opponent's
;; half is only worth its advanced piece-square bonus when it cannot be chased
;; off cheaply.  A minor there that no friendly pawn defends and that an enemy
;; pawn already attacks -- or can attack after a single push (a "kick") -- is a
;; premature advance that typically costs a tempo, so its advanced placement is
;; penalised in proportion to how deep it has strayed.  A knight that is instead
;; pawn-supported on a square no enemy pawn can ever reach is a true outpost and
;; earns a bonus.  These are generic positional principles, not position
;; constants, and remain exactly colour-antisymmetric (see TEST-EVAL-SYMMETRY).
(defconstant +minor-unstable-base+ 10
  "Base penalty for an unsupported minor in the enemy half a pawn push can hit.")
(defconstant +minor-advance-step+ 7
  "Extra unstable/loose penalty per rank the minor has advanced past the middle.")
(defconstant +minor-loose-base+ 22
  "Base penalty for an unsupported minor the enemy already attacks with a pawn.")
(defconstant +minor-attacked-defended-penalty+ 8
  "Penalty for a pawn-defended minor an enemy pawn attacks (harassed, not loose).")
(defconstant +knight-outpost-bonus+ 14
  "Bonus for a pawn-supported knight on an enemy-half square no pawn can attack.")

(defconstant +kbnk-edge-weight+ 35
  "KBNK bonus per step the bare king is pushed from centre toward any edge.")
(defconstant +kbnk-correct-corner-weight+ 40
  "KBNK bonus per step the bare king approaches a bishop-coloured corner.")
(defconstant +kbnk-attacking-king-weight+ 15
  "KBNK bonus per Chebyshev step the strong king approaches the bare king.")
(defconstant +kbnk-correct-corner-bonus+ 80
  "Extra KBNK bonus once the bare king reaches a bishop-coloured corner.")

(defconstant +elementary-mate-edge-weight+ 30
  "Elementary endgame bonus per step the bare king is pushed toward any edge.")
(defconstant +elementary-mate-corner-weight+ 18
  "Elementary endgame bonus per step the bare king approaches any corner.")
(defconstant +elementary-mate-king-weight+ 12
  "Elementary endgame bonus per step the strong king approaches the bare king.")
(defconstant +elementary-mate-corner-bonus+ 45
  "Extra elementary endgame bonus once the bare king is in any corner.")
(defconstant +elementary-mate-queen-scale+ 1
  "Multiplier for the KQK mating-technique bonus.")
(defconstant +elementary-mate-rook-scale+ 2
  "Multiplier for the KRK mating-technique bonus.")
(defconstant +elementary-mate-bishops-scale+ 2
  "Multiplier for the KBBK mating-technique bonus.")

(defconstant +minor-pawn-advanced-bonus+ 80
  "Conservative bonus for supported, advanced minor-plus-pawn endgames.")
(defconstant +minor-pawn-blockade-penalty+ 120
  "Draw-scaling penalty when the defender king blockades the pawn from the front.")
(defconstant +minor-pawn-defender-control-penalty+ 90
  "Draw-scaling penalty when the defending minor controls the promotion square.")
(defconstant +minor-pawn-wrong-bishop-penalty+ 180
  "Draw-scaling penalty for rook pawn plus wrong bishop fortress patterns.")
(defconstant +minor-pawn-opposite-bishop-penalty+ 90
  "Draw-scaling penalty for many one-pawn opposite-coloured bishop endgames.")
(defconstant +minor-pawn-knight-blockade-penalty+ 70
  "Draw-scaling penalty when a defending knight hits the pawn/blockade square.")

(defconstant +rook-vs-minor-draw-penalty+ 170
  "Baseline draw-scaling penalty for KR vs KB/KN without pawns.")
(defconstant +queen-vs-pawn-draw-penalty+ 650
  "Draw-scaling penalty for known supported rook/bishop-pawn-on-7th Q vs P motifs.")

(defun kbnk-endgame-info (board)
  "Return exact KBNK data as values, or SIGN = 0 when another material exists.

SIGN is 1 when White has bishop+knight against a bare black king, -1 for the
reverse.  BISHOP-COLOR is the strong side bishop's colour complex; WEAK-* and
STRONG-* are king coordinates.  This deliberately excludes every other material
configuration, including extra bishops/knights, so the heuristic cannot leak into
ordinary endgames."
  (declare (type board board) (optimize (speed 3) (safety 2)))
  (let ((white-bishops 0)
        (white-knights 0)
        (black-bishops 0)
        (black-knights 0)
        (white-bishop-color 0)
        (black-bishop-color 0))
    (declare (type fixnum white-bishops white-knights black-bishops black-knights
                   white-bishop-color black-bishop-color))
    (dotimes (row 8)
      (dotimes (col 8)
        (let ((piece (aref (board-board board) row col)))
          (case piece
            (ab
             (incf white-bishops)
             (setf white-bishop-color (bishop-square-color row col)))
            (cb
             (incf white-knights))
            (an
             (incf black-bishops)
             (setf black-bishop-color (bishop-square-color row col)))
            (cn
             (incf black-knights))
            ((rb rn vv) nil)
            (otherwise
             (return-from kbnk-endgame-info (values 0 0 0 0 0 0)))))))
    (cond ((and (= white-bishops 1) (= white-knights 1)
                (= black-bishops 0) (= black-knights 0))
           (let ((weak (board-black-king-pos board))
                 (strong (board-white-king-pos board)))
             (values 1 white-bishop-color
                     (pos-row weak) (pos-col weak)
                     (pos-row strong) (pos-col strong))))
          ((and (= black-bishops 1) (= black-knights 1)
                (= white-bishops 0) (= white-knights 0))
           (let ((weak (board-white-king-pos board))
                 (strong (board-black-king-pos board)))
             (values -1 black-bishop-color
                     (pos-row weak) (pos-col weak)
                     (pos-row strong) (pos-col strong))))
          (t (values 0 0 0 0 0 0)))))

(defun kbnk-correct-corner-distance (row col bishop-color)
  "Manhattan distance from (ROW,COL) to the nearest bishop-coloured corner."
  (declare (type fixnum row col bishop-color) (optimize (speed 3) (safety 2)))
  (if (= bishop-color 0)
      (min (+ row col)
           (+ (- 7 row) (- 7 col)))
      (min (+ row (- 7 col))
           (+ (- 7 row) col))))

(defun kbnk-endgame-bonus (board)
  "Return a white-relative KBNK mating-technique bonus for exact KBNK only.

The bonus is a bounded centipawn-scale gradient: drive the bare king to an edge,
prefer the bishop-coloured mating corners over the wrong corners, and bring the
strong king closer.  It is intentionally far below mate scores; legal search still
has to find actual checkmate."
  (declare (type board board) (optimize (speed 3) (safety 2)))
  (multiple-value-bind (sign bishop-color weak-row weak-col strong-row strong-col)
      (kbnk-endgame-info board)
    (declare (type fixnum sign bishop-color weak-row weak-col strong-row strong-col))
    (if (= sign 0)
        0
        (let* ((edge-distance (min weak-row weak-col (- 7 weak-row) (- 7 weak-col)))
               (edge-progress (- 3 edge-distance))
               (corner-distance (kbnk-correct-corner-distance weak-row weak-col
                                                              bishop-color))
               (corner-progress (- 7 corner-distance))
               (king-distance (max (abs (- strong-row weak-row))
                                   (abs (- strong-col weak-col))))
               (king-progress (- 7 king-distance))
               (correct-corner-p (and (or (= weak-row 0) (= weak-row 7))
                                      (or (= weak-col 0) (= weak-col 7))
                                      (= (bishop-square-color weak-row weak-col)
                                         bishop-color)))
               (bonus (+ (* +kbnk-edge-weight+ edge-progress)
                         (* +kbnk-correct-corner-weight+ corner-progress)
                         (* +kbnk-attacking-king-weight+ king-progress)
                         (if correct-corner-p +kbnk-correct-corner-bonus+ 0))))
          (declare (type fixnum edge-distance edge-progress corner-distance
                         corner-progress king-distance king-progress bonus))
          (the fixnum (* sign bonus))))))

(defun elementary-lone-king-endgame-info (board)
  "Return exact KQK/KRK/KBBK data as values, or SIGN = 0 for other material.

TYPE is one of :QUEEN, :ROOK or :BISHOPS.  SIGN is 1 when White is the strong
side and -1 when Black is the strong side.  KBBK requires two bishops on opposite
colour complexes; same-colour bishops against a bare king are already draw
material and are deliberately excluded."
  (declare (type board board) (optimize (speed 3) (safety 2)))
  (let ((white-queens 0)
        (white-rooks 0)
        (white-bishops 0)
        (white-knights 0)
        (black-queens 0)
        (black-rooks 0)
        (black-bishops 0)
        (black-knights 0)
        (white-bishop-color -1)
        (white-second-bishop-color -1)
        (black-bishop-color -1)
        (black-second-bishop-color -1))
    (declare (type fixnum white-queens white-rooks white-bishops white-knights
                   black-queens black-rooks black-bishops black-knights
                   white-bishop-color white-second-bishop-color
                   black-bishop-color black-second-bishop-color))
    (dotimes (row 8)
      (dotimes (col 8)
        (let ((piece (aref (board-board board) row col)))
          (case piece
            (db (incf white-queens))
            (tb (incf white-rooks))
            (ab
             (if (zerop white-bishops)
                 (setf white-bishop-color (bishop-square-color row col))
                 (setf white-second-bishop-color (bishop-square-color row col)))
             (incf white-bishops))
            (cb (incf white-knights))
            (dn (incf black-queens))
            (tn (incf black-rooks))
            (an
             (if (zerop black-bishops)
                 (setf black-bishop-color (bishop-square-color row col))
                 (setf black-second-bishop-color (bishop-square-color row col)))
             (incf black-bishops))
            (cn (incf black-knights))
            ((rb rn vv) nil)
            (otherwise
             (return-from elementary-lone-king-endgame-info
               (values 0 nil 0 0 0 0)))))))
    (labels ((emit (sign type weak strong)
               (values sign type
                       (pos-row weak) (pos-col weak)
                       (pos-row strong) (pos-col strong))))
      (cond ((and (= white-queens 1) (= white-rooks 0) (= white-bishops 0)
                  (= white-knights 0) (= black-queens 0) (= black-rooks 0)
                  (= black-bishops 0) (= black-knights 0))
             (emit 1 :queen (board-black-king-pos board) (board-white-king-pos board)))
            ((and (= black-queens 1) (= black-rooks 0) (= black-bishops 0)
                  (= black-knights 0) (= white-queens 0) (= white-rooks 0)
                  (= white-bishops 0) (= white-knights 0))
             (emit -1 :queen (board-white-king-pos board) (board-black-king-pos board)))
            ((and (= white-rooks 1) (= white-queens 0) (= white-bishops 0)
                  (= white-knights 0) (= black-queens 0) (= black-rooks 0)
                  (= black-bishops 0) (= black-knights 0))
             (emit 1 :rook (board-black-king-pos board) (board-white-king-pos board)))
            ((and (= black-rooks 1) (= black-queens 0) (= black-bishops 0)
                  (= black-knights 0) (= white-queens 0) (= white-rooks 0)
                  (= white-bishops 0) (= white-knights 0))
             (emit -1 :rook (board-white-king-pos board) (board-black-king-pos board)))
            ((and (= white-bishops 2) (= white-queens 0) (= white-rooks 0)
                  (= white-knights 0) (= black-queens 0) (= black-rooks 0)
                  (= black-bishops 0) (= black-knights 0)
                  (/= white-bishop-color white-second-bishop-color))
             (emit 1 :bishops (board-black-king-pos board) (board-white-king-pos board)))
            ((and (= black-bishops 2) (= black-queens 0) (= black-rooks 0)
                  (= black-knights 0) (= white-queens 0) (= white-rooks 0)
                  (= white-bishops 0) (= white-knights 0)
                  (/= black-bishop-color black-second-bishop-color))
             (emit -1 :bishops (board-white-king-pos board) (board-black-king-pos board)))
            (t (values 0 nil 0 0 0 0))))))

(defun elementary-corner-distance (row col)
  "Manhattan distance from (ROW,COL) to the nearest board corner."
  (declare (type fixnum row col) (optimize (speed 3) (safety 2)))
  (min (+ row col)
       (+ row (- 7 col))
       (+ (- 7 row) col)
       (+ (- 7 row) (- 7 col))))

(defun elementary-lone-king-endgame-bonus (board)
  "Return a white-relative mating-technique bonus for exact KQK/KRK/KBBK.

The gradient is deliberately bounded: push the bare king to an edge/corner and
bring the strong king closer.  It guides search in elementary mates while staying
far below terminal mate scores."
  (declare (type board board) (optimize (speed 3) (safety 2)))
  (multiple-value-bind (sign type weak-row weak-col strong-row strong-col)
      (elementary-lone-king-endgame-info board)
    (declare (type fixnum sign weak-row weak-col strong-row strong-col))
    (if (= sign 0)
        0
        (let* ((edge-distance (min weak-row weak-col (- 7 weak-row) (- 7 weak-col)))
               (edge-progress (- 3 edge-distance))
               (corner-distance (elementary-corner-distance weak-row weak-col))
               (corner-progress (- 7 corner-distance))
               (king-distance (max (abs (- strong-row weak-row))
                                   (abs (- strong-col weak-col))))
               (king-progress (- 7 king-distance))
               (corner-p (and (or (= weak-row 0) (= weak-row 7))
                              (or (= weak-col 0) (= weak-col 7))))
               (scale (case type
                        (:queen +elementary-mate-queen-scale+)
                        (:rook +elementary-mate-rook-scale+)
                        (:bishops +elementary-mate-bishops-scale+)
                        (t 0)))
               (bonus (* scale
                         (+ (* +elementary-mate-edge-weight+ edge-progress)
                            (* +elementary-mate-corner-weight+ corner-progress)
                            (* +elementary-mate-king-weight+ king-progress)
                            (if corner-p +elementary-mate-corner-bonus+ 0)))))
          (declare (type fixnum edge-distance edge-progress corner-distance
                         corner-progress king-distance king-progress scale bonus))
          (the fixnum (* sign bonus))))))

(defun chebyshev-distance (row1 col1 row2 col2)
  "Return king-move distance between two squares."
  (declare (type fixnum row1 col1 row2 col2) (optimize (speed 3) (safety 2)))
  (max (abs (- row1 row2)) (abs (- col1 col2))))

(defun clear-diagonal-between-p (board from-row from-col to-row to-col)
  "True if the diagonal segment between FROM and TO has no occupied squares."
  (declare (type board board) (type fixnum from-row from-col to-row to-col)
           (optimize (speed 3) (safety 2)))
  (let ((dr (cond ((< from-row to-row) 1) ((> from-row to-row) -1) (t 0)))
        (dc (cond ((< from-col to-col) 1) ((> from-col to-col) -1) (t 0))))
    (declare (type fixnum dr dc))
    (and (/= dr 0)
         (/= dc 0)
         (= (abs (- from-row to-row)) (abs (- from-col to-col)))
         (do ((r (+ from-row dr) (+ r dr))
              (c (+ from-col dc) (+ c dc)))
             ((and (= r to-row) (= c to-col)) t)
           (declare (type fixnum r c))
           (unless (eq (aref (board-board board) r c) 'vv)
             (return nil))))))

(defun minor-controls-square-p (board minor row col target-row target-col)
  "True if MINOR on ROW,COL attacks TARGET-ROW,TARGET-COL."
  (declare (type board board) (type symbol minor)
           (type fixnum row col target-row target-col)
           (optimize (speed 3) (safety 2)))
  (cond ((or (eq minor :knight) (eq minor 'cb) (eq minor 'cn))
         (let ((dr (abs (- row target-row)))
               (dc (abs (- col target-col))))
           (declare (type fixnum dr dc))
           (or (and (= dr 1) (= dc 2))
               (and (= dr 2) (= dc 1)))))
        ((or (eq minor :bishop) (eq minor 'ab) (eq minor 'an))
         (and (= (abs (- row target-row)) (abs (- col target-col)))
              (clear-diagonal-between-p board row col target-row target-col)))
        (t nil)))

(defun exact-minor-pawn-vs-minor-info (board)
  "Return exact minor+pawn-vs-minor data, or SIGN = 0 for other material.

SIGN is the pawn side (1 for White, -1 for Black).  This helper deliberately
rejects every extra piece so draw-scaling cannot leak into richer positions."
  (declare (type board board) (optimize (speed 3) (safety 2)))
  (let ((white-pawns 0) (black-pawns 0)
        (white-bishops 0) (white-knights 0)
        (black-bishops 0) (black-knights 0)
        (wp-row 0) (wp-col 0) (bp-row 0) (bp-col 0)
        (wb-row 0) (wb-col 0) (wn-row 0) (wn-col 0)
        (bb-row 0) (bb-col 0) (bn-row 0) (bn-col 0))
    (declare (type fixnum white-pawns black-pawns white-bishops white-knights
                   black-bishops black-knights wp-row wp-col bp-row bp-col
                   wb-row wb-col wn-row wn-col bb-row bb-col bn-row bn-col))
    (dotimes (row 8)
      (dotimes (col 8)
        (case (aref (board-board board) row col)
          (pb (incf white-pawns) (setf wp-row row wp-col col))
          (pn (incf black-pawns) (setf bp-row row bp-col col))
          (ab (incf white-bishops) (setf wb-row row wb-col col))
          (cb (incf white-knights) (setf wn-row row wn-col col))
          (an (incf black-bishops) (setf bb-row row bb-col col))
          (cn (incf black-knights) (setf bn-row row bn-col col))
          ((rb rn vv) nil)
          (otherwise
           (return-from exact-minor-pawn-vs-minor-info
             (values 0 nil nil 0 0 0 0 0 0 0 0))))))
    (let ((white-minors (+ white-bishops white-knights))
          (black-minors (+ black-bishops black-knights)))
      (declare (type fixnum white-minors black-minors))
      (cond ((and (= white-pawns 1) (= black-pawns 0)
                  (= white-minors 1) (= black-minors 1))
             (let ((attacker (if (= white-bishops 1) :bishop :knight))
                   (defender (if (= black-bishops 1) :bishop :knight))
                   (def-row (if (= black-bishops 1) bb-row bn-row))
                   (def-col (if (= black-bishops 1) bb-col bn-col))
                   (strong (board-white-king-pos board))
                   (weak (board-black-king-pos board)))
               (values 1 attacker defender wp-row wp-col
                       (pos-row strong) (pos-col strong)
                       (pos-row weak) (pos-col weak) def-row def-col)))
            ((and (= black-pawns 1) (= white-pawns 0)
                  (= black-minors 1) (= white-minors 1))
             (let ((attacker (if (= black-bishops 1) :bishop :knight))
                   (defender (if (= white-bishops 1) :bishop :knight))
                   (def-row (if (= white-bishops 1) wb-row wn-row))
                   (def-col (if (= white-bishops 1) wb-col wn-col))
                   (strong (board-black-king-pos board))
                   (weak (board-white-king-pos board)))
               (values -1 attacker defender bp-row bp-col
                       (pos-row strong) (pos-col strong)
                       (pos-row weak) (pos-col weak) def-row def-col)))
            (t (values 0 nil nil 0 0 0 0 0 0 0 0))))))

(defun pawn-advance-progress (sign pawn-row)
  "Return how far the pawn side has advanced from its home rank."
  (declare (type fixnum sign pawn-row) (optimize (speed 3) (safety 2)))
  (if (= sign 1)
      (- 6 pawn-row)
      (- pawn-row 1)))

(defun pawn-promotion-row (sign)
  "Return the promotion row for SIGN's pawn side."
  (declare (type fixnum sign) (optimize (speed 3) (safety 2)))
  (if (= sign 1) 0 7))

(defun pawn-front-row (sign pawn-row)
  "Return the square row immediately in front of SIGN's pawn."
  (declare (type fixnum sign pawn-row) (optimize (speed 3) (safety 2)))
  (+ pawn-row (if (= sign 1) -1 1)))

(defun rook-pawn-file-p (col)
  "True if COL is an a- or h-file pawn."
  (declare (type fixnum col) (optimize (speed 3) (safety 2)))
  (or (= col 0) (= col 7)))

(defun bishop-pawn-file-p (col)
  "True if COL is a c- or f-file pawn."
  (declare (type fixnum col) (optimize (speed 3) (safety 2)))
  (or (= col 2) (= col 5)))

(defun attacker-bishop-color-for-sign (board sign)
  "Return SIGN side's only bishop colour complex, or -1 if no such bishop exists."
  (declare (type board board) (type fixnum sign) (optimize (speed 3) (safety 2)))
  (let ((bishop (if (= sign 1) 'ab 'an)))
    (dotimes (row 8 -1)
      (dotimes (col 8)
        (when (eq (aref (board-board board) row col) bishop)
          (return-from attacker-bishop-color-for-sign
            (bishop-square-color row col)))))))

(defun minor-pawn-vs-minor-endgame-bonus (board)
  "Return a conservative white-relative bonus for exact minor+pawn-vs-minor.

The term intentionally penalizes classic drawish patterns more than it rewards
progress, so the engine avoids false confidence in blockades and wrong-bishop
rook-pawn positions."
  (declare (type board board) (optimize (speed 3) (safety 2)))
  (multiple-value-bind (sign attacker defender pawn-row pawn-col strong-row strong-col
                        weak-row weak-col def-row def-col)
      (exact-minor-pawn-vs-minor-info board)
    (declare (type fixnum sign pawn-row pawn-col strong-row strong-col
                   weak-row weak-col def-row def-col))
    (if (= sign 0)
        0
        (let* ((promotion-row (pawn-promotion-row sign))
               (front-row (pawn-front-row sign pawn-row))
               (progress (max 0 (pawn-advance-progress sign pawn-row)))
               (front-distance (if (in-board-p front-row pawn-col)
                                   (chebyshev-distance strong-row strong-col
                                                       front-row pawn-col)
                                   (chebyshev-distance strong-row strong-col
                                                       pawn-row pawn-col)))
               (king-support (max 0 (- 4 front-distance)))
               (base (+ (* progress 22) (* king-support 28)))
               (defender-in-front-p (and (= weak-col pawn-col)
                                         (if (= sign 1)
                                             (< weak-row pawn-row)
                                             (> weak-row pawn-row))))
               (defender-controls-promotion-p
                 (minor-controls-square-p board defender def-row def-col
                                          promotion-row pawn-col))
               (defender-knight-blockade-p
                 (and (eq defender :knight)
                      (or (minor-controls-square-p board defender def-row def-col
                                                   pawn-row pawn-col)
                          (and (in-board-p front-row pawn-col)
                               (minor-controls-square-p board defender def-row def-col
                                                        front-row pawn-col)))))
               (attacker-bishop-color (attacker-bishop-color-for-sign board sign))
               (wrong-bishop-p (and (eq attacker :bishop)
                                    (rook-pawn-file-p pawn-col)
                                    (/= attacker-bishop-color -1)
                                    (/= (bishop-square-color promotion-row pawn-col)
                                        attacker-bishop-color)))
               (opposite-bishops-p (and (eq attacker :bishop) (eq defender :bishop)))
               (penalty (+ (if defender-in-front-p +minor-pawn-blockade-penalty+ 0)
                           (if defender-controls-promotion-p
                               +minor-pawn-defender-control-penalty+
                               0)
                           (if defender-knight-blockade-p
                               +minor-pawn-knight-blockade-penalty+
                               0)
                           (if wrong-bishop-p +minor-pawn-wrong-bishop-penalty+ 0)
                           (if opposite-bishops-p +minor-pawn-opposite-bishop-penalty+ 0)))
               (score (- base penalty)))
          (declare (type fixnum promotion-row front-row progress front-distance
                         king-support attacker-bishop-color base penalty score))
          (the fixnum (* sign (max -260 (min 220 score))))))))

(defun exact-major-minor-endgame-info (board)
  "Return exact KQ-KR / KR-KB / KR-KN data, or SIGN = 0 for other material."
  (declare (type board board) (optimize (speed 3) (safety 2)))
  (let ((wq 0) (wr 0) (wb 0) (wn 0) (bq 0) (br 0) (bb 0) (bn 0))
    (declare (type fixnum wq wr wb wn bq br bb bn))
    (dotimes (row 8)
      (dotimes (col 8)
        (case (aref (board-board board) row col)
          (db (incf wq)) (tb (incf wr)) (ab (incf wb)) (cb (incf wn))
          (dn (incf bq)) (tn (incf br)) (an (incf bb)) (cn (incf bn))
          ((rb rn vv) nil)
          (otherwise
           (return-from exact-major-minor-endgame-info (values 0 nil 0 0 0 0))))))
    (labels ((emit (sign type weak strong)
               (values sign type (pos-row weak) (pos-col weak)
                       (pos-row strong) (pos-col strong))))
      (cond ((and (= wq 1) (= br 1) (= wr 0) (= wb 0) (= wn 0)
                  (= bq 0) (= bb 0) (= bn 0))
             (emit 1 :q-v-r (board-black-king-pos board) (board-white-king-pos board)))
            ((and (= bq 1) (= wr 1) (= br 0) (= bb 0) (= bn 0)
                  (= wq 0) (= wb 0) (= wn 0))
             (emit -1 :q-v-r (board-white-king-pos board) (board-black-king-pos board)))
            ((and (= wr 1) (= bb 1) (= wq 0) (= wb 0) (= wn 0)
                  (= bq 0) (= br 0) (= bn 0))
             (emit 1 :r-v-b (board-black-king-pos board) (board-white-king-pos board)))
            ((and (= br 1) (= wb 1) (= bq 0) (= bb 0) (= bn 0)
                  (= wq 0) (= wr 0) (= wn 0))
             (emit -1 :r-v-b (board-white-king-pos board) (board-black-king-pos board)))
            ((and (= wr 1) (= bn 1) (= wq 0) (= wb 0) (= wn 0)
                  (= bq 0) (= br 0) (= bb 0))
             (emit 1 :r-v-n (board-black-king-pos board) (board-white-king-pos board)))
            ((and (= br 1) (= wn 1) (= bq 0) (= bb 0) (= bn 0)
                  (= wq 0) (= wr 0) (= wb 0))
             (emit -1 :r-v-n (board-white-king-pos board) (board-black-king-pos board)))
            (t (values 0 nil 0 0 0 0))))))

(defun major-minor-endgame-bonus (board)
  "Return conservative conversion/draw-scaling for exact major/minor imbalances."
  (declare (type board board) (optimize (speed 3) (safety 2)))
  (multiple-value-bind (sign type weak-row weak-col strong-row strong-col)
      (exact-major-minor-endgame-info board)
    (declare (type fixnum sign weak-row weak-col strong-row strong-col))
    (if (= sign 0)
        0
        (let* ((edge-distance (min weak-row weak-col (- 7 weak-row) (- 7 weak-col)))
               (edge-progress (- 3 edge-distance))
               (corner-progress (- 7 (elementary-corner-distance weak-row weak-col)))
               (king-progress (- 7 (chebyshev-distance strong-row strong-col
                                                       weak-row weak-col)))
               (activity (+ (* 12 edge-progress)
                            (* 6 corner-progress)
                            (* 8 king-progress)))
               (score (case type
                        (:q-v-r activity)
                        (:r-v-b (- activity +rook-vs-minor-draw-penalty+))
                        (:r-v-n (- activity +rook-vs-minor-draw-penalty+))
                        (t 0))))
          (declare (type fixnum edge-distance edge-progress corner-progress
                         king-progress activity score))
          (the fixnum (* sign (max -220 (min 180 score))))))))

(defun exact-queen-vs-pawn-info (board)
  "Return exact KQ-vs-KP data, or SIGN = 0 for other material."
  (declare (type board board) (optimize (speed 3) (safety 2)))
  (let ((wq 0) (bq 0) (wp 0) (bp 0) (pawn-row 0) (pawn-col 0))
    (declare (type fixnum wq bq wp bp pawn-row pawn-col))
    (dotimes (row 8)
      (dotimes (col 8)
        (case (aref (board-board board) row col)
          (db (incf wq)) (dn (incf bq))
          (pb (incf wp) (setf pawn-row row pawn-col col))
          (pn (incf bp) (setf pawn-row row pawn-col col))
          ((rb rn vv) nil)
          (otherwise
           (return-from exact-queen-vs-pawn-info (values 0 0 0 0 0 0 0))))))
    (cond ((and (= wq 1) (= bp 1) (= bq 0) (= wp 0))
           (let ((strong (board-white-king-pos board))
                 (weak (board-black-king-pos board)))
             (values 1 pawn-row pawn-col (pos-row strong) (pos-col strong)
                     (pos-row weak) (pos-col weak))))
          ((and (= bq 1) (= wp 1) (= wq 0) (= bp 0))
           (let ((strong (board-black-king-pos board))
                 (weak (board-white-king-pos board)))
             (values -1 pawn-row pawn-col (pos-row strong) (pos-col strong)
                     (pos-row weak) (pos-col weak))))
          (t (values 0 0 0 0 0 0 0)))))

(defun queen-vs-pawn-endgame-bonus (board)
  "Return conservative Q-vs-P scaling, including rook/bishop-pawn draw motifs."
  (declare (type board board) (optimize (speed 3) (safety 2)))
  (multiple-value-bind (sign pawn-row pawn-col strong-row strong-col weak-row weak-col)
      (exact-queen-vs-pawn-info board)
    (declare (type fixnum sign pawn-row pawn-col strong-row strong-col weak-row weak-col))
    (if (= sign 0)
        0
        (let* ((pawn-sign (- sign))
               (promotion-row (pawn-promotion-row pawn-sign))
               (near-promotion-p (= pawn-row (if (= pawn-sign 1) 1 6)))
               (rook-or-bishop-pawn-p (or (rook-pawn-file-p pawn-col)
                                          (bishop-pawn-file-p pawn-col)))
               (pawn-king-support-p (<= (chebyshev-distance weak-row weak-col
                                                            promotion-row pawn-col)
                                        1))
               (queen-king-far-p (> (chebyshev-distance strong-row strong-col
                                                        promotion-row pawn-col)
                                    3))
               (drawish-p (and near-promotion-p rook-or-bishop-pawn-p
                               pawn-king-support-p queen-king-far-p))
               (edge-progress (- 3 (min weak-row weak-col (- 7 weak-row) (- 7 weak-col))))
               (score (if drawish-p
                          (- +queen-vs-pawn-draw-penalty+)
                          (+ 50 (* 10 edge-progress)))))
          (declare (type fixnum pawn-sign promotion-row edge-progress score))
          (the fixnum (* sign (max -700 (min 120 score))))))))

(defun knight-rim-penalty (row col)
  "Return a penalty for knights on edge/near-edge squares."
  (declare (type fixnum row col) (optimize (speed 3) (safety 2)))
  (cond ((or (= row 0) (= row 7) (= col 0) (= col 7))
         +knight-rim-edge-penalty+)
        ((or (= row 1) (= row 6) (= col 1) (= col 6))
         +knight-rim-near-penalty+)
        (t 0)))

(defun castled-p (board color)
  "True when COLOR king is on a castled square."
  (declare (type board board) (type symbol color) (optimize (speed 3) (safety 2)))
  (let ((kp (if (eq color 'white)
                (board-white-king-pos board)
                (board-black-king-pos board))))
    (and (if (eq color 'white)
             (= (pos-row kp) 7)
             (= (pos-row kp) 0))
         (or (= (pos-col kp) 6)
             (= (pos-col kp) 2)))))

(defun developed-minors (board color)
  "Count minor pieces that are no longer on their initial squares."
  (declare (type board board) (type symbol color) (optimize (speed 3) (safety 2)))
  (if (eq color 'white)
      (+ (if (eq (aref (board-board board) 7 1) 'cb) 0 1)
         (if (eq (aref (board-board board) 7 6) 'cb) 0 1)
         (if (eq (aref (board-board board) 7 2) 'ab) 0 1)
         (if (eq (aref (board-board board) 7 5) 'ab) 0 1))
      (+ (if (eq (aref (board-board board) 0 1) 'cn) 0 1)
         (if (eq (aref (board-board board) 0 6) 'cn) 0 1)
         (if (eq (aref (board-board board) 0 2) 'an) 0 1)
         (if (eq (aref (board-board board) 0 5) 'an) 0 1))))

(defun king-wing-pawn-pushes (board color)
  "Count moved g/h pawns for COLOR from the initial files."
  (declare (type board board) (type symbol color) (optimize (speed 3) (safety 2)))
  (if (eq color 'white)
      (+ (if (eq (aref (board-board board) 6 6) 'pb) 0 1)
         (if (eq (aref (board-board board) 6 7) 'pb) 0 1))
      (+ (if (eq (aref (board-board board) 1 6) 'pn) 0 1)
         (if (eq (aref (board-board board) 1 7) 'pn) 0 1))))

(defun center-pawn-score (piece row col)
  "Return a white-perspective bonus for central pawns."
  (declare (type symbol piece) (type fixnum row col) (optimize (speed 3) (safety 2)))
  (if (or (and (= col 3) (or (= row 3) (= row 4)))
          (and (= col 4) (or (= row 3) (= row 4))))
      (cond ((eq piece 'pb) +center-pawn-bonus+)
            ((eq piece 'pn) (- +center-pawn-bonus+))
            (t 0))
      0))

(defun mobility-target-p (board color row col)
  "True if ROW,COL is empty or occupied by the enemy of COLOR."
  (declare (type board board) (type symbol color) (type fixnum row col)
           (optimize (speed 3) (safety 2)))
  (and (in-board-p row col)
       (let ((piece (aref (board-board board) row col)))
         (or (eq piece 'vv)
             (not (eq (piece-color piece) color))))))

(defun count-knight-mobility (board color row col)
  "Return pseudo-legal knight mobility without allocating move lists."
  (declare (type board board) (type symbol color) (type fixnum row col)
           (optimize (speed 3) (safety 2)))
  (let ((count 0))
    (declare (type fixnum count))
    (dolist (offset +knight-deltas+ count)
      (let ((r (+ row (the fixnum (first offset))))
            (c (+ col (the fixnum (second offset)))))
        (declare (type fixnum r c))
        (when (mobility-target-p board color r c)
          (incf count))))))

(defun count-bishop-mobility (board color row col)
  "Return pseudo-legal bishop mobility without allocating move lists."
  (declare (type board board) (type symbol color) (type fixnum row col)
           (optimize (speed 3) (safety 2)))
  (let ((count 0))
    (declare (type fixnum count))
    (dolist (direction +bishop-directions+ count)
      (let ((dr (the fixnum (first direction)))
            (dc (the fixnum (second direction))))
        (declare (type fixnum dr dc))
        (do ((r (+ row dr) (+ r dr))
             (c (+ col dc) (+ c dc)))
            ((not (in-board-p r c)))
          (declare (type fixnum r c))
          (let ((piece (aref (board-board board) r c)))
            (cond ((eq piece 'vv)
                   (incf count))
                  ((not (eq (piece-color piece) color))
                   (incf count)
                   (return))
                  (t
                   (return)))))))))

(defun pseudo-minor-mobility (board row col piece)
  "Return pseudo-legal minor-piece mobility without move-list allocation."
  (declare (type board board) (type fixnum row col) (type symbol piece)
           (optimize (speed 3) (safety 2)))
  (case piece
    (cb (count-knight-mobility board 'white row col))
    (cn (count-knight-mobility board 'black row col))
    (ab (count-bishop-mobility board 'white row col))
    (an (count-bishop-mobility board 'black row col))
    (t 0)))

(defun bishop-development-penalty (board color row col)
  "Penalty for bishops developed to low-scope edge squares or blocking own center pawns."
  (declare (type board board) (type symbol color) (type fixnum row col)
           (optimize (speed 3) (safety 2)))
  (let ((penalty 0)
        (mobility (pseudo-minor-mobility board row col
                                         (aref (board-board board) row col))))
    (declare (type fixnum penalty mobility))
    (when (or (= col 0) (= col 7))
      (incf penalty +edge-bishop-penalty+))
    (when (<= mobility 2)
      (incf penalty +low-bishop-mobility-penalty+))
    ;; Generic development principle: an early bishop on the same file directly
    ;; in front of an unmoved central pawn obstructs central pawn play.
    (when (and (eq color 'white)
               (= row 5)
               (or (and (= col 3) (eq (aref (board-board board) 6 3) 'pb))
                   (and (= col 4) (eq (aref (board-board board) 6 4) 'pb))))
      (incf penalty +blocked-central-pawn-penalty+))
    (when (and (eq color 'black)
               (= row 2)
               (or (and (= col 3) (eq (aref (board-board board) 1 3) 'pn))
                   (and (= col 4) (eq (aref (board-board board) 1 4) 'pn))))
      (incf penalty +blocked-central-pawn-penalty+))
    penalty))

(defun minor-advance-depth (color row)
  "Ranks a minor of COLOR has advanced past the middle into the enemy half.
0 or less means the piece is still in its own half (rank 4 or nearer home)."
  (declare (type symbol color) (type fixnum row) (optimize (speed 3) (safety 2)))
  ;; White's enemy half is rows 0-3 (ranks 8-5); Black's is rows 4-7 (ranks 1-4).
  (if (eq color 'white) (- 4 row) (- row 3)))

(defun pawn-push-can-attack-p (board row col enemy-color)
  "True when an ENEMY-COLOR pawn can reach, by a single or initial double push, a
square from which it attacks (ROW,COL) -- i.e. the piece there is kickable now."
  (declare (type board board) (type fixnum row col) (type symbol enemy-color)
           (optimize (speed 3) (safety 2)))
  (let* ((edir (pawn-attack-direction enemy-color))
         (attack-row (- row edir))
         (pawn (if (eq enemy-color 'white) 'pb 'pn))
         (home-row (if (eq enemy-color 'white) 6 1))
         (grid (board-board board)))
    (declare (type fixnum edir attack-row home-row))
    (dolist (dc '(-1 1) nil)
      (let ((f (+ col (the fixnum dc)))
            (one (- (- row edir) edir)))       ; one square behind the landing rank
        (declare (type fixnum f one))
        (when (and (in-board-p attack-row f)
                   (eq (aref grid attack-row f) 'vv))   ; pawn can land here
          (when (and (in-board-p one f) (eq (aref grid one f) pawn))
            (return-from pawn-push-can-attack-p t))
          (let ((two (- attack-row (the fixnum (* 2 edir)))))
            (declare (type fixnum two))
            (when (and (= two home-row) (in-board-p two f)
                       (eq (aref grid two f) pawn)
                       (eq (aref grid one f) 'vv))       ; double push, path clear
              (return-from pawn-push-can-attack-p t))))))))

(defun pawn-can-ever-attack-p (board row col enemy-color)
  "True when ENEMY-COLOR has a pawn on an adjacent file that could, by advancing,
ever reach a square attacking (ROW,COL).  Used to reject non-outpost squares."
  (declare (type board board) (type fixnum row col) (type symbol enemy-color)
           (optimize (speed 3) (safety 2)))
  (let* ((edir (pawn-attack-direction enemy-color))
         (attack-row (- row edir))
         (pawn (if (eq enemy-color 'white) 'pb 'pn))
         (grid (board-board board)))
    (declare (type fixnum edir attack-row))
    (dolist (dc '(-1 1) nil)
      (let ((f (+ col (the fixnum dc))))
        (declare (type fixnum f))
        (do ((r attack-row (- r edir)))                 ; scan back toward enemy home
            ((not (in-board-p r f)))
          (declare (type fixnum r))
          (let ((p (aref grid r f)))
            (cond ((eq p pawn) (return-from pawn-can-ever-attack-p t))
                  ((not (eq p 'vv)) (return)))))))))     ; blocked by another piece

(defun minor-safety-score (board color row col knightp)
  "White-perspective-magnitude adjustment (may be negative) for a minor of COLOR
on (ROW,COL): penalise a loose or kickable advance into the enemy half; reward a
knight sitting on a true, pawn-supported outpost."
  (declare (type board board) (type symbol color) (type fixnum row col)
           (optimize (speed 3) (safety 2)))
  (let ((adv (minor-advance-depth color row)))
    (declare (type fixnum adv))
    (if (<= adv 0)
        0
        (let* ((enemy (invert-color color))
               (guarded (attacked-by-pawn-p board row col color)))
          (cond
            ;; Already attacked by an enemy pawn and undefended: a loose piece.
            ((and (attacked-by-pawn-p board row col enemy) (not guarded))
             (- (+ +minor-loose-base+ (the fixnum (* adv +minor-advance-step+)))))
            ;; Attacked by a pawn but pawn-defended: harassed, minor penalty.
            ((attacked-by-pawn-p board row col enemy)
             (- +minor-attacked-defended-penalty+))
            ;; Pawn-supported knight no pawn can ever attack: a genuine outpost.
            ((and knightp guarded
                  (not (pawn-can-ever-attack-p board row col enemy)))
             +knight-outpost-bonus+)
            ;; Unsupported and a pawn push can chase it off: premature advance.
            ((and (not guarded) (pawn-push-can-attack-p board row col enemy))
             (- (+ +minor-unstable-base+ (the fixnum (* adv +minor-advance-step+)))))
            (t 0))))))

(defun minor-positional-score (board piece row col base-score)
  "Return a white-perspective minor-piece score with mobility, development and
piece-safety terms."
  (declare (type board board) (type symbol piece) (type fixnum row col base-score)
           (optimize (speed 3) (safety 2)))
  (let ((mobility-bonus (* +minor-mobility-weight+
                           (pseudo-minor-mobility board row col piece))))
    (declare (type fixnum mobility-bonus))
    (case piece
      ((cb cn)
       (let* ((color (if (eq piece 'cb) 'white 'black))
              (white-score (+ base-score mobility-bonus
                              (- (knight-rim-penalty row col))
                              (minor-safety-score board color row col t))))
         (if (eq piece 'cb) white-score (- white-score))))
      ((ab an)
       (let* ((color (if (eq piece 'ab) 'white 'black))
              (white-score (+ (- (+ base-score mobility-bonus)
                                 (bishop-development-penalty board color row col))
                              (minor-safety-score board color row col nil))))
         (if (eq piece 'ab) white-score (- white-score))))
      (t 0))))

(defun fev (board)
  (declare (type board board) (optimize (speed 3) (safety 2)))
  (let ((fev 0))
    (declare (type fixnum fev))
    ;; UCI positions are always standard orientation (white at the bottom,
    ;; row 7; black at the top, row 0) -- the side-to-move field only
    ;; determines who moves, never which rows belong to which color.
    (dotimes (i 8)
      (dotimes (j 8)
        (let ((piece (aref (board-board board) i j)))
          (incf fev (the fixnum (center-pawn-score piece i j)))
          (cond ((eq piece 'pb)
                 (incf fev (the fixnum
                                (+ +pawn-value+
                                   (the fixnum (aref +pawn-pos-v-weigh+ (- 7 i)))
                                   (the fixnum (aref +pawn-pos-h-weigh+ j))))))
                ((eq piece 'pn)
                 (decf fev (the fixnum
                                (+ +pawn-value+
                                   (the fixnum (aref +pawn-pos-v-weigh+ i))
                                   (the fixnum (aref +pawn-pos-h-weigh+ j))))))
                ((eq piece 'tb)
                 (incf fev (the fixnum (+ +rook-value+
                                          (the fixnum (aref +rook-pos-weigh+ i))
                                          (the fixnum (aref +rook-pos-weigh+ j))))))
                ((eq piece 'tn)
                 (decf fev (the fixnum (+ +rook-value+
                                          (the fixnum (aref +rook-pos-weigh+ i))
                                          (the fixnum (aref +rook-pos-weigh+ j))))))
                ((or (eq piece 'ab) (eq piece 'an)
                     (eq piece 'cb) (eq piece 'cn))
                 (incf fev
                       (the fixnum
                            (minor-positional-score
                             board piece i j
                             (the fixnum (+ (if (or (eq piece 'cb) (eq piece 'cn))
                                                +knight-value+
                                                +bishop-value+)
                                            (the fixnum (aref +knight-bishop-pos-weigh+ i))
                                            (the fixnum (aref +knight-bishop-pos-weigh+ j))))))))
                ((eq piece 'rb) (incf fev +king-value+))
                ((eq piece 'rn) (decf fev +king-value+))
                ((eq piece 'db) (incf fev +queen-value+))
                ((eq piece 'dn) (decf fev +queen-value+))))))
    (let* ((white-developed (developed-minors board 'white))
           (black-developed (developed-minors board 'black))
           (white-castled (castled-p board 'white))
           (black-castled (castled-p board 'black))
           (white-risk (if white-castled
                           0
                           (+ (* +uncastled-king-penalty+
                                 (max 0 (- 4 white-developed)))
                              (* +wing-pawn-push-penalty+
                                 (king-wing-pawn-pushes board 'white)))))
           (black-risk (if black-castled
                           0
                           (+ (* +uncastled-king-penalty+
                                 (max 0 (- 4 black-developed)))
                              (* +wing-pawn-push-penalty+
                                 (king-wing-pawn-pushes board 'black))))))
      (declare (type fixnum white-developed black-developed white-risk black-risk))
      (incf fev (the fixnum (* +development-weight+
                              (- white-developed black-developed))))
      (when white-castled
        (incf fev +castled-bonus+))
      (when black-castled
        (decf fev +castled-bonus+))
      (decf fev white-risk)
      (incf fev black-risk))
    (incf fev (the fixnum (kbnk-endgame-bonus board)))
    (incf fev (the fixnum (elementary-lone-king-endgame-bonus board)))
    (incf fev (the fixnum (minor-pawn-vs-minor-endgame-bonus board)))
    (incf fev (the fixnum (major-minor-endgame-bonus board)))
    (incf fev (the fixnum (queen-vs-pawn-endgame-bonus board)))
    fev))
