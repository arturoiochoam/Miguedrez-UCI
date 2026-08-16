;;; -*- encoding: utf-8 -*-
;;;
;;; data.lisp
;;; Data definitions, manipulation functions and FEN parsing.
;;; gamallo, February 11, 2007
;;; UCI port and cleanup by Arthur Matheus, 2026.
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


;;;; Main data structures

(defstruct board
  (board (make-array '(8 8) :initial-element 'vv) :type (simple-array t (8 8)))
  (whites (make-array '(8 8) :initial-element '0) :type (simple-array t (8 8)))
  (blacks (make-array '(8 8) :initial-element '0) :type (simple-array t (8 8)))
  (whites-enpass (make-array 8 :initial-element '0) :type (simple-array t (8)))
  (blacks-enpass (make-array 8 :initial-element '0) :type (simple-array t (8)))
  (side-to-move 'white)
  (halfmove-clock 0 :type (integer 0 *))
  (fullmove-number 1 :type (integer 1 *))
  (position-history nil)
  (white-king-pos (make-pos))
  (black-king-pos (make-pos))
  (white-king-moved nil)
  (black-king-moved nil)
  ;; Per-side rook movement flags; required for correct castling state.
  (white-kingside-rook-moved nil)
  (white-queenside-rook-moved nil)
  (black-kingside-rook-moved nil)
  (black-queenside-rook-moved nil)
  (key 0 :type (unsigned-byte 64)))


;;;; Zobrist hashing

(declaim (type (unsigned-byte 64) *zobrist-seed* *zobrist-side*))
(declaim (type (simple-array (unsigned-byte 64) (64 12)) *zobrist-pieces*))
(declaim (type (simple-array (unsigned-byte 64) (4)) *zobrist-castling*))
(declaim (type (simple-array (unsigned-byte 64) (8)) *zobrist-enpassant*))

(defvar *zobrist-seed* 88172645463325252)

(defun zobrist-random ()
  "Return a deterministic 64-bit pseudo-random integer (xorshift64*)."
  (declare (optimize (speed 3) (safety 2)))
  (let ((x *zobrist-seed*)
        (mask (1- (ash 1 64))))
    (declare (type (unsigned-byte 64) x mask))
    (setf x (logand (logxor x (ash x -12)) mask))
    (setf x (logand (logxor x (ash x 25)) mask))
    (setf x (logand (logxor x (ash x -27)) mask))
    (setf *zobrist-seed* x)
    (the (unsigned-byte 64)
         (logand (* x 2685821657736338717) mask))))

(defvar *zobrist-pieces*
  (load-time-value (make-array '(64 12) :element-type '(unsigned-byte 64) :initial-element 0) t))

(defvar *zobrist-side*
  (load-time-value (zobrist-random) t))

(defvar *zobrist-castling*
  (load-time-value (make-array 4 :element-type '(unsigned-byte 64) :initial-element 0) t))

(defvar *zobrist-enpassant*
  (load-time-value (make-array 8 :element-type '(unsigned-byte 64) :initial-element 0) t))

(defun init-zobrist-tables ()
  "Fill the Zobrist tables with deterministic 64-bit values."
    (declare (optimize (speed 3) (safety 2)))
(dotimes (sq 64)
    (dotimes (piece 12)
      (setf (aref *zobrist-pieces* sq piece) (zobrist-random))))
  (dotimes (i 4)
    (setf (aref *zobrist-castling* i) (zobrist-random)))
  (dotimes (i 8)
    (setf (aref *zobrist-enpassant* i) (zobrist-random))))

(init-zobrist-tables)

(defun piece-zobrist-index (piece)
  "Return the Zobrist piece index for PIECE, or NIL for empty."
  (declare (type symbol piece) (optimize (speed 3) (safety 2)))
  (case piece
    (pb 0) (pn 1)
    (cb 2) (cn 3)
    (ab 4) (an 5)
    (tb 6) (tn 7)
    (db 8) (dn 9)
    (rb 10) (rn 11)
    (t nil)))

(declaim (inline castling-right-available-p))
(defun castling-right-available-p (king-moved rook-moved)
  "Return true if a castling right is available given the two flags."
  (declare (optimize (speed 3) (safety 2)))
  (and (not king-moved) (not rook-moved)))

(defun compute-board-key (board)
  "Compute the Zobrist key for BOARD from scratch."
  (declare (type board board) (optimize (speed 3) (safety 2)))
  (let ((key 0))
    (declare (type (unsigned-byte 64) key))
    ;; Piece placement.
    (dotimes (row 8)
      (dotimes (col 8)
        (let* ((piece (aref (board-board board) row col))
               (idx (piece-zobrist-index piece))
               (sq (+ (* row 8) col)))
          (declare (type fixnum sq))
          (when idx
            (setf key (logxor key (aref *zobrist-pieces* sq idx)))))))
    ;; Side to move.
    (when (eq (board-side-to-move board) 'black)
      (setf key (logxor key *zobrist-side*)))
    ;; Castling rights.
    (when (castling-right-available-p (board-white-king-moved board)
                                      (board-white-kingside-rook-moved board))
      (setf key (logxor key (aref *zobrist-castling* 0))))
    (when (castling-right-available-p (board-white-king-moved board)
                                      (board-white-queenside-rook-moved board))
      (setf key (logxor key (aref *zobrist-castling* 1))))
    (when (castling-right-available-p (board-black-king-moved board)
                                      (board-black-kingside-rook-moved board))
      (setf key (logxor key (aref *zobrist-castling* 2))))
    (when (castling-right-available-p (board-black-king-moved board)
                                      (board-black-queenside-rook-moved board))
      (setf key (logxor key (aref *zobrist-castling* 3))))
    ;; En passant files.
    (dotimes (col 8)
      (when (eq (aref (board-whites-enpass board) col) '1)
        (setf key (logxor key (aref *zobrist-enpassant* col))))
      (when (eq (aref (board-blacks-enpass board) col) '1)
        (setf key (logxor key (aref *zobrist-enpassant* col)))))
    key))

;;;; Undo record for make / unmake move

(defstruct move-undo
  "All state changed by a single move, sufficient to undo it destructively."
  (from-piece 'vv :type symbol)
  (captured 'vv :type symbol)
  (ep-captured-pos nil)
  (castled nil)
  (rook-from (make-pos))
  (rook-to (make-pos))
  (rook-piece 'vv :type symbol)
  (whites-enpass (make-array 8 :initial-element '0) :type (simple-array t (8)))
  (blacks-enpass (make-array 8 :initial-element '0) :type (simple-array t (8)))
  (white-king-pos (make-pos))
  (black-king-pos (make-pos))
  (white-king-moved nil)
  (black-king-moved nil)
  (white-kingside-rook-moved nil)
  (white-queenside-rook-moved nil)
  (black-kingside-rook-moved nil)
  (black-queenside-rook-moved nil)
  (halfmove-clock 0 :type (integer 0 *))
  (fullmove-number 1 :type (integer 1 *))
  (side-to-move 'white :type symbol)
  (key 0 :type (unsigned-byte 64)))


(defstruct null-undo
  "State changed by a null move (a pass), sufficient to undo it destructively.
   A null move flips only the side to move, clears both en-passant arrays, and
   advances the halfmove clock; it does NOT touch piece placement, castling
   flags, king positions, fullmove number, or position-history.  The Zobrist
   key is saved verbatim and restored on unmake."
  (whites-enpass (make-array 8 :initial-element '0) :type (simple-array t (8)))
  (blacks-enpass (make-array 8 :initial-element '0) :type (simple-array t (8)))
  (halfmove-clock 0 :type (integer 0 *))
  (side-to-move 'white :type symbol)
  (key 0 :type (unsigned-byte 64)))


(defstruct player
  (type 'manual) ; 'auto or 'manual
  (color 'white))


(defstruct pos
  (row 0 :type (integer 0 7))
  (col 0 :type (integer 0 7)))


(defstruct move
  (from (make-pos) :type pos)
  (to (make-pos) :type pos)
  (promotion nil)) ; 'q, 'r, 'b, 'n or nil


;;;; Conditions

(define-condition invalid-fen (error)
  ((fen :initarg :fen :reader invalid-fen-string))
  (:report (lambda (c s)
             (format s "Invalid FEN: ~A" (invalid-fen-string c)))))

(define-condition invalid-move (error)
  ((move :initarg :move :reader invalid-move-string))
  (:report (lambda (c s)
             (format s "Invalid move: ~A" (invalid-move-string c)))))


;;;; Position / move utilities

(defun equal-pos (pos1 pos2)
  "True if POS1 and POS2 have the same row and column."
  (declare (optimize (speed 3) (safety 2)))
  (and (= (pos-row pos1) (pos-row pos2))
       (= (pos-col pos1) (pos-col pos2))))


(defmacro add-move (move-list from-pos row col &optional promotion)
  "Push a move from FROM-POS to (ROW,COL) onto MOVE-LIST."
  `(push (make-move :from ,from-pos :to (make-pos :row ,row :col ,col)
                   :promotion ,promotion)
         ,move-list))

(defmacro add-promotion-moves (move-list from-pos row col)
  "Push all UCI promotion choices for a pawn reaching (ROW,COL).

FROM-POS/ROW/COL are each spliced into four ADD-MOVE calls below, so their
value is bound once here (once-only discipline) -- otherwise a
side-effecting or expensive argument form would run four times instead of
once. MOVE-LIST is left as-is (spliced as the literal place ADD-MOVE pushes
onto), not bound to a value, since every call site passes a plain variable."
  (let ((from-pos-var (gensym "FROM-POS"))
        (row-var (gensym "ROW"))
        (col-var (gensym "COL")))
    `(let ((,from-pos-var ,from-pos)
           (,row-var ,row)
           (,col-var ,col))
       (add-move ,move-list ,from-pos-var ,row-var ,col-var 'q)
       (add-move ,move-list ,from-pos-var ,row-var ,col-var 'r)
       (add-move ,move-list ,from-pos-var ,row-var ,col-var 'b)
       (add-move ,move-list ,from-pos-var ,row-var ,col-var 'n))))


(defun equal-moves (mov1 mov2)
  "True if MOV1 and MOV2 have the same origin, target, and promotion."
  (declare (optimize (speed 3) (safety 2)))
  (and (equal-pos (move-from mov1) (move-from mov2))
       (equal-pos (move-to mov1) (move-to mov2))
       (eq (move-promotion mov1) (move-promotion mov2))))


(declaim (inline pos-square-index move-origin-index move-target-index))

(defun pos-square-index (pos)
  "Return POS as a 0..63 square index."
  (declare (type pos pos) (optimize (speed 3) (safety 2)))
  (+ (ash (pos-row pos) 3) (pos-col pos)))

(defun move-origin-index (move)
  "Return MOVE's origin square as a 0..63 index."
  (declare (type move move) (optimize (speed 3) (safety 2)))
  (pos-square-index (move-from move)))

(defun move-target-index (move)
  "Return MOVE's target square as a 0..63 index."
  (declare (type move move) (optimize (speed 3) (safety 2)))
  (pos-square-index (move-to move)))


(declaim (inline board-pos (setf board-pos)))

(defun board-pos (board pos)
  "Return the square in BOARD addressed by POS, evaluating POS once."
  (declare (type (simple-array t (8 8)) board) (type pos pos)
           (optimize (speed 3) (safety 2)))
  (aref board (pos-row pos) (pos-col pos)))

(defun (setf board-pos) (value board pos)
  "Store VALUE in BOARD at POS, evaluating POS once."
  (declare (type (simple-array t (8 8)) board) (type pos pos)
           (optimize (speed 3) (safety 2)))
  (setf (aref board (pos-row pos) (pos-col pos)) value))


;;;; Board copying

(defun duplicate-board-array (board-array)
    (declare (optimize (speed 3) (safety 2)))
(let ((new-board-array (make-array '(8 8))))
    (dotimes (row 8)
      (dotimes (col 8)
        (setf (aref new-board-array row col)
              (aref board-array row col))))
    new-board-array))


(defun duplicate-board-row (row-array)
    (declare (optimize (speed 3) (safety 2)))
(copy-seq row-array))


(defun duplicate-board (board)
    (declare (optimize (speed 3) (safety 2)))
(make-board
   ;; Carry the parent's Zobrist key.  Omitting this left the copy with the
   ;; struct default (KEY 0), so a duplicated board silently lost its hash
   ;; identity: any subsequent incremental update (EXECUTE-MOVE) diverged from
   ;; COMPUTE-BOARD-KEY, and every TT probe/store and repetition check on the
   ;; copy used a wrong key.  Copying the slot is cheaper than recomputing it
   ;; with COMPUTE-BOARD-KEY and is always correct because the parent's key is
   ;; an invariant maintained incrementally.
   :key (board-key board)
   :board (duplicate-board-array (board-board board))
   :whites (duplicate-board-array (board-whites board))
   :blacks (duplicate-board-array (board-blacks board))
   :whites-enpass (duplicate-board-row (board-whites-enpass board))
   :blacks-enpass (duplicate-board-row (board-blacks-enpass board))
   :side-to-move (board-side-to-move board)
   :halfmove-clock (board-halfmove-clock board)
   :fullmove-number (board-fullmove-number board)
   :position-history (board-position-history board)
   :white-king-pos (make-pos :row (pos-row (board-white-king-pos board))
                             :col (pos-col (board-white-king-pos board)))
   :black-king-pos (make-pos :row (pos-row (board-black-king-pos board))
                             :col (pos-col (board-black-king-pos board)))
   :white-king-moved (board-white-king-moved board)
   :black-king-moved (board-black-king-moved board)
   :white-kingside-rook-moved (board-white-kingside-rook-moved board)
   :white-queenside-rook-moved (board-white-queenside-rook-moved board)
   :black-kingside-rook-moved (board-black-kingside-rook-moved board)
   :black-queenside-rook-moved (board-black-queenside-rook-moved board)))


;;;; Color and occupancy helpers

(defun invert-color (color)
    (declare (optimize (speed 3) (safety 2)))
(if (eq color 'white) 'black 'white))

(defun whitep (color)
    (declare (optimize (speed 3) (safety 2)))
(eq color 'white))

(defun color-occupied-p (board-color row col)
    (declare (optimize (speed 3) (safety 2)))
(eq (aref board-color row col) '1))


;;;; Portable string utilities

(defun split-string (string &optional (separator #\Space))
  "Split STRING into a list of substrings separated by SEPARATOR."
    (declare (optimize (speed 3) (safety 2)))
(let ((result '())
        (start 0)
        (len (length string)))
    (dotimes (i (1+ len) (nreverse result))
      (when (or (= i len) (char= (aref string i) separator))
        (when (> i start)
          (push (subseq string start i) result))
        (setf start (1+ i))))))


;;;; FEN parsing
;;;
;;; Standard starting position:
;;;   rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1
;;;
;;; Internal coordinates use row 0..7 from top (rank 8) to bottom (rank 1)
;;; and col 0..7 from a to h.  White pieces at the bottom is the default
;;; orientation used by UCI.

(defparameter +standard-start-position-fen+
  "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")

(defvar *fen-piece-map*
  '((#\r . tn) (#\n . cn) (#\b . an) (#\q . dn) (#\k . rn) (#\p . pn)
    (#\R . tb) (#\N . cb) (#\B . ab) (#\Q . db) (#\K . rb) (#\P . pb)))


(defun initial-board ()
  "Return the standard chess starting position as a board."
    (declare (optimize (speed 3) (safety 2)))
(parse-fen +standard-start-position-fen+))


(defun parse-fen (fen)
  "Parse a FEN string and return a fresh board struct."
    (declare (optimize (speed 3) (safety 2)))
(let* ((parts (split-string (string-trim " " fen)))
         (board (make-empty-board)))
    (unless (= (length parts) 6)
      (error 'invalid-fen :fen fen))
    (parse-fen-placement board (first parts))
    (parse-fen-side-to-move board (second parts))
    (parse-fen-castling board (third parts))
    (parse-fen-enpassant board (fourth parts))
    (parse-fen-clock board (fifth parts))
    (parse-fen-fullmove board (sixth parts))
    (locate-kings board)
    (validate-fen-board board fen)
    (normalize-enpassant-rights board)
    (setf (board-key board) (compute-board-key board))
    board))


(defun make-empty-board ()
    (declare (optimize (speed 3) (safety 2)))
(make-board :board (make-array '(8 8) :initial-element 'vv)
              :whites (make-array '(8 8) :initial-element '0)
              :blacks (make-array '(8 8) :initial-element '0)
              :whites-enpass (make-array 8 :initial-element '0)
              :blacks-enpass (make-array 8 :initial-element '0)
              :side-to-move 'white
              :halfmove-clock 0
              :fullmove-number 1
              :position-history nil
              :key 0))


(defun parse-fen-placement (board placement)
    (declare (optimize (speed 3) (safety 2)))
(when (or (zerop (length placement))
            (char= (aref placement 0) #\/)
            (char= (aref placement (1- (length placement))) #\/)
            (search "//" placement))
    (error 'invalid-fen :fen placement))
  (let ((rows (split-string placement #\/)))
    (unless (= (length rows) 8)
      (error 'invalid-fen :fen placement))
    (let ((rank 8))
      (dolist (row-str rows)
        (parse-fen-row board row-str rank)
        (decf rank)))))


(defun parse-fen-row (board row-str rank)
    (declare (optimize (speed 3) (safety 2)))
(let ((col 0))
    (dotimes (i (length row-str))
      (let ((ch (aref row-str i)))
        (cond ((digit-char-p ch)
               (let ((empty-count (digit-char-p ch)))
                 (unless (<= 1 empty-count 8)
                   (error 'invalid-fen :fen (format nil "bad empty count ~A" ch)))
                 (incf col empty-count)
                 (when (> col 8)
                   (error 'invalid-fen :fen (format nil "row overflow ~A" row-str)))))
              (t
               (when (>= col 8)
                 (error 'invalid-fen :fen (format nil "row overflow ~A" row-str)))
               (let ((piece (cdr (assoc ch *fen-piece-map*))))
                 (unless piece
                   (error 'invalid-fen :fen (format nil "bad piece ~A" ch)))
                 (set-piece-at board (convert-rank rank) col piece)
                 (incf col))))))
    (unless (= col 8)
      (error 'invalid-fen :fen (format nil "row length ~A" row-str)))))



(defun convert-rank (rank)
  "Convert FEN rank (1..8) to internal row (0..7)."
    (declare (optimize (speed 3) (safety 2)))
(- 8 rank))


(defun convert-file (file)
  "Convert FEN file character (?a..?h) to internal col (0..7)."
    (declare (optimize (speed 3) (safety 2)))
(- (char-code (char-downcase file)) (char-code #\a)))


(defun set-piece-at (board row col piece)
    (declare (optimize (speed 3) (safety 2)))
(setf (aref (board-board board) row col) piece)
  (if (white-piece-p piece)
      (setf (aref (board-whites board) row col) '1)
      (setf (aref (board-blacks board) row col) '1)))


(defun white-piece-p (piece)
    (declare (optimize (speed 3) (safety 2)))
(member piece '(pb cb ab tb db rb)))


(defun black-piece-p (piece)
    (declare (optimize (speed 3) (safety 2)))
(member piece '(pn cn an tn dn rn)))


(defun piece-color (piece)
  "Return 'white, 'black or nil for an empty square."
    (declare (optimize (speed 3) (safety 2)))
(cond ((white-piece-p piece) 'white)
        ((black-piece-p piece) 'black)
        (t nil)))


(defun pawnp (piece)
    (declare (optimize (speed 3) (safety 2)))
(or (eq piece 'pb) (eq piece 'pn)))


(defun parse-fen-castling (board castling)
    (declare (optimize (speed 3) (safety 2)))
(unless (or (string= castling "-")
              (and (plusp (length castling))
                   (loop for ch across castling always (find ch "KQkq"))
                   (= (length castling)
                      (length (remove-duplicates castling :test #'char=)))))
    (error 'invalid-fen :fen castling))
  (setf (board-white-king-moved board)
        (not (or (find #\K castling) (find #\Q castling))))
  (setf (board-black-king-moved board)
        (not (or (find #\k castling) (find #\q castling))))
  (setf (board-white-kingside-rook-moved board)
        (not (find #\K castling)))
  (setf (board-white-queenside-rook-moved board)
        (not (find #\Q castling)))
  (setf (board-black-kingside-rook-moved board)
        (not (find #\k castling)))
  (setf (board-black-queenside-rook-moved board)
        (not (find #\q castling))))


(defun parse-fen-side-to-move (board side)
    (declare (optimize (speed 3) (safety 2)))
(setf (board-side-to-move board)
        (cond ((string-equal side "w") 'white)
              ((string-equal side "b") 'black)
              (t (error 'invalid-fen :fen side)))))


(defun parse-fen-nonnegative-integer (token &optional max-value)
  "Parse TOKEN as a complete non-negative decimal integer.
When MAX-VALUE is non-NIL, also require TOKEN not to exceed it."
    (declare (optimize (speed 3) (safety 2)))
(multiple-value-bind (value end)
      (parse-integer token :junk-allowed t)
    (unless (and value
                 (= end (length token))
                 (>= value 0)
                 (or (null max-value) (<= value max-value)))
      (error 'invalid-fen :fen token))
    value))


(defun parse-fen-clock (board clock)
  "Parse the FEN halfmove clock.  The board slot is unbounded, so any complete
non-negative decimal integer is accepted."
    (declare (optimize (speed 3) (safety 2)))
(setf (board-halfmove-clock board)
        (if clock
            (parse-fen-nonnegative-integer clock)
            0)))


(defun parse-fen-fullmove (board fullmove)
  "Parse the FEN fullmove number.  It must be a complete decimal integer >= 1."
    (declare (optimize (speed 3) (safety 2)))
(setf (board-fullmove-number board)
        (if fullmove
            (let ((value (parse-fen-nonnegative-integer fullmove)))
              (if (>= value 1)
                  value
                  (error 'invalid-fen :fen fullmove)))
            1)))


(defun parse-fen-enpassant (board ep)
    (declare (optimize (speed 3) (safety 2)))
(unless (string= ep "-")
    (unless (and (= (length ep) 2)
                 (char<= #\a (char-downcase (aref ep 0)) #\h)
                 (digit-char-p (aref ep 1)))
      (error 'invalid-fen :fen ep))
    (let* ((file (aref ep 0))
           (rank-digit (aref ep 1))
           (col (convert-file file))
           (rank (digit-char-p rank-digit)))
      (cond
        ;; White pawn just moved two squares: target rank is 3 (row 5)
        ((= rank 3)
         (setf (aref (board-whites-enpass board) col) '1))
        ;; Black pawn just moved two squares: target rank is 6 (row 2)
        ((= rank 6)
         (setf (aref (board-blacks-enpass board) col) '1))
        (t
         (error 'invalid-fen :fen ep))))))

(defun ep-capture-legal-p (board capturer-row capturer-col
                           target-row target-col captured-row captured-col
                           capturer-piece king-color)
  "True if an en-passant capture by CAPTURER-PIECE from (CAPTURER-ROW,
CAPTURER-COL) onto (TARGET-ROW, TARGET-COL) -- removing the pawn at
(CAPTURED-ROW, CAPTURED-COL) -- would leave KING-COLOR's own king not in check.

Pure board scan: it temporarily vacates the capturer's source square and the
captured pawn's square, places the capturer on the e.p. target, probes the king
with SQUARE-ATTACKED-P, and restores all three squares before returning.  No
make/unmake and no scratch undo record is touched, and SQUARE-ATTACKED-P reads
only the piece array (never the occupancy bitboards), so this is safe to call
from inside EXECUTE-MOVE -- including the KING-VALID simulation path -- without
disturbing shared state or recursing through execute-move.  That is what lets
the e.p. flag be made legality-aware on the make path itself, including the
post-double-push flag set in MAYBE-SET-ENPASSANT-RIGHT.

This catches every way a pseudo-legal e.p. capture can be illegal: the classic
horizontal pin (capturer and captured pawn share a rank with the capturer's king
and an enemy slider behind them, so removing both pawns exposes the king) and
diagonal pins whose ray the capture stays on."
  (declare (type board board)
           (type fixnum capturer-row capturer-col target-row target-col
                       captured-row captured-col)
           (type symbol capturer-piece king-color)
           (optimize (speed 3) (safety 2)))
  (let* ((a-board (board-board board))
         (attacker (invert-color king-color))
         (king-pos (if (eq king-color 'white)
                       (board-white-king-pos board)
                       (board-black-king-pos board))))
    (let ((save-from (aref a-board capturer-row capturer-col))
          (save-cap (aref a-board captured-row captured-col))
          (save-tgt (aref a-board target-row target-col)))
      (setf (aref a-board capturer-row capturer-col) 'vv)
      (setf (aref a-board captured-row captured-col) 'vv)
      (setf (aref a-board target-row target-col) capturer-piece)
      (let ((safe (not (square-attacked-p board (pos-row king-pos)
                                          (pos-col king-pos) attacker))))
        (setf (aref a-board capturer-row capturer-col) save-from)
        (setf (aref a-board captured-row captured-col) save-cap)
        (setf (aref a-board target-row target-col) save-tgt)
        safe))))

(defun enpassant-right-capturable-p (board pawn-color col)
  "True if the en-passant right on COL can produce at least one LEGAL en-passant
capture by the side to move.  PAWN-COLOR is the colour of the pawn that just
advanced two squares.

A mere adjacent enemy pawn is not enough: that pawn may be pinned -- the
classic horizontal-pin case where the capturer and the captured pawn share a
rank with the capturer's king and an enemy slider, or a diagonal pin whose ray
the capture stays on -- so the e.p. move is pseudo-legal but illegal.  Folding
such a non-capturable right into the Zobrist key would make an otherwise
identical position (reached without the fresh double push) hash differently,
under-counting repetitions and missing TT transpositions.  The right is
retained only when some adjacent pawn's e.p. capture is actually legal,
verified by EP-CAPTURE-LEGAL-P."
    (declare (optimize (speed 3) (safety 2)))
(flet ((adjacent-legal-p (adj-col capturer-piece target-row captured-row
                         king-color)
         (declare (type fixnum adj-col target-row captured-row)
                  (type symbol capturer-piece king-color))
         (and (>= adj-col 0) (<= adj-col 7)
              (eq (aref (board-board board) captured-row adj-col) capturer-piece)
              (ep-capture-legal-p board captured-row adj-col
                                  target-row col captured-row col
                                  capturer-piece king-color))))
  (case pawn-color
    (white
     (and (eq (board-side-to-move board) 'black)
          (eq (aref (board-board board) 4 col) 'pb)
          (eq (aref (board-board board) 5 col) 'vv)
          (or (adjacent-legal-p (1- col) 'pn 5 4 'black)
              (adjacent-legal-p (1+ col) 'pn 5 4 'black))))
    (black
     (and (eq (board-side-to-move board) 'white)
          (eq (aref (board-board board) 3 col) 'pn)
          (eq (aref (board-board board) 2 col) 'vv)
          (or (adjacent-legal-p (1- col) 'pb 2 3 'white)
              (adjacent-legal-p (1+ col) 'pb 2 3 'white))))
    (t nil))))

(defun normalize-enpassant-rights (board)
  "Clear en-passant flags that cannot produce a legal en-passant candidate.

Some FEN producers record an en-passant target after every double pawn push,
even when the opponent has no adjacent pawn.  Keeping that non-capturable flag
would make otherwise identical positions hash differently; worse, an impossible
flag with no pawn behind it could make move generation fabricate an en-passant
capture.  Canonicalize the board to only retain capturable rights."
    (declare (optimize (speed 3) (safety 2)))
(dotimes (col 8 board)
    (unless (and (eq (aref (board-whites-enpass board) col) '1)
                 (enpassant-right-capturable-p board 'white col))
      (setf (aref (board-whites-enpass board) col) '0))
    (unless (and (eq (aref (board-blacks-enpass board) col) '1)
                 (enpassant-right-capturable-p board 'black col))
      (setf (aref (board-blacks-enpass board) col) '0))))


(defun locate-kings (board)
  "Locate both kings on BOARD. VALIDATE-FEN-BOARD (called right after this,
unconditionally, by PARSE-FEN) is what actually rejects a missing/duplicate
king -- BOARD-WHITE-KING-POS/BOARD-BLACK-KING-POS default to a non-NIL
MAKE-POS, so a presence check here could never fire."
    (declare (optimize (speed 3) (safety 2)))
(dotimes (row 8)
    (dotimes (col 8)
      (let ((piece (aref (board-board board) row col)))
        (cond ((eq piece 'rb)
               (setf (board-white-king-pos board)
                     (make-pos :row row :col col)))
              ((eq piece 'rn)
               (setf (board-black-king-pos board)
                     (make-pos :row row :col col))))))))


(defun validate-fen-knight-attack-p (board piece row col target-row target-col)
  "True if PIECE is a knight attacking TARGET-ROW,TARGET-COL."
  (declare (type board board) (ignore board)
           (type symbol piece) (type fixnum row col target-row target-col)
           (optimize (speed 3) (safety 2)))
  (when (or (eq piece 'cb) (eq piece 'cn))
    (let ((dr (abs (- row target-row)))
          (dc (abs (- col target-col))))
      (declare (type fixnum dr dc))
      (or (and (= dr 1) (= dc 2))
          (and (= dr 2) (= dc 1))))))

(defun validate-fen-pawn-attack-p (piece row col target-row target-col)
  "True if PIECE is a pawn attacking TARGET-ROW,TARGET-COL."
  (declare (type symbol piece) (type fixnum row col target-row target-col)
           (optimize (speed 3) (safety 2)))
  (or (and (eq piece 'pb)
           (= target-row (1- row))
           (= (abs (- target-col col)) 1))
      (and (eq piece 'pn)
           (= target-row (1+ row))
           (= (abs (- target-col col)) 1))))

(defun validate-fen-clear-ray-p (board row col target-row target-col dr dc)
  "True if the ray from ROW,COL by DR,DC reaches target without blockers."
  (declare (type board board) (type fixnum row col target-row target-col dr dc)
           (optimize (speed 3) (safety 2)))
  (do ((r (+ row dr) (+ r dr))
       (c (+ col dc) (+ c dc)))
      ((or (< r 0) (> r 7) (< c 0) (> c 7)) nil)
    (declare (type fixnum r c))
    (cond ((and (= r target-row) (= c target-col))
           (return t))
          ((not (eq (aref (board-board board) r c) 'vv))
           (return nil)))))

(defun validate-fen-slider-attack-p (board piece row col target-row target-col)
  "True if PIECE is a bishop/rook/queen attacking TARGET-ROW,TARGET-COL."
  (declare (type board board) (type symbol piece)
           (type fixnum row col target-row target-col)
           (optimize (speed 3) (safety 2)))
  (let ((same-rank (= row target-row))
        (same-file (= col target-col))
        (same-diag (= (abs (- row target-row)) (abs (- col target-col)))))
    (cond ((and (or (eq piece 'ab) (eq piece 'an)) same-diag)
           (validate-fen-clear-ray-p board row col target-row target-col
                                     (if (< row target-row) 1 -1)
                                     (if (< col target-col) 1 -1)))
          ((and (or (eq piece 'tb) (eq piece 'tn)) (or same-rank same-file))
           (validate-fen-clear-ray-p board row col target-row target-col
                                     (cond ((< row target-row) 1)
                                           ((> row target-row) -1)
                                           (t 0))
                                     (cond ((< col target-col) 1)
                                           ((> col target-col) -1)
                                           (t 0))))
          ((and (or (eq piece 'db) (eq piece 'dn))
                (or same-rank same-file same-diag))
           (validate-fen-clear-ray-p board row col target-row target-col
                                     (cond ((< row target-row) 1)
                                           ((> row target-row) -1)
                                           (t 0))
                                     (cond ((< col target-col) 1)
                                           ((> col target-col) -1)
                                           (t 0))))
          (t nil))))

(defun validate-fen-side-attacks-square-p (board side target-row target-col)
  "True if SIDE has any piece attacking TARGET-ROW,TARGET-COL."
  (declare (type board board) (type symbol side) (type fixnum target-row target-col)
           (optimize (speed 3) (safety 2)))
  (dotimes (row 8 nil)
    (dotimes (col 8)
      (let ((piece (aref (board-board board) row col)))
        (when (and (not (eq piece 'vv))
                   (eq (piece-color piece) side)
                   (or (validate-fen-pawn-attack-p piece row col target-row target-col)
                       (validate-fen-knight-attack-p board piece row col target-row target-col)
                       (validate-fen-slider-attack-p board piece row col target-row target-col)))
          (return-from validate-fen-side-attacks-square-p t))))))

(defun validate-fen-board (board fen)
  (declare (optimize (speed 3) (safety 2)))
  (let ((white-kings 0)
        (black-kings 0))
    (dotimes (row 8)
      (dotimes (col 8)
        (let ((piece (aref (board-board board) row col)))
          (cond ((eq piece 'rb) (incf white-kings))
                ((eq piece 'rn) (incf black-kings))))))
    (unless (and (= white-kings 1) (= black-kings 1))
      (error 'invalid-fen :fen fen))
    (let ((white-king (board-white-king-pos board))
          (black-king (board-black-king-pos board)))
      (when (and (<= (abs (- (pos-row white-king) (pos-row black-king))) 1)
                 (<= (abs (- (pos-col white-king) (pos-col black-king))) 1))
        (error 'invalid-fen :fen fen))
      (when (or (and (eq (board-side-to-move board) 'white)
                     (validate-fen-side-attacks-square-p
                      board 'white (pos-row black-king) (pos-col black-king)))
                (and (eq (board-side-to-move board) 'black)
                     (validate-fen-side-attacks-square-p
                      board 'black (pos-row white-king) (pos-col white-king))))
        (error 'invalid-fen :fen fen)))))


;;;; UCI / algebraic helpers

(defun uci-file-char (col)
    (declare (optimize (speed 3) (safety 2)))
(code-char (+ (char-code #\a) col)))

(defun uci-rank-char (row)
    (declare (optimize (speed 3) (safety 2)))
(code-char (+ (char-code #\1) (- 7 row))))

(defun pos-to-uci (pos)
    (declare (optimize (speed 3) (safety 2)))
(format nil "~A~A" (uci-file-char (pos-col pos)) (uci-rank-char (pos-row pos))))

(defun move-to-uci (move)
    (declare (optimize (speed 3) (safety 2)))
(let ((base (format nil "~A~A"
                      (pos-to-uci (move-from move))
                      (pos-to-uci (move-to move)))))
    (if (move-promotion move)
        (format nil "~A~A" base (promotion-char (move-promotion move)))
        base)))

(defun promotion-char (promotion)
    (declare (optimize (speed 3) (safety 2)))
(case promotion
    ((r) #\r)
    ((b) #\b)
    ((n) #\n)
    (t #\q)))

(defun uci-to-pos (algebraic)
  "Convert a UCI square string (e.g. \"e2\") to a pos struct."
    (declare (optimize (speed 3) (safety 2)))
(unless (= (length algebraic) 2)
    (error 'invalid-move :move algebraic))
  (let* ((file (aref algebraic 0))
         (rank (aref algebraic 1))
         (rank-digit (digit-char-p rank)))
    (unless (and (char<= #\a (char-downcase file) #\h)
                 rank-digit (<= 1 rank-digit 8))
      (error 'invalid-move :move algebraic))
    (make-pos :row (convert-rank rank-digit)
              :col (convert-file file))))

(defun char-to-promotion (ch)
  "Return the UCI promotion symbol (q/r/b/n) for a promotion char."
    (declare (optimize (speed 3) (safety 2)))
(case (char-downcase ch)
    ((#\q) 'q)
    ((#\r) 'r)
    ((#\b) 'b)
    ((#\n) 'n)
    (t nil)))


(defun promotion-piece-for-color (promotion color)
  "Return the internal piece symbol for a UCI promotion PROMOTION and COLOR."
    (declare (optimize (speed 3) (safety 2)))
(case promotion
    ((r) (if (eq color 'white) 'tb 'tn))
    ((b) (if (eq color 'white) 'ab 'an))
    ((n) (if (eq color 'white) 'cb 'cn))
    (t (if (eq color 'white) 'db 'dn))))

(defun uci-to-move (algebraic board)
  "Convert a UCI move string (e.g. \"e2e4\" or \"e7e8q\") to a move struct."
    (declare (optimize (speed 3) (safety 2)))
(let ((len (length algebraic)))
    (unless (or (= len 4) (= len 5))
      (error 'invalid-move :move algebraic))
    (let ((from (uci-to-pos (subseq algebraic 0 2)))
          (to (uci-to-pos (subseq algebraic 2 4)))
          (promotion nil))
      (when (= len 5)
        (let ((piece (board-pos (board-board board) from)))
          (unless (and (pawnp piece)
                       (or (= (pos-row to) 0) (= (pos-row to) 7)))
            (error 'invalid-move :move algebraic))
          (setf promotion (char-to-promotion (aref algebraic 4)))
          (unless promotion
            (error 'invalid-move :move algebraic))))
      (make-move :from from :to to :promotion promotion))))
