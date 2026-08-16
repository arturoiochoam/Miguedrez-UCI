;;; -*- encoding: utf-8 -*-
;;;
;;; moves.lisp
;;; Move execution, move generation orchestration and check/mate detection.
;;; gamallo, March 8, 2007
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


;;;; Move execution

(defun make-move-on-board (board algebraic)
  "Apply a legal UCI algebraic move string to BOARD destructively."
    (declare (optimize (speed 3) (safety 2)))
(let* ((move (uci-to-move algebraic board))
         (legal (legal-moves board (board-side-to-move board))))
    (unless (member move legal :test #'equal-moves)
      (error 'invalid-move :move algebraic))
    (execute-move board move)
    move))


(defun save-enpass-to-undo (board undo)
  "Copy the current en-passant flags from BOARD into UNDO."
  (declare (type board board) (type move-undo undo)
           (optimize (speed 3) (safety 2)))
  (replace (move-undo-whites-enpass undo) (board-whites-enpass board))
  (replace (move-undo-blacks-enpass undo) (board-blacks-enpass board)))


(defun execute-move (board move &optional (undo (make-move-undo)))
  "Execute MOVE on BOARD destructively.  Return BOARD and an UNDO record.

The UNDO record contains all state changed by the move so that
UNMAKE-MOVE can restore the position exactly.

UNDO may be supplied by the caller and reused as scratch storage: the
KING-VALID legality path passes a single shared record (*KING-VALID-UNDO*)
to avoid allocating a fresh one per pseudo-legal move.  Because a reused
record can carry state from a previous move, the two slots that are only
populated conditionally (CASTLED and EP-CAPTURED-POS) are reset here before
anything else; every other slot is written unconditionally below."
  (declare (type board board) (type move move) (type move-undo undo)
           (optimize (speed 3) (safety 2)))
  (let* ((color (board-side-to-move board))
         (from (move-from move))
         (to (move-to move))
         (piece (board-pos (board-board board) from))
         (captured (board-pos (board-board board) to)))
    (declare (type symbol color piece captured))
    ;; Reset conditionally-populated slots so a reused scratch record never
    ;; restores phantom castling/en-passant state (fresh records already NIL).
    (setf (move-undo-castled undo) nil)
    (setf (move-undo-ep-captured-pos undo) nil)
    ;; Save pre-move state.
    (setf (move-undo-from-piece undo) piece)
    (setf (move-undo-captured undo) captured)
    (setf (move-undo-side-to-move undo) color)
    (setf (move-undo-halfmove-clock undo) (board-halfmove-clock board))
    (setf (move-undo-fullmove-number undo) (board-fullmove-number board))
    ;; Save the king positions by reference, not by copy.  A POS is never
    ;; mutated in place anywhere in the engine (verified: no SETF/INCF of
    ;; POS-ROW/POS-COL exists) -- BOARD-*-KING-POS is only ever *replaced*
    ;; wholesale -- so the current object is already a stable, effectively
    ;; immutable snapshot that UNMAKE can restore directly.  This removes the
    ;; two MAKE-POS allocations per EXECUTE-MOVE call, the largest remaining
    ;; hot-path allocation after the KING-VALID undo was pooled (G8 change 1).
    (setf (move-undo-white-king-pos undo) (board-white-king-pos board))
    (setf (move-undo-black-king-pos undo) (board-black-king-pos board))
    (setf (move-undo-white-king-moved undo) (board-white-king-moved board))
    (setf (move-undo-black-king-moved undo) (board-black-king-moved board))
    (setf (move-undo-white-kingside-rook-moved undo)
          (board-white-kingside-rook-moved board))
    (setf (move-undo-white-queenside-rook-moved undo)
          (board-white-queenside-rook-moved board))
    (setf (move-undo-black-kingside-rook-moved undo)
          (board-black-kingside-rook-moved board))
    (setf (move-undo-black-queenside-rook-moved undo)
          (board-black-queenside-rook-moved board))
    (save-enpass-to-undo board undo)
    ;; Save the Zobrist key before the move.
    (setf (move-undo-key undo) (board-key board))
    ;; Record the position before the move so future repetitions can be
    ;; detected.
    (push (board-key board) (board-position-history board))
    ;; Execute en passant captures and update en passant flags.
    (let ((ep-pos (execute-move-enpass board move)))
      (when ep-pos
        (setf (move-undo-ep-captured-pos undo) ep-pos)))
    ;; Move the piece on the main board.
    (execute-move-board board move)
    ;; Update occupancy, king position, castling and promotion.
    (if (eq color 'white)
        (execute-move-white board move piece undo)
        (execute-move-black board move piece undo))
    ;; Update clocks and side to move.
    (setf (board-side-to-move board) (invert-color color))
    (setf (board-halfmove-clock board)
          (if (or (pawnp piece) (not (eq captured 'vv)))
              0
              (1+ (board-halfmove-clock board))))
    (when (eq color 'black)
      (incf (board-fullmove-number board)))
    ;; Set a new en-passant right for a just-completed double push, legality-
    ;; aware (pin-aware).  Deferred until the pawn is on its destination and
    ;; side-to-move has flipped, so EP-CAPTURE-LEGAL-P sees the true post-push
    ;; position.  Must run before UPDATE-BOARD-KEY-AFTER-MOVE so the incremental
    ;; key toggle reflects the final flag state.
    (maybe-set-enpassant-right board move)
    ;; Incrementally update the Zobrist key from the saved undo state.
    (update-board-key-after-move board move undo)
    (values board undo)))


(defun unmake-move (board move undo)
  "Restore BOARD to the state saved in UNDO, reversing MOVE."
  (declare (type board board) (type move move) (type move-undo undo)
           (optimize (speed 3) (safety 2)))
  ;; Pop the position key pushed by execute-move.
  (pop (board-position-history board))
  ;; Restore side, clocks and incremental flags.
  (setf (board-side-to-move board) (move-undo-side-to-move undo))
  (setf (board-halfmove-clock board) (move-undo-halfmove-clock undo))
  (setf (board-fullmove-number board) (move-undo-fullmove-number undo))
  (replace (board-whites-enpass board) (move-undo-whites-enpass undo))
  (replace (board-blacks-enpass board) (move-undo-blacks-enpass undo))
  (setf (board-white-king-pos board) (move-undo-white-king-pos undo))
  (setf (board-black-king-pos board) (move-undo-black-king-pos undo))
  (setf (board-white-king-moved board) (move-undo-white-king-moved undo))
  (setf (board-black-king-moved board) (move-undo-black-king-moved undo))
  (setf (board-white-kingside-rook-moved board)
        (move-undo-white-kingside-rook-moved undo))
  (setf (board-white-queenside-rook-moved board)
        (move-undo-white-queenside-rook-moved undo))
  (setf (board-black-kingside-rook-moved board)
        (move-undo-black-kingside-rook-moved undo))
  (setf (board-black-queenside-rook-moved board)
        (move-undo-black-queenside-rook-moved undo))
  ;; Undo the castling rook move first, if any.
  (when (move-undo-castled undo)
    (let ((from (move-undo-rook-to undo))
          (to (move-undo-rook-from undo))
          (piece (move-undo-rook-piece undo)))
      (declare (type pos from to) (type symbol piece))
      (setf (aref (board-board board) (pos-row to) (pos-col to)) piece)
      (setf (aref (board-board board) (pos-row from) (pos-col from)) 'vv)
      (if (eq piece 'tb)
          (progn
            (setf (aref (board-whites board) (pos-row to) (pos-col to)) '1)
            (setf (aref (board-whites board) (pos-row from) (pos-col from)) '0)
            (setf (aref (board-blacks board) (pos-row to) (pos-col to)) '0))
          (progn
            (setf (aref (board-blacks board) (pos-row to) (pos-col to)) '1)
            (setf (aref (board-blacks board) (pos-row from) (pos-col from)) '0)
            (setf (aref (board-whites board) (pos-row to) (pos-col to)) '0)))))
  ;; Restore the moving piece and any capture on the destination square.
  (setf (board-pos (board-board board) (move-to move))
        (move-undo-captured undo))
  (setf (board-pos (board-board board) (move-from move))
        (move-undo-from-piece undo))
  ;; Restore occupancy for the from/to squares.
  (let ((color (move-undo-side-to-move undo))
        (from (move-from move))
        (to (move-to move)))
    (declare (type symbol color))
    (if (eq color 'white)
        (progn
          (setf (board-pos (board-whites board) from) '1)
          (setf (board-pos (board-whites board) to) '0)
          (setf (board-pos (board-blacks board) to)
                (if (eq (move-undo-captured undo) 'vv) '0 '1)))
        (progn
          (setf (board-pos (board-blacks board) from) '1)
          (setf (board-pos (board-blacks board) to) '0)
          (setf (board-pos (board-whites board) to)
                (if (eq (move-undo-captured undo) 'vv) '0 '1)))))
  ;; Restore a pawn captured en passant.
  (let ((ep (move-undo-ep-captured-pos undo)))
    (when ep
      (let ((row (pos-row ep)) (col (pos-col ep)))
        (setf (aref (board-board board) row col)
              (if (eq (move-undo-side-to-move undo) 'white) 'pn 'pb))
        (setf (aref (board-blacks board) row col)
              (if (eq (move-undo-side-to-move undo) 'white) '1 '0))
        (setf (aref (board-whites board) row col)
              (if (eq (move-undo-side-to-move undo) 'white) '0 '1)))))
  ;; Restore the Zobrist key saved before the move.
  (setf (board-key board) (move-undo-key undo))
  board)


(defun make-null-move (board &optional (undo (make-null-undo)))
  "Play a null move (a pass) on BOARD destructively.  Return BOARD and an
UNDO record.

A null move only flips the side to move; the opponent is granted a free
move.  It is used by null-move pruning in the search.  Correctness of the
Zobrist key is critical (an incorrect null-move key causes invalid TT hits
and severe tactical errors), so this mirrors COMPUTE-BOARD-KEY's en-passant
folding exactly:

  * every set en-passant file, in BOTH the white and black arrays, has its
    *ZOBRIST-ENPASSANT* value XORed OUT of the key BEFORE the arrays are
    cleared (a file set in both arrays is toggled twice, i.e. cancels, which
    is precisely COMPUTE-BOARD-KEY's convention);
  * the side-to-move key is toggled;
  * both en-passant arrays are cleared (a pass forfeits any en-passant right).

The halfmove clock advances (a pass is neither a pawn move nor a capture).
Piece placement, castling flags, king positions, the fullmove number, and
the position history are deliberately left untouched -- in particular the
position history is NOT pushed, so null moves neither poison threefold-
repetition detection nor unbalance make/unmake."
  (declare (type board board) (type null-undo undo)
           (optimize (speed 3) (safety 2)))
  (let ((color (board-side-to-move board))
        (key (board-key board))
        (whites-ep (board-whites-enpass board))
        (blacks-ep (board-blacks-enpass board)))
    (declare (type (unsigned-byte 64) key) (type symbol color))
    ;; Save the pre-null state.
    (replace (null-undo-whites-enpass undo) whites-ep)
    (replace (null-undo-blacks-enpass undo) blacks-ep)
    (setf (null-undo-halfmove-clock undo) (board-halfmove-clock board))
    (setf (null-undo-side-to-move undo) color)
    (setf (null-undo-key undo) key)
    ;; XOR out every set en-passant file (both arrays) before clearing them.
    (dotimes (col 8)
      (when (eq (aref whites-ep col) '1)
        (setf key (logxor key (aref *zobrist-enpassant* col))))
      (when (eq (aref blacks-ep col) '1)
        (setf key (logxor key (aref *zobrist-enpassant* col)))))
    ;; Toggle side to move.
    (setf key (logxor key *zobrist-side*))
    ;; Clear the en-passant arrays (a pass forfeits any en-passant right).
    (fill whites-ep '0)
    (fill blacks-ep '0)
    ;; Commit the flipped side, advanced clock and new key.
    (setf (board-side-to-move board) (invert-color color))
    (setf (board-halfmove-clock board) (1+ (board-halfmove-clock board)))
    (setf (board-key board) key)
    (values board undo)))


(defun unmake-null-move (board undo)
  "Reverse the null move recorded in UNDO, restoring BOARD exactly."
  (declare (type board board) (type null-undo undo)
           (optimize (speed 3) (safety 2)))
  (setf (board-side-to-move board) (null-undo-side-to-move undo))
  (setf (board-halfmove-clock board) (null-undo-halfmove-clock undo))
  (replace (board-whites-enpass board) (null-undo-whites-enpass undo))
  (replace (board-blacks-enpass board) (null-undo-blacks-enpass undo))
  (setf (board-key board) (null-undo-key undo))
  board)


(defun update-board-key-after-move (board move undo)
  "Incrementally update BOARD's Zobrist key after MOVE has been applied.
The old key is read from UNDO; the new board state supplies side-to-move,
castling and en-passant changes."
  (declare (type board board) (type move move) (type move-undo undo)
           (optimize (speed 3) (safety 2)))
  (let ((key (move-undo-key undo))
        (from (move-from move))
        (to (move-to move))
        (color (move-undo-side-to-move undo))
        (from-piece (move-undo-from-piece undo))
        (captured (move-undo-captured undo))
        (promotion (move-promotion move)))
    (declare (type (unsigned-byte 64) key)
             (type pos from to)
             (type symbol color from-piece captured))
    (flet ((toggle-piece (key piece sq)
             (let ((idx (piece-zobrist-index piece)))
               (declare (type fixnum sq))
               (if idx
                   (logxor key (aref *zobrist-pieces* sq idx))
                   key)))
           (toggle-ep (key col)
             (logxor key (aref *zobrist-enpassant* col)))
           (toggle-castle (key index)
             (logxor key (aref *zobrist-castling* index))))
      (declare (inline toggle-piece toggle-ep toggle-castle))
      ;; Side to move always flips.
      (setf key (logxor key *zobrist-side*))
      ;; Remove moving piece from its source square.
      (setf key (toggle-piece key from-piece (move-origin-index move)))
      ;; Remove any piece captured on the destination square.
      (unless (eq captured 'vv)
        (setf key (toggle-piece key captured (move-target-index move))))
      ;; Place the piece (or promotion piece) on the destination square.  A
      ;; pawn that lands on the promotion rank MUST promote; PROMOTE-IF-NEEDED
      ;; defaults a nil PROMOTION field to a queen in that case, so the key
      ;; path must make the same default -- otherwise a hand-built promotion
      ;; move with a nil PROMOTION would place a queen on the board but toggle
      ;; the pawn's Zobrist index here, desyncing the incremental key from both
      ;; the board and COMPUTE-BOARD-KEY (corrupting TT probes and repetition
      ;; detection).  Unreachable via UCI today (EQUAL-MOVES compares PROMOTION
      ;; and POSSIBLE-PAWN always sets it), but this closes the latent gap.
      (let ((to-piece (if promotion
                          (promotion-piece-for-color promotion color)
                          (if (and (pawnp from-piece)
                                   (or (= (pos-row to) 0)
                                       (= (pos-row to) 7)))
                              (promotion-piece-for-color 'q color)
                              from-piece))))
        (setf key (toggle-piece key to-piece (move-target-index move))))
      ;; Remove a pawn captured en passant.
      (let ((ep (move-undo-ep-captured-pos undo)))
        (when ep
          (let ((ep-piece (if (eq color 'white) 'pn 'pb))
                (ep-sq (pos-square-index ep)))
            (setf key (toggle-piece key ep-piece ep-sq)))))
      ;; Move the castling rook.
      (when (move-undo-castled undo)
        (let ((rook-from (move-undo-rook-from undo))
              (rook-to (move-undo-rook-to undo))
              (rook-piece (move-undo-rook-piece undo)))
          (setf key (toggle-piece key rook-piece (pos-square-index rook-from)))
          (setf key (toggle-piece key rook-piece (pos-square-index rook-to)))))
      ;; En passant flags: toggle files whose state changed.
      (dotimes (col 8)
        (let ((old-white (aref (move-undo-whites-enpass undo) col))
              (new-white (aref (board-whites-enpass board) col))
              (old-black (aref (move-undo-blacks-enpass undo) col))
              (new-black (aref (board-blacks-enpass board) col)))
          (unless (eq old-white new-white)
            (setf key (toggle-ep key col)))
          (unless (eq old-black new-black)
            (setf key (toggle-ep key col)))))
      ;; Castling rights: toggle rights whose availability changed.
      (let ((old-wk (castling-right-available-p (move-undo-white-king-moved undo)
                                                (move-undo-white-kingside-rook-moved undo)))
            (new-wk (castling-right-available-p (board-white-king-moved board)
                                                (board-white-kingside-rook-moved board)))
            (old-wq (castling-right-available-p (move-undo-white-king-moved undo)
                                                (move-undo-white-queenside-rook-moved undo)))
            (new-wq (castling-right-available-p (board-white-king-moved board)
                                                (board-white-queenside-rook-moved board)))
            (old-bk (castling-right-available-p (move-undo-black-king-moved undo)
                                                (move-undo-black-kingside-rook-moved undo)))
            (new-bk (castling-right-available-p (board-black-king-moved board)
                                                (board-black-kingside-rook-moved board)))
            (old-bq (castling-right-available-p (move-undo-black-king-moved undo)
                                                (move-undo-black-queenside-rook-moved undo)))
            (new-bq (castling-right-available-p (board-black-king-moved board)
                                                (board-black-queenside-rook-moved board))))
        (unless (eq old-wk new-wk) (setf key (toggle-castle key 0)))
        (unless (eq old-wq new-wq) (setf key (toggle-castle key 1)))
        (unless (eq old-bk new-bk) (setf key (toggle-castle key 2)))
        (unless (eq old-bq new-bq) (setf key (toggle-castle key 3))))
      (setf (board-key board) key))))


(defun execute-move-board (board move)
  "Move the piece in the main board array."
  (declare (type board board) (type move move) (optimize (speed 3) (safety 2)))
  (setf (board-pos (board-board board) (move-to move))
        (board-pos (board-board board) (move-from move)))
  (setf (board-pos (board-board board) (move-from move)) 'vv))


(defun execute-move-white (board move piece undo)
  (declare (type board board) (type move move) (type symbol piece)
           (type move-undo undo) (optimize (speed 3) (safety 2)))
  (setf (board-pos (board-whites board) (move-to move)) '1)
  (setf (board-pos (board-whites board) (move-from move)) '0)
  (when (eq (board-pos (board-blacks board) (move-to move)) '1)
    (setf (board-pos (board-blacks board) (move-to move)) '0))
  (update-rook-capture-castling-rights board move (move-undo-captured undo))
  (when (eq piece 'rb)
    (update-white-king-and-castle board move undo))
  (when (eq piece 'tb)
    (update-white-rook-moved board move))
  (promote-if-needed board move 'white))


(defun execute-move-black (board move piece undo)
  (declare (type board board) (type move move) (type symbol piece)
           (type move-undo undo) (optimize (speed 3) (safety 2)))
  (setf (board-pos (board-blacks board) (move-to move)) '1)
  (setf (board-pos (board-blacks board) (move-from move)) '0)
  (when (eq (board-pos (board-whites board) (move-to move)) '1)
    (setf (board-pos (board-whites board) (move-to move)) '0))
  (update-rook-capture-castling-rights board move (move-undo-captured undo))
  (when (eq piece 'rn)
    (update-black-king-and-castle board move undo))
  (when (eq piece 'tn)
    (update-black-rook-moved board move))
  (promote-if-needed board move 'black))


(defun update-white-king-and-castle (board move undo)
  (declare (type board board) (type move move) (type move-undo undo)
           (optimize (speed 3) (safety 2)))
  (setf (board-white-king-pos board) (move-to move))
  (unless (board-white-king-moved board)
    (setf (board-white-king-moved board) t)
    (handle-castling board move undo 7)))


(defun update-black-king-and-castle (board move undo)
  (declare (type board board) (type move move) (type move-undo undo)
           (optimize (speed 3) (safety 2)))
  (setf (board-black-king-pos board) (move-to move))
  (unless (board-black-king-moved board)
    (setf (board-black-king-moved board) t)
    (handle-castling board move undo 0)))


(defun handle-castling (board move undo base-row)
  "If MOVE is a king move from e-file to the g- or c-file on BASE-ROW (rank 1
for white, rank 8 for black), move the corresponding rook and record its
old/new position and piece in UNDO for UNMAKE-MOVE and the incremental
Zobrist update.  BASE-ROW is 7 for white, 0 for black -- the only difference
between the two colors' castling geometry."
  (declare (type board board) (type move move) (type move-undo undo)
           (type fixnum base-row) (optimize (speed 3) (safety 2)))
  (let ((from (move-from move))
        (to (move-to move)))
    (cond ((and (= (pos-row from) base-row) (= (pos-col from) 4)
                (= (pos-row to) base-row) (= (pos-col to) 6))
           (setf (move-undo-castled undo) t)
           (setf (move-undo-rook-from undo) (make-pos :row base-row :col 7))
           (setf (move-undo-rook-to undo) (make-pos :row base-row :col 5))
           (setf (move-undo-rook-piece undo) (aref (board-board board) base-row 7))
           (move-rook-for-castle board base-row 7 base-row 5))
          ((and (= (pos-row from) base-row) (= (pos-col from) 4)
                (= (pos-row to) base-row) (= (pos-col to) 2))
           (setf (move-undo-castled undo) t)
           (setf (move-undo-rook-from undo) (make-pos :row base-row :col 0))
           (setf (move-undo-rook-to undo) (make-pos :row base-row :col 3))
           (setf (move-undo-rook-piece undo) (aref (board-board board) base-row 0))
           (move-rook-for-castle board base-row 0 base-row 3)))))


(defun move-rook-for-castle (board from-row from-col to-row to-col)
  (declare (type board board) (type fixnum from-row from-col to-row to-col)
           (optimize (speed 3) (safety 2)))
  (let ((piece (aref (board-board board) from-row from-col)))
    (setf (aref (board-board board) to-row to-col) piece)
    (setf (aref (board-board board) from-row from-col) 'vv)
    (if (eq piece 'tb)
        (progn
          (setf (aref (board-whites board) to-row to-col) '1)
          (setf (aref (board-whites board) from-row from-col) '0))
        (progn
          (setf (aref (board-blacks board) to-row to-col) '1)
          (setf (aref (board-blacks board) from-row from-col) '0)))))


(defun update-white-rook-moved (board move)
  (declare (type board board) (type move move) (optimize (speed 3) (safety 2)))
  (let ((from (move-from move)))
    (when (and (= (pos-row from) 7) (= (pos-col from) 7))
      (setf (board-white-kingside-rook-moved board) t))
    (when (and (= (pos-row from) 7) (= (pos-col from) 0))
      (setf (board-white-queenside-rook-moved board) t))))


(defun update-black-rook-moved (board move)
  (declare (type board board) (type move move) (optimize (speed 3) (safety 2)))
  (let ((from (move-from move)))
    (when (and (= (pos-row from) 0) (= (pos-col from) 7))
      (setf (board-black-kingside-rook-moved board) t))
    (when (and (= (pos-row from) 0) (= (pos-col from) 0))
      (setf (board-black-queenside-rook-moved board) t))))


(defun update-rook-capture-castling-rights (board move captured)
  "Clear castling rights when a rook is captured on its original corner."
  (declare (type board board) (type move move) (type symbol captured)
           (optimize (speed 3) (safety 2)))
  (let ((to (move-to move)))
    (cond ((and (eq captured 'tb) (= (pos-row to) 7) (= (pos-col to) 7))
           (setf (board-white-kingside-rook-moved board) t))
          ((and (eq captured 'tb) (= (pos-row to) 7) (= (pos-col to) 0))
           (setf (board-white-queenside-rook-moved board) t))
          ((and (eq captured 'tn) (= (pos-row to) 0) (= (pos-col to) 7))
           (setf (board-black-kingside-rook-moved board) t))
          ((and (eq captured 'tn) (= (pos-row to) 0) (= (pos-col to) 0))
           (setf (board-black-queenside-rook-moved board) t)))))


(defun promote-if-needed (board move color)
  (declare (type board board) (type move move) (type symbol color)
           (optimize (speed 3) (safety 2)))
  (let ((piece (board-pos (board-board board) (move-to move))))
    (when (pawnp piece)
      (when (or (= (pos-row (move-to move)) 0)
                (= (pos-row (move-to move)) 7))
        (let ((promotion (or (move-promotion move) 'q)))
          (setf (board-pos (board-board board) (move-to move))
                (promotion-piece-for-color promotion color)))))))


;;;; En passant handling

(defun execute-move-enpass (board move)
  "Apply en passant captures and update en passant flags.
Return the position of a pawn captured en passant, or NIL."
  (declare (type board board) (type move move) (optimize (speed 3) (safety 2)))
  (let ((a-board (board-board board))
        (from (move-from move))
        (to (move-to move))
        (ep-captured nil))
    ;; Capture en passant if applicable.  The captured pawn must really be on
    ;; the square behind the target; a malformed FEN or hand-built board with a
    ;; stale flag must never make UNMAKE-MOVE resurrect a phantom pawn.
    (cond ((and (eq (board-pos a-board from) 'pn)
                (= (pos-row to) 5)
                (eq (aref (board-whites-enpass board) (pos-col to)) '1)
                (eq (aref a-board 4 (pos-col to)) 'pb))
           (setf (aref a-board 4 (pos-col to)) 'vv)
           (setf (aref (board-whites board) 4 (pos-col to)) '0)
           (setf ep-captured (make-pos :row 4 :col (pos-col to))))
          ((and (eq (board-pos a-board from) 'pb)
                (= (pos-row to) 2)
                (eq (aref (board-blacks-enpass board) (pos-col to)) '1)
                (eq (aref a-board 3 (pos-col to)) 'pn))
           (setf (aref a-board 3 (pos-col to)) 'vv)
           (setf (aref (board-blacks board) 3 (pos-col to)) '0)
           (setf ep-captured (make-pos :row 3 :col (pos-col to)))))
    ;; Reset en passant flags.  The new-right set for a just-completed double
    ;; push is deferred to MAYBE-SET-ENPASSANT-RIGHT, which EXECUTE-MOVE runs
    ;; after the pawn has been placed on its destination -- the legality probe
    ;; (pin-aware, via EP-CAPTURE-LEGAL-P) must see the true post-push position,
    ;; but at this point the moving pawn is still on FROM.
    (fill (board-whites-enpass board) '0)
    (fill (board-blacks-enpass board) '0)
    ep-captured))


(defun maybe-set-enpassant-right (board move)
  "After MOVE has been fully played on BOARD, set the en-passant flag for a
just-completed double pawn push -- but only when at least one adjacent enemy
pawn can LEGALLY capture it (ENPASSANT-RIGHT-CAPTURABLE-P, which is pin-aware
via EP-CAPTURE-LEGAL-P).

Split out of EXECUTE-MOVE-ENPASS and run after EXECUTE-MOVE-BOARD /
EXECUTE-MOVE-WHITE|-BLACK have placed the pawn on its destination and
BOARD-SIDE-TO-MOVE has flipped, so the legality probe sees the double-pushed
pawn on its target square, the skipped square empty, and the opponent to move.
EXECUTE-MOVE-ENPASS still resets all flags and performs any e.p. capture; only
the new-right set is deferred here."
  (declare (type board board) (type move move) (optimize (speed 3) (safety 2)))
  (let ((a-board (board-board board))
        (from (move-from move))
        (to (move-to move)))
    (cond ((and (eq (aref a-board (pos-row to) (pos-col to)) 'pb)
                (= (pos-row from) 6) (= (pos-row to) 4)
                (enpassant-right-capturable-p board 'white (pos-col to)))
           (setf (aref (board-whites-enpass board) (pos-col to)) '1))
          ((and (eq (aref a-board (pos-row to) (pos-col to)) 'pn)
                (= (pos-row from) 1) (= (pos-row to) 3)
                (enpassant-right-capturable-p board 'black (pos-col to)))
           (setf (aref (board-blacks-enpass board) (pos-col to)) '1)))))


;;;; Move generation orchestration

(defun children-piece (board pos &key castle (in-check-known :unknown))
  "Return a list of pseudo-legal moves for the piece at POS.

Each piece generator now builds full move structures directly, avoiding the
intermediate target-square lists that used to be wrapped into moves here.

IN-CHECK-KNOWN (:UNKNOWN to compute it) is threaded into POSSIBLE-KING so the
king-threatened scan can be shared with a caller that already knows it."
  (declare (type board board) (type pos pos) (optimize (speed 3) (safety 2)))
  (let ((piece (board-pos (board-board board) pos)))
    (cond ((or (eq piece 'pb) (eq piece 'pn))
           (possible-pawn board pos))
          ((or (eq piece 'cb) (eq piece 'cn))
           (possible-knight board pos))
          ((or (eq piece 'tb) (eq piece 'tn))
           (possible-rook board pos))
          ((or (eq piece 'ab) (eq piece 'an))
           (possible-bishop board pos))
          ((or (eq piece 'db) (eq piece 'dn))
           (possible-queen board pos))
          ((or (eq piece 'rb) (eq piece 'rn))
           (if (eq castle 'without-castling)
               (possible-king-without-castling board pos)
               (possible-king board pos in-check-known)))
          (t nil))))


(defun children (board color &key castle (in-check-known :unknown))
  "Return every pseudo-legal move for COLOR's pieces on BOARD, in no
particular order (both callers -- LEGAL-MOVES via DOLIST, VALID via
MEMBER -- are order-independent).

IN-CHECK-KNOWN (:UNKNOWN to compute it inside POSSIBLE-KING) lets LEGAL-MOVES
pass the in-check status it already derived from CHECKERS-OF, avoiding a
redundant king-threatened scan on the per-node hot path."
  (declare (type board board) (type symbol color) (optimize (speed 3) (safety 2)))
  (let ((board-color (if (whitep color)
                         (board-whites board)
                         (board-blacks board)))
        (children '()))
    (dotimes (row 8 children)
      (dotimes (col 8)
        (when (color-occupied-p board-color row col)
          (let ((current-pos (make-pos :row row :col col)))
            ;; Each CHILDREN-PIECE call returns a freshly-consed list, so
            ;; splicing it onto the front of the accumulator via NCONC is
            ;; safe and O(that piece's own move count) -- unlike appending
            ;; to the end, which would re-walk the whole, ever-growing
            ;; accumulator on every piece (O(n^2) over the pieces on board).
            (setf children
                  (nconc (children-piece board current-pos
                                         :castle castle :in-check-known in-check-known)
                         children))))))))


;;;; Threats, validity and check/mate detection

(declaim (inline in-board-p))
(defun in-board-p (row col)
    (declare (optimize (speed 3) (safety 2)))
(and (>= row 0) (< row 8) (>= col 0) (< col 8)))


(defparameter +knight-deltas+
  '((-2 -1) (-2 1) (-1 -2) (-1 2) (1 -2) (1 2) (2 -1) (2 1))
  "The 8 knight-move offsets, shared by knight move generation
(src/moves/knight.lisp) and knight-attack detection (ATTACKED-BY-KNIGHT-P,
CHECKERS-OF below) so all three stay in sync instead of independently
hand-duplicating the same deltas.")


(defparameter +king-deltas+
  '((-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 -1) (1 0) (1 1))
  "The 8 king-move offsets, shared by king move generation
(src/moves/king.lisp) and king-attack detection (ATTACKED-BY-KING-P,
CHECKERS-OF below) so all three stay in sync instead of independently
hand-duplicating the same deltas.")


(defparameter +queen-directions+
  '((-1 0) (1 0) (0 -1) (0 1) (-1 -1) (-1 1) (1 -1) (1 1))
  "The 4 rook (orthogonal) and 4 bishop (diagonal) ray directions combined,
shared by ATTACKED-BY-SLIDING-P, CHECKERS-OF and PIN-MAP below so the same
8-direction sliding scan isn't hand-duplicated three times. Defined here
(rather than reusing ROOK-LISP/BISHOP-LISP's own direction constants)
because this file loads before those.")


(defun rook-and-bishop-pieces (color)
  "Return (VALUES ROOK-PIECES BISHOP-PIECES), the piece symbols for COLOR's
rook/queen and bishop/queen sliders. Shared by ATTACKED-BY-SLIDING-P,
CHECKERS-OF and PIN-MAP so the two piece-symbol pairs are derived in one
place instead of each function re-deriving them from COLOR."
  (declare (type symbol color) (optimize (speed 3) (safety 2)))
  (if (eq color 'white)
      (values '(tb db) '(ab db))
      (values '(tn dn) '(an dn))))


(defun scan-rays (board pos opponent-array directions)
  "Return all sliding moves from POS along DIRECTIONS (a list of (dr dc)
pairs): every empty square is added, and the first blocking square on each
ray adds one further move only when OPPONENT-ARRAY (the enemy color's
occupancy array) marks it as enemy-occupied, then the ray stops either way.
Shared scan primitive for rooks and bishops (ROOK-LISP/BISHOP-LISP call
this with their own direction list and BOARD-BLACKS/BOARD-WHITES) so the
sliding-piece scan logic exists in exactly one place."
  (declare (type board board) (type pos pos) (type list directions)
           (optimize (speed 3) (safety 2)))
  (let ((moves '())
        (a-board (board-board board))
        (row (pos-row pos))
        (col (pos-col pos)))
    (declare (type fixnum row col))
    (dolist (dir directions)
      (let ((dr (first dir)) (dc (second dir)))
        (declare (type fixnum dr dc))
        (do ((r (+ row dr) (+ r dr))
             (c (+ col dc) (+ c dc)))
            ((not (in-board-p r c)))
          (declare (type fixnum r c))
          (let ((piece (aref a-board r c)))
            (cond ((eq piece 'vv)
                   (add-move moves pos r c))
                  ((eq (aref opponent-array r c) '1)
                   (add-move moves pos r c)
                   (return))
                  (t (return)))))))
    moves))


(declaim (inline pawn-attack-direction))
(defun pawn-attack-direction (color)
  "Return the row delta a pawn of COLOR moves toward promotion."
  (declare (type symbol color) (optimize (speed 3) (safety 2)))
  (if (eq color 'white) -1 1))


(defun square-attacked-p (board row col by-color)
  "Return true if the square (ROW,COL) is attacked by BY-COLOR."
  (declare (type board board) (type fixnum row col) (type symbol by-color)
           (optimize (speed 3) (safety 2)))
  (or (attacked-by-pawn-p board row col by-color)
      (attacked-by-knight-p board row col by-color)
      (attacked-by-king-p board row col by-color)
      (attacked-by-sliding-p board row col by-color)))


(defun attacked-by-pawn-p (board row col by-color)
  "True if (ROW,COL) is attacked by a pawn of BY-COLOR.\n\nA pawn attacks one square forward-diagonally; the source square depends on the\npawn's forward direction."
  (declare (type board board) (type fixnum row col) (type symbol by-color)
           (optimize (speed 3) (safety 2)))
  (let ((dir (pawn-attack-direction by-color))
        (pawn-piece (if (eq by-color 'white) 'pb 'pn)))
    (declare (type fixnum dir))
    (flet ((pawn-at-p (r c)
             (declare (type fixnum r c))
             (and (in-board-p r c)
                  (eq (aref (board-board board) r c) pawn-piece))))
      (or (pawn-at-p (- row dir) (- col 1))
          (pawn-at-p (- row dir) (+ col 1))))))


(defun attacked-by-knight-p (board row col by-color)
  (declare (type board board) (type fixnum row col) (type symbol by-color)
           (optimize (speed 3) (safety 2)))
  (let ((knight-piece (if (eq by-color 'white) 'cb 'cn)))
    (dolist (offset +knight-deltas+)
      (let ((r (+ row (first offset)))
            (c (+ col (second offset))))
        (declare (type fixnum r c))
        (when (and (in-board-p r c)
                   (eq (aref (board-board board) r c) knight-piece))
          (return t))))))


(defun attacked-by-king-p (board row col by-color)
  (declare (type board board) (type fixnum row col) (type symbol by-color)
           (optimize (speed 3) (safety 2)))
  (let ((king-piece (if (eq by-color 'white) 'rb 'rn)))
    (dolist (offset +king-deltas+)
      (let ((r (+ row (first offset)))
            (c (+ col (second offset))))
        (declare (type fixnum r c))
        (when (and (in-board-p r c)
                   (eq (aref (board-board board) r c) king-piece))
          (return t))))))


(defun attacked-by-sliding-p (board row col by-color)
  "True if (ROW,COL) is attacked by a sliding piece of BY-COLOR.

Rooks attack along ranks and files; bishops attack along diagonals; queens
attack along both.  The attack must not be blocked by an intervening piece."
  (declare (type board board) (type fixnum row col) (type symbol by-color)
           (optimize (speed 3) (safety 2)))
  (multiple-value-bind (rook-pieces bishop-pieces) (rook-and-bishop-pieces by-color)
    (dolist (direction +queen-directions+)
      (let* ((dr (first direction))
             (dc (second direction))
             (target-pieces (if (or (= dr 0) (= dc 0))
                                rook-pieces
                                bishop-pieces)))
        (do ((r (+ row dr) (+ r dr))
             (c (+ col dc) (+ c dc)))
            ((not (in-board-p r c)))
          (let ((piece (aref (board-board board) r c)))
            (cond ((eq piece 'vv) nil)
                  ((member piece target-pieces) (return-from attacked-by-sliding-p t))
                  (t (return)))))))))


;;;; Pin-aware / checkers-aware legal move filtering
;;;
;;; LEGAL-MOVES (below) used to filter CHILDREN's pseudo-legal list by making
;;; every single candidate, doing a full board rescan for king safety, then
;;; unmaking it -- a full SQUARE-ATTACKED-P scan per candidate move.  Profiling
;;; (compilation/g8-profile-postchange2.log) showed this at 91% of search time,
;;; with 8.6-9.2 million KING-VALID/KING-THREATENED/SQUARE-ATTACKED-P calls for
;;; only ~304k LEGAL-MOVES calls.
;;;
;;; LEGAL-MOVES-FAST replaces the per-candidate rescan with CHECKERS-OF and
;;; PIN-MAP, each computed once per node, then filters pseudo-legal moves with
;;; O(1)/O(ray-length) geometry.  It is validated against the unchanged
;;; LEGAL-MOVES by an exhaustive differential perft check (see
;;; benchmark/g13-differential-legal-moves.lisp) before anything calls it.

(defun slider-piece-p (piece)
  "True if PIECE is a rook, bishop or queen (moves along a ray)."
  (declare (type symbol piece) (optimize (speed 3) (safety 2)))
  (member piece '(tb tn ab an db dn)))


(defun checkers-of (board king-row king-col by-color)
  "Return NIL if (KING-ROW,KING-COL) is not attacked by BY-COLOR, a single
attacker POS if attacked by exactly one piece, or :DOUBLE if attacked by two
or more.  A reachable chess position never has more than two simultaneous
checkers, but callers only ever test for \">= 2\" (via :DOUBLE), so that
invariant is never assumed for correctness -- scanning simply stops early."
  (declare (type board board) (type fixnum king-row king-col)
           (type symbol by-color) (optimize (speed 3) (safety 2)))
  (let ((found nil))
    (flet ((note (r c)
             (if found
                 (return-from checkers-of :double)
                 (setf found (make-pos :row r :col c)))))
      (let ((dir (pawn-attack-direction by-color))
            (pawn-piece (if (eq by-color 'white) 'pb 'pn)))
        (dolist (dc '(-1 1))
          (let ((r (- king-row dir)) (c (+ king-col dc)))
            (when (and (in-board-p r c)
                       (eq (aref (board-board board) r c) pawn-piece))
              (note r c)))))
      (let ((knight-piece (if (eq by-color 'white) 'cb 'cn)))
        (dolist (offset +knight-deltas+)
          (let ((r (+ king-row (first offset))) (c (+ king-col (second offset))))
            (when (and (in-board-p r c)
                       (eq (aref (board-board board) r c) knight-piece))
              (note r c)))))
      (let ((king-piece (if (eq by-color 'white) 'rb 'rn)))
        (dolist (offset +king-deltas+)
          (let ((r (+ king-row (first offset))) (c (+ king-col (second offset))))
            (when (and (in-board-p r c)
                       (eq (aref (board-board board) r c) king-piece))
              (note r c)))))
      (multiple-value-bind (rook-pieces bishop-pieces) (rook-and-bishop-pieces by-color)
        (dolist (direction +queen-directions+)
          (let* ((dr (first direction))
                 (dc (second direction))
                 (target-pieces (if (or (= dr 0) (= dc 0)) rook-pieces bishop-pieces)))
            (do ((r (+ king-row dr) (+ r dr))
                 (c (+ king-col dc) (+ c dc)))
                ((not (in-board-p r c)))
              (let ((piece (aref (board-board board) r c)))
                (cond ((eq piece 'vv) nil)
                      ((member piece target-pieces) (note r c) (return))
                      (t (return))))))))
      found)))


(defun pin-map (board color)
  "Return an 8x8 array; entry (R,C) is (DR . DC) -- the ray direction from
COLOR's king through square (R,C) -- when the piece on (R,C) is pinned
against COLOR's king by an enemy slider, or NIL otherwise.  Computed once per
LEGAL-MOVES-FAST call by walking each of the 8 rays out of the king square."
  (declare (type board board) (type symbol color) (optimize (speed 3) (safety 2)))
  (let* ((king-pos (if (eq color 'white)
                       (board-white-king-pos board)
                       (board-black-king-pos board)))
         (king-row (pos-row king-pos))
         (king-col (pos-col king-pos))
         (own-piece-p (if (eq color 'white) #'white-piece-p #'black-piece-p))
         (result (make-array '(8 8) :initial-element nil)))
    (multiple-value-bind (rook-pieces bishop-pieces) (rook-and-bishop-pieces (invert-color color))
      (dolist (direction +queen-directions+)
        (let* ((dr (first direction))
               (dc (second direction))
               (target-pieces (if (or (= dr 0) (= dc 0)) rook-pieces bishop-pieces))
               (friendly-r nil)
               (friendly-c nil))
          (do ((r (+ king-row dr) (+ r dr))
               (c (+ king-col dc) (+ c dc)))
              ((not (in-board-p r c)))
            (let ((piece (aref (board-board board) r c)))
              (cond ((eq piece 'vv) nil)
                    ((funcall own-piece-p piece)
                     (if friendly-r
                         (return)
                         (setf friendly-r r friendly-c c)))
                    ((member piece target-pieces)
                     (when friendly-r
                       (setf (aref result friendly-r friendly-c) (cons dr dc)))
                     (return))
                    (t (return)))))))
      result)))


(defun pin-violated-p (pins from to king-pos)
  "True if moving the piece at FROM to TO would move it off its pin ray, per
PINS (a PIN-MAP array).  NIL (not violated) when FROM is not pinned."
  (declare (type (simple-array t (8 8)) pins) (type pos from to king-pos)
           (optimize (speed 3) (safety 2)))
  (let ((entry (aref pins (pos-row from) (pos-col from))))
    (when entry
      (let ((dr (car entry))
            (dc (cdr entry))
            (delta-row (- (pos-row to) (pos-row king-pos)))
            (delta-col (- (pos-col to) (pos-col king-pos))))
        (declare (type fixnum dr dc delta-row delta-col))
        (/= 0 (- (* dr delta-col) (* dc delta-row)))))))


(defun on-checker-ray-p (king-pos checker-pos to)
  "True if TO lies strictly between KING-POS and CHECKER-POS on the ray
connecting them -- a legal blocking square for a sliding check.  Only
meaningful when the checking piece is a slider; callers gate on that.

Collinearity alone is not enough: TO must also be on the SAME SIDE of the
king as the checker, not behind it on the opposite ray.  The dot product of
the two delta vectors is positive exactly when they point the same way; a
prior version omitted this and treated a square on the far side of the king,
as long as it was on the same infinite line, as a legal block."
  (declare (type pos king-pos checker-pos to) (optimize (speed 3) (safety 2)))
  (let* ((delta-row (- (pos-row to) (pos-row king-pos)))
         (delta-col (- (pos-col to) (pos-col king-pos)))
         (check-delta-row (- (pos-row checker-pos) (pos-row king-pos)))
         (check-delta-col (- (pos-col checker-pos) (pos-col king-pos))))
    (and (zerop (- (* check-delta-row delta-col) (* check-delta-col delta-row)))
         (> (+ (* delta-row check-delta-row) (* delta-col check-delta-col)) 0)
         (let ((to-steps (max (abs delta-row) (abs delta-col)))
               (checker-steps (max (abs check-delta-row) (abs check-delta-col))))
           (< to-steps checker-steps)))))


(defun en-passant-move-p (board move)
  "True if MOVE is a pawn capturing en passant: a diagonal pawn move to an
empty target square on a currently marked en-passant file, with the captured
pawn really present behind the target."
  (declare (type board board) (type move move) (optimize (speed 3) (safety 2)))
  (let ((piece (board-pos (board-board board) (move-from move)))
        (to (move-to move)))
    (and (pawnp piece)
         (/= (pos-col (move-from move)) (pos-col to))
         (eq (board-pos (board-board board) to) 'vv)
         (cond ((eq piece 'pb)
                (and (= (pos-row to) 2)
                     (eq (aref (board-blacks-enpass board) (pos-col to)) '1)
                     (eq (aref (board-board board) 3 (pos-col to)) 'pn)))
               ((eq piece 'pn)
                (and (= (pos-row to) 5)
                     (eq (aref (board-whites-enpass board) (pos-col to)) '1)
                     (eq (aref (board-board board) 4 (pos-col to)) 'pb)))))))


(defun legal-moves (board color)
  "Return all legal moves for COLOR on BOARD.

Pin-aware / checkers-aware filter: computes checkers and pinned pieces once
per call instead of doing a full make/unmake king-safety rescan per candidate
move (the former approach, ~91% of search time per
compilation/g8-profile-postchange2.log).  King moves (including castling) and
en-passant captures always fall back to the exact KING-VALID make/unmake
check: king moves are few, and en passant is the only move that removes two
pieces from one rank at once, which breaks the single-ray assumption the
pin/checker model relies on (the classic horizontal discovered-check
pattern).  Validated by an exhaustive per-node differential perft check
against the prior full-rescan implementation before switch-over
(benchmark/g13-differential-legal-moves.lisp: 10.19M nodes, zero mismatches,
covering single check/double check/pins/en passant/castling)."
  (declare (type board board) (type symbol color) (optimize (speed 3) (safety 2)))
  (let* ((king-pos (if (eq color 'white)
                       (board-white-king-pos board)
                       (board-black-king-pos board)))
         (checkers (checkers-of board (pos-row king-pos) (pos-col king-pos)
                                 (invert-color color)))
         (pins (unless (eq checkers :double)
                 (pin-map board color)))
         (result '()))
    (dolist (mv (children board color
                          :castle (and checkers 'without-castling)
                          ;; CHECKERS is non-NIL iff COLOR is in check; thread
                          ;; that status into POSSIBLE-KING so it does not
                          ;; recompute KING-THREATENED (a full attack scan) on
                          ;; every node.  POSSIBLE-KING is only reached here
                          ;; when NOT in check, so this passes NIL in that case.
                          :in-check-known (not (null checkers)))
                (nreverse result))
      (let ((from (move-from mv))
            (to (move-to mv)))
        (cond
          ((equal-pos from king-pos)
           (when (king-valid board mv color) (push mv result)))
          ((eq checkers :double) nil)
          ((en-passant-move-p board mv)
           (when (king-valid board mv color) (push mv result)))
          (checkers
           (let ((checker-piece (board-pos (board-board board) checkers)))
             (when (and (or (equal-pos to checkers)
                            (and (slider-piece-p checker-piece)
                                 (on-checker-ray-p king-pos checkers to)))
                        (not (pin-violated-p pins from to king-pos)))
               (push mv result))))
          (t
           (unless (pin-violated-p pins from to king-pos)
             (push mv result))))))))


(defun valid (board move color)
  (declare (type board board) (type move move) (type symbol color)
           (optimize (speed 3) (safety 2)))
  (and (member move (children board color) :test #'equal-moves)
       (king-valid board move color)))


;; A single reusable scratch UNDO record for KING-VALID.  Legality testing
;; makes a move, tests for check via SQUARE-ATTACKED-P (which only scans the
;; board -- it never executes a move or re-enters KING-VALID), then unmakes.
;; So exactly one KING-VALID make/unmake is ever in flight, and the engine is
;; single-threaded; reusing one record is therefore safe and removes the
;; per-pseudo-move MOVE-UNDO + en-passant-array allocation that dominated the
;; G4 hot-path profile (KING-VALID drove ~8.66M of ~8.97M EXECUTE-MOVE calls).
(defvar *king-valid-undo* (make-move-undo)
  "Scratch UNDO record reused by KING-VALID; see the note above.")

(defun king-valid (board move color)
  "Return true if MOVE leaves COLOR's king not in check.

Uses make/unmake on the same board instead of copying, reusing the shared
scratch record *KING-VALID-UNDO* to avoid per-move allocation.  LEGAL-MOVES may
be asked about a colour other than BOARD-SIDE-TO-MOVE (tests and diagnostics do
this), while EXECUTE-MOVE derives the moving colour from BOARD-SIDE-TO-MOVE.  Bind
that slot around the simulation and restore it afterwards so the legality check
stays side-effect-free for arbitrary COLOR."
  (declare (type board board) (type move move) (type symbol color)
           (optimize (speed 3) (safety 2)))
  (let ((original-side (board-side-to-move board))
        (executed nil))
    (setf (board-side-to-move board) color)
    (unwind-protect
         (progn
           (execute-move board move *king-valid-undo*)
           (setf executed t)
           (null (king-threatened board color)))
      (when executed
        (unmake-move board move *king-valid-undo*))
      (setf (board-side-to-move board) original-side))))


(defun king-threatened (board color)
  (declare (type board board) (type symbol color) (optimize (speed 3) (safety 2)))
  (let ((king-pos (if (eq color 'white)
                      (board-white-king-pos board)
                      (board-black-king-pos board))))
    (square-attacked-p board (pos-row king-pos) (pos-col king-pos)
                       (invert-color color))))


(defun checkp (board color)
    (declare (optimize (speed 3) (safety 2)))
(king-threatened board color))


(defun checkmatep (board color)
    (declare (optimize (speed 3) (safety 2)))
(and (checkp board color)
       (null (legal-moves board color))))


(defun stalematep (board color)
    (declare (optimize (speed 3) (safety 2)))
(and (not (checkp board color))
       (null (legal-moves board color))))


(defun game-over-p (board color)
  "Return a status symbol if the game is over for the side to move."
    (declare (optimize (speed 3) (safety 2)))
(cond ((checkmatep board color) 'checkmate)
        ((stalematep board color) 'stalemate)
        ((drawp board) 'draw)
        (t nil)))
