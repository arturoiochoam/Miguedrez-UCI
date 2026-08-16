;;; -*- encoding: utf-8 -*-
;;;
;;; pawn.lisp
;;; Pawn movements.
;;; gamallo, March 22, 2007
;;; UCI port and cleanup by Arthur Matheus, 2026.
;;;

;; Copyright 2007, 2008, 2009 Manuel Felipe Gamallo Rivero.
;; Copyright 2026 Arthur Matheus (UCI port and further releases).

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


;;;; Pawn move generation

(defun possible-pawn (board pos)
  "Return all pseudo-legal pawn moves from POS, including captures, two-square
pushes, en-passant and promotions. Dispatches on `(eq piece 'pb)` and
assumes the else branch is 'pn (a black pawn) -- safe today because the
sole caller, CHILDREN-PIECE (src/moves/moves.lisp), only invokes this when
the square holds 'pb or 'pn, the same not-white-implies-black convention
used by every other piece's dispatcher (knight/bishop/rook/queen/king).

White and black differ only in which way is \"forward\": DIR, START-ROW,
PROMO-ROW, EP-TARGET-ROW and the opponent occupancy/en-passant arrays to
read are all derived from color once, below, instead of hand-duplicating
the whole move-generation body per color."
  (declare (type board board) (type pos pos)
           (optimize (speed 3) (safety 2)))
  (let* ((moves '())
         (piece (board-pos (board-board board) pos))
         (row (pos-row pos))
         (col (pos-col pos))
         (white-p (eq piece 'pb))
         (color (piece-color piece))
         (dir (pawn-attack-direction color))
         (start-row (if white-p 6 1))
         (promo-row (if white-p 0 7))
         (ep-target-row (if white-p 2 5))
         (opponent-pawn (if white-p 'pn 'pb))
         (opponent-array (if white-p (board-blacks board) (board-whites board)))
         (opponent-enpass (if white-p (board-blacks-enpass board) (board-whites-enpass board))))
    (declare (type symbol piece color opponent-pawn) (type fixnum row col dir start-row promo-row ep-target-row)
             (type (simple-array t (8 8)) opponent-array)
             (type (simple-array t (8)) opponent-enpass))
    ;; One square forward.
    (let ((r1 (+ row dir)))
      (declare (type fixnum r1))
      (when (and (in-board-p r1 col)
                 (eq (aref (board-board board) r1 col) 'vv))
        (if (= r1 promo-row)
            (add-promotion-moves moves pos r1 col)
            (add-move moves pos r1 col))))
    ;; Two-square push from starting rank.
    (when (= row start-row)
      (let ((r1 (+ row dir))
            (r2 (+ row dir dir)))
        (declare (type fixnum r1 r2))
        (when (and (eq (aref (board-board board) r1 col) 'vv)
                   (eq (aref (board-board board) r2 col) 'vv))
          (add-move moves pos r2 col))))
    ;; Captures, left then right.
    (dolist (dc '(-1 1))
      (let ((target-row (+ row dir))
            (target-col (+ col dc)))
        (declare (type fixnum target-row target-col))
        (when (in-board-p target-row target-col)
          (when (or (eq (aref opponent-array target-row target-col) '1)
                    (and (= target-row ep-target-row)
                         (eq (aref opponent-enpass target-col) '1)
                         (eq (aref (board-board board) row target-col) opponent-pawn)))
            (if (= target-row promo-row)
                (add-promotion-moves moves pos target-row target-col)
                (add-move moves pos target-row target-col))))))
    moves))
