;;; -*- encoding: utf-8 -*-
;;;
;;; draw.lisp
;;; Draw-rule detection: threefold repetition, 50-move rule and
;;; insufficient material.
;;; Arthur Matheus, 2026.
;;;

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


;;;; Position key for repetition detection

(defun game-position-key (board)
  "Return an equal-comparable key for the current position.

The key is the incremental Zobrist hash stored on the board; it
includes piece placement, side to move, castling rights and en passant
square.  The halfmove-clock is intentionally excluded."
  (declare (type board board) (optimize (speed 3) (safety 2)))
  (board-key board))



(defun position-repeated-at-least-p (board needed)
  "True if the current position appears at least NEEDED times in history."
  (declare (type board board) (type fixnum needed) (optimize (speed 3) (safety 2)))
  (let ((key (game-position-key board))
        (count 0))
    (declare (type (unsigned-byte 64) key) (type fixnum count))
    (dolist (hist-key (board-position-history board) nil)
      (declare (type (unsigned-byte 64) hist-key))
      (when (= key hist-key)
        (incf count)
        (when (>= count needed)
          (return t))))))


(defun draw-by-repetition-p (board)
  "True if the current position has occurred three times."
  (declare (type board board) (optimize (speed 3) (safety 2)))
  (position-repeated-at-least-p board 2))


(defun draw-by-50-move-rule-p (board)
  "True if 50 consecutive reversible moves have occurred (halfmove-clock >= 100)."
  (declare (type board board) (optimize (speed 3) (safety 2)))
  (>= (board-halfmove-clock board) 100))


;;;; Insufficient material

(defun insufficient-material-p (board)
  "Return true for positions that are automatically drawn under FIDE rules.

Covered cases:
  K-K
  KB-K (single bishop against lone king)
  KN-K (single knight against lone king)
  KB-KB with same-colour bishops (both bishops on one colour complex;
    no mate is possible, so the position is dead)
  KBB-K with same-colour bishops (any number of bishops confined to one
    colour complex against a lone king is dead for the same reason as
    KB-K -- the king can always retreat to the opposite colour complex)

Not covered (checkmate is still possible with cooperative play, so these
are NOT dead positions under FIDE Art. 5.2.2):
  KNN-K (two knights can cooperatively mate a lone king; it merely cannot
    be forced), KN-KN, BN-BN, KB-KB with opposite-colour bishops, or any
  position containing a pawn, rook or queen."
  (declare (type board board) (optimize (speed 3) (safety 2)))
  (let ((white '())
        (black '()))
    (dotimes (row 8)
      (dotimes (col 8)
        (let ((piece (aref (board-board board) row col)))
          ;; A pawn, rook or queen anywhere always makes every COND branch
          ;; below fall through to NIL (none of them match a piece outside
          ;; cb/cn/ab/an, and none match a non-zero count of anything else),
          ;; so bail out here before consing a full piece list for what is
          ;; overwhelmingly the common case (opening/middlegame positions).
          (case piece
            ((pb pn tb tn db dn) (return-from insufficient-material-p nil))
            (vv nil)
            (t (if (white-piece-p piece)
                   (push (list piece row col) white)
                   (push (list piece row col) black)))))))
    ;; Both sides must have exactly one king.
    (unless (and (= (count 'rb white :key #'first) 1)
                 (= (count 'rn black :key #'first) 1))
      (return-from insufficient-material-p nil))
    ;; Strip the kings from the material count.
    (setf white (remove 'rb white :key #'first))
    (setf black (remove 'rn black :key #'first))
    (let ((white-count (length white))
          (black-count (length black)))
      (cond ((and (= white-count 0) (= black-count 0)) t)
            ;; K vs KN / KB
            ((and (= white-count 0) (= black-count 1)
                  (member (first (first black)) '(cn an)))
             t)
            ((and (= black-count 0) (= white-count 1)
                  (member (first (first white)) '(cb ab)))
             t)
            ;; KNN-K (two knights vs lone king) is intentionally NOT here: a
            ;; helpmate exists, so it is not a dead position. Returning a draw
            ;; would also mask a real KNN-K checkmate, because DRAWP is tested
            ;; before terminal-mate detection in the search.
            ;; KB-KB with both bishops on the same colour complex is a dead
            ;; draw (mate impossible). Opposite-colour KB-KB is NOT dead --
            ;; a helpmate exists -- so it is deliberately excluded here.
            ((and (= white-count 1) (= black-count 1)
                  (eq (first (first white)) 'ab)
                  (eq (first (first black)) 'an)
                  (= (bishop-square-color (second (first white)) (third (first white)))
                     (bishop-square-color (second (first black)) (third (first black)))))
             t)
            ;; KBB-K / K-KBB: any number of same-side bishops, all on one
            ;; colour complex, against a lone king -- also dead, same reason
            ;; as the single-bishop case above.
            ((and (= black-count 0) (plusp white-count)
                  (every (lambda (p) (eq (first p) 'ab)) white)
                  (same-colour-complex-p white))
             t)
            ((and (= white-count 0) (plusp black-count)
                  (every (lambda (p) (eq (first p) 'an)) black)
                  (same-colour-complex-p black))
             t)
            (t nil)))))


(defun same-colour-complex-p (pieces)
  "True if every (PIECE ROW COL) entry in PIECES sits on the same bishop
colour complex."
  (declare (optimize (speed 3) (safety 2)))
  (let ((first-colour (bishop-square-color (second (first pieces)) (third (first pieces)))))
    (every (lambda (p) (= (bishop-square-color (second p) (third p)) first-colour))
           pieces)))


(defun bishop-square-color (row col)
  "Return 0 or 1 according to the colour complex of square (ROW,COL)."
  (declare (type fixnum row col) (optimize (speed 3) (safety 2)))
  (mod (+ row col) 2))


;;;; Aggregate draw predicate

(defun drawp (board)
  "True if the position is drawn by repetition, 50-move rule or material."
  (declare (type board board) (optimize (speed 3) (safety 2)))
  (or (draw-by-repetition-p board)
      (draw-by-50-move-rule-p board)
      (insufficient-material-p board)))
