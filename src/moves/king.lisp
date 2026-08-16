;;; -*- encoding: utf-8 -*-
;;;
;;; king.lisp
;;; Movements of kings, including castling.
;;; gamallo, July 25, 2007
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


;;;; King move generation

(defun square-empty-p (board row col)
  "True if BOARD contains no piece at ROW,COL."
  (declare (type board board) (type fixnum row col) (optimize (speed 3) (safety 2)))
  (eq (aref (board-board board) row col) 'vv))

(defun square-occupied-by-p (board row col piece)
  "True if BOARD contains PIECE at ROW,COL."
  (declare (type board board) (type fixnum row col) (type symbol piece)
           (optimize (speed 3) (safety 2)))
  (eq (aref (board-board board) row col) piece))

(defun occupied-by-opponent (board row col color)
  "True if ROW,COL is occupied by COLOR's opponent on BOARD."
  (declare (type board board) (type fixnum row col) (type symbol color)
           (optimize (speed 3) (safety 2)))
  (color-occupied-p (if (eq color 'white) (board-blacks board) (board-whites board))
                     row col))

(defun possible-king-without-castling (board pos &optional color)
  "Return all non-castling king moves from POS. COLOR may be passed by a
caller (POSSIBLE-KING) that already computed it, to avoid re-deriving it
from BOARD; when omitted (the CHILDREN-PIECE :WITHOUT-CASTLING call site)
it is derived here as before."
  (declare (type board board) (type pos pos) (type (or null symbol) color)
           (optimize (speed 3) (safety 2)))
  (let ((moves '())
        (color (or color
                   (if (eq (aref (board-board board)
                                 (pos-row pos) (pos-col pos))
                           'rb)
                       'white
                       'black)))
        (row (pos-row pos))
        (col (pos-col pos)))
    (declare (type symbol color) (type fixnum row col))
    (dolist (offset +king-deltas+)
      (let ((r (+ row (first offset)))
            (c (+ col (second offset))))
        (declare (type fixnum r c))
        (when (and (in-board-p r c)
                   (or (square-empty-p board r c)
                       (occupied-by-opponent board r c color)))
          (add-move moves pos r c))))
    moves))


(defun possible-king (board pos &optional (in-check-known :unknown))
  "Return all pseudo-legal king moves from POS, including castling when legal.

IN-CHECK-KNOWN is COLOR's KING-THREATENED status at POS, supplied by a caller
that already computed it (LEGAL-MOVES derives it from CHECKERS-OF).  It defaults
to :UNKNOWN, in which case KING-THREATENED is computed here -- a full
pawn/knight/king/sliding-attack scan.  LEGAL-MOVES is the per-node hot path and
already knows the in-check answer (CHECKERS is non-NIL iff in check), and
POSSIBLE-KING is only reached from it when NOT in check (in-check cases route
to POSSIBLE-KING-WITHOUT-CASTLING), so threading the known status avoids a
redundant full-board attack scan on every node.

When supplied, IN-CHECK-KNOWN is also shared with both castle-check functions
below instead of each recomputing it -- both castling rights are typically still
available for many opening/early-middlegame nodes, so without sharing this every
POSSIBLE-KING call would pay for the scan twice."
  (declare (type board board) (type pos pos)
           (optimize (speed 3) (safety 2)))
  (let* ((color (if (eq (aref (board-board board)
                              (pos-row pos) (pos-col pos))
                        'rb)
                    'white
                    'black))
         (in-check (if (eq in-check-known :unknown)
                       (king-threatened board color)
                       in-check-known)))
    (nconc (possible-king-without-castling board pos color)
           (short-castle-if-possible board color pos in-check)
           (long-castle-if-possible board color pos in-check))))


(defun castle-rights (board color)
  "Return (VALUES BASE-ROW KING-MOVED KINGSIDE-ROOK-MOVED QUEENSIDE-ROOK-MOVED
ROOK-PIECE), the castling geometry and rights that differ between COLOR's
side -- BASE-ROW is 7 for white, 0 for black, mirroring HANDLE-CASTLING's
own base-row parameterization for castling execution."
  (declare (type board board) (type symbol color) (optimize (speed 3) (safety 2)))
  (if (eq color 'white)
      (values 7 (board-white-king-moved board)
              (board-white-kingside-rook-moved board)
              (board-white-queenside-rook-moved board) 'tb)
      (values 0 (board-black-king-moved board)
              (board-black-kingside-rook-moved board)
              (board-black-queenside-rook-moved board) 'tn)))

(defun short-castle-if-possible (board color pos in-check)
  "Return a list containing the kingside castling move, or NIL.
IN-CHECK is COLOR's KING-THREATENED status at POS, computed once by
POSSIBLE-KING and shared with LONG-CASTLE-IF-POSSIBLE."
  (declare (type board board) (type symbol color) (type pos pos)
           (type boolean in-check) (optimize (speed 3) (safety 2)))
  (multiple-value-bind (base-row king-moved kingside-rook-moved queenside-rook-moved rook-piece)
      (castle-rights board color)
    (declare (ignore queenside-rook-moved) (type fixnum base-row))
    (let ((attacker (invert-color color)))
      (when (and (= (pos-row pos) base-row) (= (pos-col pos) 4)
                 (not king-moved)
                 (not kingside-rook-moved)
                 (square-empty-p board base-row 5)
                 (square-empty-p board base-row 6)
                 (square-occupied-by-p board base-row 7 rook-piece)
                 (not in-check)
                 ;; The king may not pass through or into check.
                 (not (square-attacked-p board base-row 5 attacker))
                 (not (square-attacked-p board base-row 6 attacker)))
        (list (make-move :from pos :to (make-pos :row base-row :col 6)))))))


(defun long-castle-if-possible (board color pos in-check)
  "Return a list containing the queenside castling move, or NIL.
IN-CHECK is COLOR's KING-THREATENED status at POS, computed once by
POSSIBLE-KING and shared with SHORT-CASTLE-IF-POSSIBLE."
  (declare (type board board) (type symbol color) (type pos pos)
           (type boolean in-check) (optimize (speed 3) (safety 2)))
  (multiple-value-bind (base-row king-moved kingside-rook-moved queenside-rook-moved rook-piece)
      (castle-rights board color)
    (declare (ignore kingside-rook-moved) (type fixnum base-row))
    (let ((attacker (invert-color color)))
      (when (and (= (pos-row pos) base-row) (= (pos-col pos) 4)
                 (not king-moved)
                 (not queenside-rook-moved)
                 ;; All squares between the king (e-file) and rook (a-file) must
                 ;; be empty: b, c, d. The a-file rook slides across b, so b
                 ;; must be empty even though the king never crosses it.
                 (square-empty-p board base-row 1)
                 (square-empty-p board base-row 2)
                 (square-empty-p board base-row 3)
                 (square-occupied-by-p board base-row 0 rook-piece)
                 (not in-check)
                 ;; The king passes through the d- and c-files; both must be safe.
                 (not (square-attacked-p board base-row 3 attacker))
                 (not (square-attacked-p board base-row 2 attacker)))
        (list (make-move :from pos :to (make-pos :row base-row :col 2)))))))
