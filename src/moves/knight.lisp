;;; -*- encoding: utf-8 -*-
;;;
;;; knight.lisp
;;; Movements of the knights.
;;; gamallo, April 1, 2007
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


;;;; Knight move generation

(defun possible-knight (board pos)
  "Return all pseudo-legal knight moves from POS."
  (declare (type board board) (type pos pos)
           (optimize (speed 3) (safety 2)))
  (let ((moves '())
        (row (pos-row pos))
        (col (pos-col pos))
        (friendly-occupied (if (eq (aref (board-board board)
                                         (pos-row pos) (pos-col pos))
                                   'cb)
                               (board-whites board)
                               (board-blacks board))))
    (declare (type fixnum row col))
    (dolist (offset +knight-deltas+)
      (let ((r (+ row (first offset)))
            (c (+ col (second offset))))
        (declare (type fixnum r c))
        (when (and (in-board-p r c)
                   (eq (aref friendly-occupied r c) '0))
          (add-move moves pos r c))))
    moves))
