;;; -*- encoding: utf-8 -*-
;;;
;;; rook.lisp
;;; Movements of the rooks.
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


;;;; Rook move generation

(defparameter +rook-directions+ '((-1 0) (1 0) (0 -1) (0 1))
  "The 4 orthogonal ray directions shared by both rook-move generators.")

(defun possible-white-rook (board pos)
  "Return all pseudo-legal white-rook moves from POS."
  (declare (type board board) (type pos pos)
           (optimize (speed 3) (safety 2)))
  (scan-rays board pos (board-blacks board) +rook-directions+))


(defun possible-black-rook (board pos)
  "Return all pseudo-legal black-rook moves from POS."
  (declare (type board board) (type pos pos)
           (optimize (speed 3) (safety 2)))
  (scan-rays board pos (board-whites board) +rook-directions+))


(defun possible-rook (board pos)
  "Return all pseudo-legal rook moves from POS, dispatching by piece colour."
  (declare (type board board) (type pos pos) (optimize (speed 3) (safety 2)))
  (if (eq (aref (board-board board) (pos-row pos) (pos-col pos)) 'tb)
      (possible-white-rook board pos)
      (possible-black-rook board pos)))
