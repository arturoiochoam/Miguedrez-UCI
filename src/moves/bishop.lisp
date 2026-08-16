;;; -*- encoding: utf-8 -*-
;;;
;;; bishop.lisp
;;; Movements of the bishops.
;;; gamallo, April 2, 2007
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


;;;; Bishop move generation

(defparameter +bishop-directions+ '((-1 1) (1 1) (1 -1) (-1 -1))
  "The 4 diagonal ray directions shared by both bishop-move generators.")

(defun possible-white-bishop (board pos)
  "Return all pseudo-legal white-bishop moves from POS."
  (declare (type board board) (type pos pos)
           (optimize (speed 3) (safety 2)))
  (scan-rays board pos (board-blacks board) +bishop-directions+))


(defun possible-black-bishop (board pos)
  "Return all pseudo-legal black-bishop moves from POS."
  (declare (type board board) (type pos pos)
           (optimize (speed 3) (safety 2)))
  (scan-rays board pos (board-whites board) +bishop-directions+))


(defun possible-bishop (board pos)
  "Return all pseudo-legal bishop moves from POS, dispatching by piece colour."
  (declare (type board board) (type pos pos) (optimize (speed 3) (safety 2)))
  (if (eq (aref (board-board board) (pos-row pos) (pos-col pos)) 'ab)
      (possible-white-bishop board pos)
      (possible-black-bishop board pos)))
