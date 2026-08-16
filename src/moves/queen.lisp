;;; -*- encoding: utf-8 -*-
;;;
;;; queen.lisp
;;; Movements of the queens.
;;; gamallo, July 9, 2007
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


;;;; Queen move generation

(defun possible-queen (board pos)
  "Return all pseudo-legal queen moves from POS with one sliding-ray scan."
  (declare (type board board) (type pos pos)
           (optimize (speed 3) (safety 2)))
  (let* ((piece (aref (board-board board) (pos-row pos) (pos-col pos)))
         (opponent-array (if (eq piece 'db)
                             (board-blacks board)
                             (board-whites board))))
    (declare (type (simple-array t (8 8)) opponent-array))
    (scan-rays board pos opponent-array +queen-directions+)))
