;;; -*- encoding: utf-8 -*-
;;;
;;; io.lisp
;;; Input/Output and board display.
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


;;;; Piece display names

(defvar *pieces* (make-hash-table))

(setf (gethash 'tn *pieces*) "TN")
(setf (gethash 'tb *pieces*) "TB")
(setf (gethash 'cn *pieces*) "CN")
(setf (gethash 'cb *pieces*) "CB")
(setf (gethash 'an *pieces*) "AN")
(setf (gethash 'ab *pieces*) "AB")
(setf (gethash 'rn *pieces*) "RN")
(setf (gethash 'rb *pieces*) "RB")
(setf (gethash 'dn *pieces*) "DN")
(setf (gethash 'db *pieces*) "DB")
(setf (gethash 'pn *pieces*) "PN")
(setf (gethash 'pb *pieces*) "PB")
(setf (gethash 'vv *pieces*) "..")


;;;; Board display (for console debugging)

(defun show-board (board)
  "Print BOARD in the legacy console display format."
  (declare (optimize (speed 3) (safety 2)))
  (format t "~%     A    B    C    D    E    F    G    H~%~%")
  (dotimes (row 8)
    (format t " ~A  " (- 8 row))
    (dotimes (column 8)
      (format t "~A   " (gethash (aref (board-board board) row column)
                                   *pieces*)))
    (format t "~%~%")))


;;;; Human move input for the console game

(defun read-move (board)
  "Prompt for and return a move typed as two squares, e.g. \"A2 A4\".
BOARD is consulted only to auto-select queen promotion when a pawn move
lands on the last rank (matching EXECUTE-MOVE's own nil-promotion default),
since this square-pair notation has no way to type a promotion piece."
  (declare (optimize (speed 3) (safety 2)))
  (loop
     (format t "Move: ")
     ;; Interactive console input only (never reached on the UCI path, which
     ;; parses strings without READ). Bind *READ-EVAL* to NIL so a #. reader
     ;; macro in typed input cannot trigger read-time evaluation; legitimate
     ;; square tokens (e.g. A1 B2) read as symbols and are unaffected.
     ;; END-OF-FILE is re-signalled (propagates to MAIN's HANDLER-CASE for a
     ;; clean exit on closed stdin) while any other reader error just reprompts.
     (handler-case
         (let* ((*read-eval* nil)
                (mov1 (read))
                (mov2 (read))
                (row-from nil)
                (column-from nil)
                (row-to nil)
                (column-to nil))
           (when (= 2 (length (string mov1)))
             (let ((r (digit-char-p (aref (string mov1) 1))))
               (when (and r (<= 1 r 8)) (setf row-from r)))
             (setf column-from (position (aref (string mov1) 0) "ABCDEFGH"
                                         :test (function string-equal))))
           (when (= 2 (length (string mov2)))
             (let ((r (digit-char-p (aref (string mov2) 1))))
               (when (and r (<= 1 r 8)) (setf row-to r)))
             (setf column-to (position (aref (string mov2) 0) "ABCDEFGH"
                                       :test (function string-equal))))
           (when (and row-from column-from row-to column-to)
             (let* ((from (make-pos :row (convert-rank row-from) :col column-from))
                    (to (make-pos :row (convert-rank row-to) :col column-to))
                    (piece (aref (board-board board) (pos-row from) (pos-col from)))
                    (promotes (or (and (eq piece 'pb) (= (pos-row to) 0))
                                  (and (eq piece 'pn) (= (pos-row to) 7)))))
               (return (make-move :from from :to to
                                  :promotion (if promotes 'q nil))))))
       (end-of-file (condition) (error condition))
       (error () nil))))
