;;; -*- encoding: utf-8 -*-
;;;
;;; cli.lisp
;;; Command-line entry point: UCI engine or legacy console game.
;;; Arthur Matheus, 2026.
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


;;;; Legacy interactive game

(defun cli-ajz ()
  "Start the original console game: human white vs. computer black."
  (declare (optimize (speed 3) (safety 2)))
  (let* ((player1 (make-player :type 'manual :color 'white))
         (player2 (make-player :type 'auto :color 'black))
         (table (initial-board))
         (current-player player1)
         (current-move nil))
    (loop
       (show-board table)
       ;; Checkmate / stalemate detection
       (let ((status (game-over-p table (player-color current-player))))
         (when status
           (if (eq status 'draw)
               (format t "Game over: draw.~%")
               (format t "Game over: ~A for ~A.~%"
                       (if (eq status 'checkmate) "checkmate" "stalemate")
                       (player-color current-player)))
           (return)))
       (setf current-move (load-move table current-player))
       (if (valid table current-move (player-color current-player))
           (progn
             (execute-move table current-move)
             (setf current-player
                   (if (eq current-player player1) player2 player1)))
           (if (eq (player-type current-player) 'auto)
               (progn
                 (format t "Internal error: engine produced an illegal move.~%")
                 (return))
               (format t "Illegal move. Please try again.~%"))))))


(defun load-move (table player)
  "Return PLAYER's next move on TABLE, reading input or asking the engine."
  (declare (optimize (speed 3) (safety 2)))
  (if (eq (player-type player) 'manual)
      (read-move table)
      (choose-move table (player-color player))))


;;;; Command-line entry point

(defun command-line-arguments ()
  "Return the list of command-line arguments passed to the executable.
   This is SBCL-specific; it is isolated here for portability."
  (declare (optimize (speed 3) (safety 2)))
  #+sbcl sb-ext:*posix-argv*
  #-sbcl nil)


(defun exit-process (&optional (code 0))
  "Exit the current process with CODE.

ANSI Common Lisp has no process-exit API; keep the SBCL boundary in one helper
so callers do not refer to implementation packages directly."
  (declare (type fixnum code) (optimize (speed 3) (safety 2)))
  #+sbcl
  (sb-ext:exit :code code)
  #-sbcl
  (error "Process exit with code ~D is not supported by this implementation." code))


(defun run-with-clean-exit (thunk)
  "Call THUNK; a closed input stream (GUI/pipe closed, or stdin EOF in
console mode) exits cleanly instead of crashing with a raw backtrace."
  (declare (optimize (speed 3) (safety 2)))
  (handler-case
      (funcall thunk)
    (end-of-file ()
      (exit-process 0))
    (error (e)
      (format *error-output* "~A~%" e)
      (exit-process 1))))

(defun main ()
  "Dispatch between UCI mode, self-test and legacy console game."
  (declare (optimize (speed 3) (safety 2)))
  (let ((args (command-line-arguments)))
    (cond ((member "--self-test" args :test #'string-equal)
           (run-with-clean-exit #'run-self-tests))
          ((member "--console" args :test #'string-equal)
           (run-with-clean-exit #'cli-ajz))
          (t
           ;; Default UCI mode
           (run-with-clean-exit #'uci-loop))))
  ;; Normal UCI exit path.
  (exit-process 0))
