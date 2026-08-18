;;; -*- encoding: utf-8 -*-
;;;
;;; miguedrez.asd
;;; ASDF system definition for the Miguedrez chess engine.
;;;

;; Copyright 2007, 2008, 2009 Manuel Felipe Gamallo Rivero.
;; Copyright 2026 Arthur Matheus (UCI port and further releases).

;;     This file is part of Miguedrez.

;;     Miguedrez is free software: you can redistribute it and/or modify
;;     it under the terms of the GNU General Public License as published by
;;     the Free Software Foundation, either version 2 of the License, or
;;     (at your option) any later version.

;;     Miguedrez is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;     GNU General Public License for more details.

;;     You should have received a copy of the GNU General Public License
;;     along with Miguedrez.  If not, see <http://www.gnu.org/licenses/>.

(asdf:defsystem "miguedrez"
  :description "Miguedrez chess engine, UCI compatible."
  :version "0.95.11.5"
  :author "Manuel Felipe Gamallo Rivero, 0.95 and Arthur Matheus further releases"
  :license "GPLv2"
  :serial t
  :components
  ((:module "src"
            :components
            ((:file "packages")
             (:file "data")
             (:file "draw")
             (:module "moves"
                      :components
                      ((:file "moves")
                       (:file "pawn")
                       (:file "knight")
                       (:file "bishop")
                       (:file "rook")
                       (:file "queen")
                       (:file "king")))
             (:file "ai")
             (:file "io")
             (:file "uci")
             (:file "cli")
             (:file "tests"))))
  :in-order-to ((asdf:test-op (asdf:test-op "miguedrez/tests"))))

(asdf:defsystem "miguedrez/tests"
  :description "Self-tests for Miguedrez."
  :version "0.95.11.5"
  :author "Manuel Felipe Gamallo Rivero, 0.95 and Arthur Matheus further releases"
  :license "GPLv2"
  :serial t
  :depends-on ("miguedrez")
  :components
  ((:module "src"
            :components
            ((:file "tests"))))
  :perform (asdf:test-op (o c)
             (uiop:symbol-call '#:miguedrez '#:run-self-tests)))
