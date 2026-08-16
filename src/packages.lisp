;;; -*- encoding: utf-8 -*-
;;;
;;; packages.lisp
;;; Package and export definitions.
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

;; Canonical project-wide optimization policy (IMPLEMENTATION_PLAN §0 rule 4).
;; Proclaimed globally here, in the first-loaded file, so every subsequent file
;; inherits the same default instead of relying on a mid-build declaim that
;; would silently leak across files. Hot functions add a local
;; (declare (optimize ...)) to raise safety where they need it.
(declaim (optimize (speed 3) (safety 1) (debug 1) (space 1) (compilation-speed 0)))

(defpackage #:miguedrez
  (:use #:cl)
  (:export
   ;; UCI / executable entry point
   #:main
   #:run-self-tests
   ;; Conditions
   #:invalid-fen
   #:invalid-move
   ;; Board and data types
   #:board
   #:player
   #:pos
   #:move
   ;; Position setup
   #:parse-fen
   #:initial-board
   ;; Move helpers
   #:move-to-uci
   #:uci-to-move
   #:legal-moves
   #:make-move-on-board
   #:game-over-p))

(in-package #:miguedrez)
