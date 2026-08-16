;;; -*- encoding: utf-8 -*-
;;;
;;; uci.lisp
;;; UCI protocol implementation.
;;; Arthur Matheus, 2026.
;;; 0.95.5 cleanups by Claude Code.
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


(defvar *uci-board* nil)
(defvar *uci-search-thread* nil
  "Background search thread, or NIL when no search is active.")

(defparameter +engine-name+ "Miguedrez UCI 0.95.11.5")
(defparameter +engine-author+ "Manuel Felipe Gamallo Rivero, 0.95 and Arthur Matheus further releases")

;; Iterative-deepening depth ceilings for `go`.  A set deadline (see
;; `uci-go-deadline`) or a `stop` command halts the search before the ceiling is
;; reached; the ceiling only bounds how deep an untimed/`infinite` search may run.
;; +uci-max-go-depth+ stays below +max-search-ply+ (128) with room for the
;; check-extension budget (max +maximum-check-extensions+ = 8 plies), so the
;; deepest reachable main-search ply (99 + 8 = 107) never overflows the
;; killer/history arrays.
(defparameter +uci-max-go-depth+ 99
  "Depth ceiling for `go infinite` and any timed `go` (stopped by stop/deadline).")
(defparameter +uci-default-go-depth+ 8
  "Depth ceiling for a bare `go` with no depth argument and no clock.")


;;;; UCI entry point

(defun stop-search-and-join ()
  "Signal any active search thread to stop and wait for it to finish.
Return true only when no live search remains.

If SBCL reports :TIMED-OUT, keep *UCI-SEARCH-THREAD* and the stop flag in place:
the command loop may continue serving non-mutating protocol commands, but callers
must not start a new search or resize shared TT/search state while that stale
worker can still be touching it.  A timed-out worker is NOT force-killed:
SBCL's DESTROY-THREAD mid-search can leave the shared *KING-VALID-UNDO* scratch,
the TT and the ordering tables inconsistent (and does not guarantee
UNWIND-PROTECT cleanup), so a kill risks silent corruption of every subsequent
search.  The cooperative-poll contract -- SEARCH-TIME-UP-P is checked every
+SEARCH-STOP-POLL-INTERVAL+ nodes at both PVS-SEARCH and QUIESCENCE-SEARCH
entry, and a single node's work is bounded -- means a >10s no-poll gap implies
an infinite-loop bug in the search itself, which is a code defect to fix at the
source rather than here.  On timeout an `info string` wedge diagnostic is
emitted so the silent live-lock becomes a visible, restartable condition for the
GUI/operator."
  (declare (optimize (speed 3) (safety 2)))
  #+sbcl
  (if (and *uci-search-thread* (sb-thread:thread-alive-p *uci-search-thread*))
      (progn
        (setf *uci-search-stop-requested* t)
        (let ((result (sb-thread:join-thread *uci-search-thread*
                                             :timeout 10
                                             :default :timed-out)))
          (if (eq result :timed-out)
              (progn
                (format t "info string search thread unresponsive after stop; ~
                           not killed (cooperative-poll contract) -- restart recommended~%")
                (force-output)
                nil)
              (progn
                (setf *uci-search-thread* nil)
                t))))
      (progn
        (setf *uci-search-thread* nil)
        t))
  #-sbcl
  t)

(defun uci-loop ()
  "Read UCI commands from *STANDARD-INPUT* until quit."
    (declare (optimize (speed 3) (safety 2)))
(setf *uci-board* (initial-board))
  (loop
     (handler-case
         (let ((line (read-uci-line)))
           (unless line
             (stop-search-and-join)
             (return))
           (let ((command (first-token line)))
             (cond ((string-equal command "uci")
                    (send-uci-identification))
                   ((string-equal command "isready")
                    (format t "readyok~%")
                    (force-output))
                   ((string-equal command "setoption")
                    ;; Every other command that mutates shared search state
                    ;; (position/stop/quit/ucinewgame below) stops any
                    ;; in-flight search first -- setoption's Hash resize was
                    ;; the one exception, and TT-RESIZE's *TT-MASK*/
                    ;; *TT-TABLE* setfs are not atomic with respect to a
                    ;; concurrently-running search thread's tt-probe/
                    ;; tt-store, which could read a new mask against the
                    ;; still-old table (or vice versa) and index out of
                    ;; range.
                    (if (stop-search-and-join)
                        (handle-uci-setoption line)
                        (progn
                          (format t "info string previous search is still stopping; setoption ignored~%")
                          (force-output))))
                   ((string-equal command "position")
                    (if (stop-search-and-join)
                        (handle-uci-position line)
                        (progn
                          (format t "info string previous search is still stopping; position ignored~%")
                          (force-output))))
                   ((string-equal command "go")
                    (handle-uci-go line))
                   ((string-equal command "stop")
                    (stop-search-and-join))
                   ((string-equal command "quit")
                    (stop-search-and-join)
                    (return))
                   ;; Non-standard but useful.
                   ((string-equal command "ucinewgame")
                    (if (stop-search-and-join)
                        (progn
                          ;; A new game is unrelated to whatever came before it.
                          ;; RESET-SEARCH-STATE clears both the TT and the
                          ;; killer/history/countermove tables together -- see
                          ;; documentation/G14 addendum for the bug where this
                          ;; handler cleared only the TT, letting move-ordering
                          ;; data learned from one position leak into the next,
                          ;; changing both node counts and the returned bestmove
                          ;; for an identical position+depth depending solely on
                          ;; what was searched earlier in the process.
                          (reset-search-state)
                          (setf *uci-board* (initial-board)))
                        (progn
                          (format t "info string previous search is still stopping; ucinewgame ignored~%")
                          (force-output)))))))
       ;; Ordered ahead of the general ERROR clause below: END-OF-FILE is a
       ;; subtype of ERROR, so without this clause a future READ (there is
       ;; none today -- READ-UCI-LINE's READ-LINE already returns NIL rather
       ;; than signaling) would be swallowed as an ordinary malformed-input
       ;; report instead of ending the loop the same way the normal EOF path
       ;; (line 68-70 above) already does -- and instead of reaching
       ;; src/cli.lisp's MAIN, which has its own dedicated END-OF-FILE clause
       ;; for a cleanly-closed GUI pipe.
       (end-of-file ()
         (stop-search-and-join)
         (return))
       (error (e)
         ;; A malformed command (bad move token, bad FEN, or any other
         ;; unexpected condition) must never take down the whole engine
         ;; process -- report it to the GUI and keep serving the protocol
         ;; instead of letting it propagate and crash. (A slow search
         ;; thread's JOIN-THREAD no longer signals on timeout -- it returns
         ;; :TIMED-OUT, see STOP-SEARCH-AND-JOIN's :DEFAULT above -- so that
         ;; case doesn't actually reach this clause; kept broad regardless,
         ;; since this is meant to be the last line of defense.)
         (format t "info string ~A~%" e)
         (force-output))))
  ;; Ensure output is flushed before exiting.
  (force-output))

(defun read-uci-line ()
  "Read and trim one UCI protocol line from standard input, or NIL on EOF."
    (declare (optimize (speed 3) (safety 2)))
(let ((line (read-line *standard-input* nil nil)))
    (when line
      (string-trim " " line))))

(defun first-token (line)
  "Return LINE's first non-empty space-delimited token without splitting the rest."
  (declare (type string line) (optimize (speed 3) (safety 2)))
  (let ((len (length line)))
    (declare (type fixnum len))
    (let ((start 0))
      (declare (type fixnum start))
      (loop while (and (< start len) (char= (aref line start) #\Space))
            do (incf start))
      (let ((end start))
        (declare (type fixnum end))
        (loop while (and (< end len) (not (char= (aref line end) #\Space)))
              do (incf end))
        (if (< start end)
            (subseq line start end)
            "")))))

(defun send-uci-identification ()
  "Emit the engine's UCI identification, options, and terminating uciok line."
    (declare (optimize (speed 3) (safety 2)))
(format t "id name ~A~%" +engine-name+)
  (format t "id author ~A~%" +engine-author+)
  (format t "option name Hash type spin default ~D min ~D max ~D~%"
          (tt-current-hash-mb) +hash-min-mb+ +hash-max-mb+)
  (format t "uciok~%")
  (force-output))


;;;; setoption command

(defun parse-setoption-arguments (line)
  "Parse `setoption` line and return plist (:name <string> :value <string-or-nil>)."
    (declare (optimize (speed 3) (safety 2)))
(let ((tokens (cdr (split-string line)))
        (name-tokens '())
        (value-tokens '())
        (mode nil))
    (dolist (tok tokens)
      (cond ((string-equal tok "name")
             (setf mode :name))
            ((string-equal tok "value")
             (setf mode :value))
            ((eq mode :name)
             (push tok name-tokens))
            ((eq mode :value)
             (push tok value-tokens))))
    (list :name (if name-tokens
                    (format nil "~{~A~^ ~}" (nreverse name-tokens))
                    nil)
          :value (if value-tokens
                     (format nil "~{~A~^ ~}" (nreverse value-tokens))
                     nil))))

(defun handle-uci-setoption (line)
  "Handle UCI setoption command for supported options."
    (declare (optimize (speed 3) (safety 2)))
(let* ((args (parse-setoption-arguments line))
         (name (getf args :name))
         (value (getf args :value)))
    (when (and name (string-equal name "Hash"))
      (let* ((parsed (safe-parse-int value))
             (requested-mb (normalize-hash-mb (or parsed (tt-current-hash-mb)))))
        ;; TT-RESIZE unconditionally wipes the table and resets the
        ;; generation counter -- skip it when the requested size (after
        ;; clamping) is already the current one, so a malformed/missing
        ;; Hash value doesn't discard all accumulated search memory as a
        ;; side effect of resolving to a no-op resize.
        (unless (= requested-mb (tt-current-hash-mb))
          (tt-resize requested-mb)))))
  ;; UCI does not require output for setoption.
  (values))


;;;; position command

(defun uci-side-to-move ()
  "Return the side to move for the current UCI board."
    (declare (optimize (speed 3) (safety 2)))
(board-side-to-move *uci-board*))

(defun handle-uci-position (line)
  "Handle a UCI position command atomically: invalid move tails do not partially
mutate *UCI-BOARD*."
  (declare (optimize (speed 3) (safety 2)))
  (let ((tokens (cdr (split-string line))))
    (unless tokens
      (return-from handle-uci-position))
    (let ((pos-type (first tokens))
          (rest-tokens (rest tokens)))
      (cond ((string-equal pos-type "startpos")
             (let ((board (initial-board)))
               (when (and rest-tokens (string-equal (first rest-tokens) "moves"))
                 (apply-uci-moves-to-board board (cdr rest-tokens)))
               (setf *uci-board* board)))
            ((string-equal pos-type "fen")
             (let ((fen-parts '())
                   (moves nil))
               (loop for remaining on rest-tokens
                     for tok = (first remaining)
                     do (if (string-equal tok "moves")
                            (progn (setf moves (rest remaining))
                                   (return))
                            (push tok fen-parts)))
               (let* ((fen (format nil "~{~A~^ ~}" (nreverse fen-parts)))
                      (board (parse-fen fen)))
                 (when moves
                   (apply-uci-moves-to-board board moves))
                 (setf *uci-board* board))))
            (t
             ;; Default to startpos if malformed.
             (setf *uci-board* (initial-board)))))))

(defun apply-uci-moves-to-board (board move-strings)
  "Apply each UCI algebraic move string in MOVE-STRINGS to BOARD.

Non-atomic: a move that is illegal partway through the list leaves the earlier
moves already played on BOARD.  Callers that need all-or-nothing semantics
(HANDLE-UCI-POSITION) apply this to a freshly made temporary board and commit
it to *UCI-BOARD* only on full success."
  (declare (type board board) (optimize (speed 3) (safety 2)))
  (dolist (alg move-strings)
    (let ((move (uci-to-move alg board)))
      (unless (member move (legal-moves board (board-side-to-move board))
                      :test #'equal-moves)
        (error 'invalid-move :move alg))
      (execute-move board move))))


;;;; go command

(defparameter +uci-known-go-keywords+
  '("infinite" "ponder" "wtime" "btime" "winc" "binc"
    "movestogo" "depth" "nodes" "movetime" "mate" "searchmoves")
  "Keywords that may appear in a UCI go command line.")

(defun go-keyword-p (token)
  "True if TOKEN is a known UCI go parameter keyword."
    (declare (optimize (speed 3) (safety 2)))
(member token +uci-known-go-keywords+ :test #'string-equal))

(defun safe-parse-int (token)
  "Parse TOKEN as a complete non-negative integer, returning NIL on failure."
  (declare (optimize (speed 3) (safety 2)))
  (when token
    (ignore-errors (parse-fen-nonnegative-integer token))))

(defun parse-go-arguments (line)
  "Return a plist of UCI go parameters parsed from LINE."
  (declare (optimize (speed 3) (safety 2)))
  (let ((result '())
        (tokens (cdr (split-string line))))
    (loop while tokens
          for key = (pop tokens)
          do (cond ((string-equal key "infinite")
                    (setf (getf result :infinite) t))
                   ((string-equal key "ponder")
                    (setf (getf result :ponder) t))
                   ((string-equal key "searchmoves")
                    (setf (getf result :searchmoves-provided) t)
                    (let ((moves '()))
                      (loop while (and tokens (not (go-keyword-p (first tokens))))
                            do (push (pop tokens) moves))
                      (setf (getf result :searchmoves) (nreverse moves))))
                   ((go-keyword-p key)
                    (let* ((kw (intern (string-upcase key) :keyword))
                           (next-token (first tokens))
                           (value (safe-parse-int next-token)))
                      (cond (value
                             (pop tokens)
                             (setf (getf result kw) value))
                            ((and next-token (not (go-keyword-p next-token)))
                             (pop tokens)))))
                   (t
                    ;; Unknown token; skip a following numeric value if any.
                    (when (and tokens (safe-parse-int (first tokens)))
                      (pop tokens)))))
    result))

(defun allocate-uci-time (params side-to-move)
  "Return the number of milliseconds to think for SIDE-TO-MOVE from PARAMS."
    (declare (optimize (speed 3) (safety 2)))
(let* ((wtime (or (getf params :wtime) 0))
         (btime (or (getf params :btime) 0))
         (winc (or (getf params :winc) 0))
         (binc (or (getf params :binc) 0))
         (movestogo (getf params :movestogo))
         (remaining (if (eq side-to-move 'white) wtime btime))
         (increment (if (eq side-to-move 'white) winc binc)))
    (cond ((getf params :movetime)
           (getf params :movetime))
          ((and movestogo (> movestogo 0))
           (min (floor (+ (/ remaining movestogo) increment))
                (floor (* remaining 0.6))))
          ((> remaining 0)
           (min (floor (+ (/ remaining 30) increment))
                (floor (* remaining 0.6))))
          (t
           100))))

(defun uci-go-deadline (params side-to-move)
  "Convert PARAMS into an internal-time deadline, or NIL for no deadline.

`go infinite` must run until an explicit `stop`, regardless of any
wtime/btime also present in PARAMS (a GUI may send clock context alongside
`infinite`), so no deadline is computed at all in that case."
    (declare (optimize (speed 3) (safety 2)))
(unless (getf params :infinite)
    (let ((ms (cond ((getf params :movetime)
                     (getf params :movetime))
                    ((or (getf params :wtime) (getf params :btime))
                     (allocate-uci-time params side-to-move)))))
      (when ms
        (+ (get-internal-real-time)
           (floor (* ms (/ internal-time-units-per-second 1000.0))))))))

(defun uci-go-depth (params deadline)
  "Return the iterative-deepening depth ceiling for a parsed `go` command.

`go depth n` searches exactly n plies (clamped to a minimum of 1 -- `go
depth 0`, though technically not a sensible request, must not silently
skip every iterative-deepening iteration and hand back an untested legal
move with no search behind it); `go infinite` runs to the ceiling until
`stop`; a timed `go` (wtime/btime/movetime, i.e. DEADLINE non-nil) also runs to
the ceiling but is halted by the deadline via `search-time-up-p`; a bare `go`
uses the default ceiling.  The DEADLINE branch is the fix for the former hard
`(t 6)` cap, which made every timed game return instantly at depth 6 without
spending its time budget."
    (declare (optimize (speed 3) (safety 2)))
(cond ((getf params :depth) (max 1 (getf params :depth)))
        ((getf params :infinite) +uci-max-go-depth+)
        (deadline +uci-max-go-depth+)
        (t +uci-default-go-depth+)))

(defun uci-score->string (score)
  "Convert internal SCORE to UCI score clause text."
  (declare (type integer score) (optimize (speed 3) (safety 2)))
  (if (> (abs score) +tt-mate-threshold+)
      ;; A mate is encoded as  +mate-score+ - plies_to_mate; UCI reports it in
      ;; MOVES.  Mate-in-N mates after (2N-1) plies, so the move count is
      ;; (plies + 1) / 2 = (+mate-score+ - |score| + 1) / 2 -- the same rounding
      ;; every UCI engine uses.  (Dropping the +1 reported every mate past
      ;; mate-in-1 one move short.)
      (let* ((sign (if (>= score 0) 1 -1))
             (distance (max 1 (truncate (1+ (- +mate-score+ (abs score))) 2))))
        (format nil "score mate ~D" (* sign distance)))
      (format nil "score cp ~D" score)))

(defun emit-uci-info (depth seldepth score nodes elapsed-ms pv)
  "Emit one UCI info line for a completed iterative-deepening iteration."
    (declare (optimize (speed 3) (safety 2)))
(let ((nps (node-rate-nps nodes elapsed-ms))
        (pv-str (if pv
                    (format nil "~{~A~^ ~}" pv)
                    "")))
    (format t "info depth ~D seldepth ~D ~A nodes ~D nps ~D hashfull ~D time ~D pv ~A~%"
            depth seldepth (uci-score->string score) nodes nps (tt-hashfull)
            elapsed-ms pv-str)
    (force-output)))

(defun legal-searchmoves (board side move-strings)
  "Return the legal root moves selected by the UCI searchmoves list."
    (declare (optimize (speed 3) (safety 2)))
(let ((legal (legal-moves board side))
        (selected '()))
    (dolist (alg move-strings (nreverse selected))
      (let ((move (ignore-errors (uci-to-move alg board))))
        (when move
          (let ((match (find move legal :test #'equal-moves)))
            (when (and match (not (member match selected :test #'equal-moves)))
              (push match selected))))))))

(defun run-uci-search (board-copy side depth &optional root-moves root-filter-p)
  "Run the iterative-deepening search and emit its `bestmove` result, falling
back to `bestmove (none)` on any internal search error instead of leaving
the GUI waiting. Shared by both the threaded (SBCL) and synchronous
(non-SBCL) `go` paths in HANDLE-UCI-GO so they cannot drift out of sync --
see documentation/G15 for the asymmetry this replaced (only the threaded
path used to guard against a search error)."
    (declare (optimize (speed 3) (safety 2)))
(handler-case
      (if (and root-filter-p (null root-moves))
          (progn
            (format t "info string every provided searchmove is illegal for this position~%")
            (format t "bestmove (none)~%"))
          (multiple-value-bind (score move)
              (choose-move-iterative board-copy side depth #'emit-uci-info root-moves nil)
            (declare (ignore score))
            (if move
                (format t "bestmove ~A~%" (move-to-uci move))
                (format t "bestmove (none)~%"))))
    (error (e)
      (format *error-output* "Search error: ~A~%" e)
      (format t "bestmove (none)~%")))
  (force-output))

(defun handle-uci-go (line)
  "Parse a UCI go command and emit a bestmove response.

The search runs in a background thread so the UCI command loop remains
responsive to `stop`, `quit` and `position` commands."
  (declare (optimize (speed 3) (safety 2)))
  ;; B1: stop any previous search before GAME-OVER-P, DUPLICATE-BOARD, or
  ;; LEGAL-SEARCHMOVES can enter LEGAL-MOVES/KING-VALID and touch shared scratch.
  (unless (stop-search-and-join)
    (format t "info string previous search is still stopping~%")
    (format t "bestmove (none)~%")
    (force-output)
    (return-from handle-uci-go))
  (let ((status (game-over-p *uci-board* (uci-side-to-move))))
    ;; B5: only CHECKMATE/STALEMATE mean no legal moves exist. A rules-draw
    ;; (repetition/50-move/insufficient-material, GAME-OVER-P's 'DRAW) still
    ;; has legal moves on the board and must get a real BESTMOVE like any
    ;; other engine, not "(none)" -- see src/draw.lisp:164 DRAWP.
    (if (member status '(checkmate stalemate))
        (format t "bestmove (none)~%")
        (let* ((params (parse-go-arguments line))
               (side (uci-side-to-move))
               ;; Operate on a copy so the GUI can change the position after
               ;; this search starts.
               (board-copy (duplicate-board *uci-board*))
               (searchmove-strings (getf params :searchmoves))
               (root-filter-p (getf params :searchmoves-provided))
               (root-moves (and root-filter-p
                                (legal-searchmoves board-copy side searchmove-strings)))
               (deadline (uci-go-deadline params side))
               (depth (uci-go-depth params deadline)))
          ;; Reset stop state for fresh search only after STOP-SEARCH-AND-JOIN
          ;; proved no stale worker remains alive.
          (setf *uci-search-stop-requested* nil)
          (setf *uci-search-deadline* deadline)
          #+sbcl
          (let ((out *standard-output*))
            (setf *uci-search-thread*
                  (sb-thread:make-thread
                   (lambda ()
                     (let ((*standard-output* out))
                       (run-uci-search board-copy side depth root-moves root-filter-p)))
                   :name "miguedrez-search")))
          #-sbcl
          (run-uci-search board-copy side depth root-moves root-filter-p))))
  (force-output))
