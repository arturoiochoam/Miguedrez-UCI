;;; -*- encoding: utf-8 -*-
;;;
;;; tests.lisp
;;; Self-tests for the Miguedrez engine.
;;; Arthur Matheus, 2026.
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


(defvar *tests-passed* 0)
(defvar *tests-failed* 0)


(defmacro assert-equal (expected actual &optional (message nil))
  "EXPECTED/ACTUAL are bound to gensyms, not the plain EXP/ACT this macro's
skeleton used to use, so an EXPECTED/ACTUAL argument form that happens to
reference a variable named exp/act at some future call site can't be
captured by this macro's own internal bindings."
  (let ((exp (gensym "EXPECTED"))
        (act (gensym "ACTUAL")))
    `(let ((,exp ,expected)
           (,act ,actual))
       (if (equal ,exp ,act)
           (incf *tests-passed*)
           (progn
             (incf *tests-failed*)
             (format *error-output* "FAIL: ~A expected ~S, got ~S~%"
                     ,(or message (format nil "~S" actual)) ,exp ,act))))))


(defmacro assert-true (condition &optional (message nil))
  "Assert that CONDITION is true, evaluating it exactly once."
  (let ((result (gensym "RESULT")))
    `(let ((,result ,condition))
       (if ,result
           (incf *tests-passed*)
           (progn
             (incf *tests-failed*)
             (format *error-output* "FAIL: ~A~%"
                     ,(or message (format nil "~S" condition))))))))


(defmacro assert-signals (condition-type form &optional (message nil))
  `(handler-case
       (progn
         ,form
         (incf *tests-failed*)
         (format *error-output* "FAIL: ~A did not signal ~S~%"
                 ,(or message (format nil "~S" form)) ',condition-type))
     (,condition-type ()
       (incf *tests-passed*))
     (error (e)
       (incf *tests-failed*)
       (format *error-output* "FAIL: ~A signaled wrong condition ~S~%"
               ,(or message (format nil "~S" form)) e))))


(defun moves-matching (moves from-row from-col to-row to-col)
  "Return the subset of MOVES going from (FROM-ROW,FROM-COL) to (TO-ROW,TO-COL)."
    (declare (optimize (speed 3) (safety 2)))
(remove-if-not
   (lambda (mv)
     (and (= (pos-row (move-from mv)) from-row)
          (= (pos-col (move-from mv)) from-col)
          (= (pos-row (move-to mv)) to-row)
          (= (pos-col (move-to mv)) to-col)))
   moves))

(defun move-among-p (moves from-row from-col to-row to-col)
  "True if MOVES contains a move from (FROM-ROW,FROM-COL) to (TO-ROW,TO-COL)."
    (declare (optimize (speed 3) (safety 2)))
(and (moves-matching moves from-row from-col to-row to-col) t))


;;;; Test suite

;;;; Individual test groups

(defun test-fen-parser ()
    (declare (optimize (speed 3) (safety 2)))
(let ((board (parse-fen +standard-start-position-fen+)))
    (assert-equal 'rb (aref (board-board board) 7 4) "white king at e1")
    (assert-equal 'rn (aref (board-board board) 0 4) "black king at e8"))
  (assert-equal 32
                (let ((board (parse-fen +standard-start-position-fen+))
                      (count 0))
                  (dotimes (i 8)
                    (dotimes (j 8)
                      (unless (eq (aref (board-board board) i j) 'vv)
                        (incf count))))
                  count)
                "32 pieces on start board")
  (dolist (fen '("4k3/8/8/8/8/8/8/4K3/8 w - - 0 1"
                 "4k3/8/8/8/8/8/4K3 w - - 0 1"
                 "08/8/8/8/8/8/4k3/4K3 w - - 0 1"
                 "4k3/8/8/8/8/8/8/4K3 w X - 0 1"
                 "4k3/8/8/8/8/8/8/4K3 w - - 12foo 1"
                 "4Kk2/8/8/8/8/8/8/8 w - - 0 1"
                 "4k3/8/8/8/8/8/4R3/4K3 w - - 0 1"))
    (assert-signals invalid-fen (parse-fen fen)
                    (format nil "invalid FEN is rejected: ~A" fen)))
  (let ((board (parse-fen "4k3/8/8/8/8/8/8/4K3 w - - 1000 10000")))
    (make-move-on-board board "e1d2")
    (make-move-on-board board "e8d7")
    (assert-equal 1002 (board-halfmove-clock board)
                  "large halfmove clocks continue incrementing")
    (assert-equal 10001 (board-fullmove-number board)
                  "large fullmove numbers continue incrementing")))


(defun test-starting-position ()
    (declare (optimize (speed 3) (safety 2)))
(let ((board (initial-board)))
    (assert-true (and (= (pos-row (board-white-king-pos board)) 7)
                      (= (pos-col (board-white-king-pos board)) 4))
                 "white king pos")
    (assert-true (and (= (pos-row (board-black-king-pos board)) 0)
                      (= (pos-col (board-black-king-pos board)) 4))
                 "black king pos")
    (assert-true (not (board-white-king-moved board)) "white king unmoved")
    (assert-true (not (board-black-king-moved board)) "black king unmoved")))


(defun test-move-generation ()
    (declare (optimize (speed 3) (safety 2)))
(let ((board (initial-board)))
    (assert-equal 20 (length (legal-moves board 'white))
                  "white has 20 legal moves from start")
    (assert-equal 20 (length (legal-moves board 'black))
                  "black has 20 legal moves from start"))
  (let* ((board (parse-fen "4k3/8/8/8/8/8/8/4K3 w - - 0 1"))
         (key (board-key board)))
    (assert-true (member (uci-to-move "e8f7" board) (legal-moves board 'black)
                         :test #'equal-moves)
                 "non-side-to-move legal-moves still finds black king moves")
    (assert-equal '0 (aref (board-whites board) 0 4)
                  "non-side-to-move legal-moves does not corrupt white occupancy")
    (assert-equal (compute-board-key board) (board-key board)
                  "non-side-to-move legal-moves leaves hash consistent")
    (assert-equal key (board-key board)
                  "non-side-to-move legal-moves leaves board key unchanged")))


(defun test-uci-notation ()
    (declare (optimize (speed 3) (safety 2)))
(let* ((board (initial-board))
         (move (uci-to-move "e2e4" board)))
    (assert-equal "e2e4" (move-to-uci move) "round-trip e2e4")
    (assert-equal 6 (pos-row (move-from move)) "e2 row")
    (assert-equal 4 (pos-col (move-from move)) "e2 col")
    (assert-equal 4 (pos-row (move-to move)) "e4 row")
    (assert-equal 4 (pos-col (move-to move)) "e4 col")
    (assert-signals invalid-move (uci-to-move "e2e4q" board)
                    "promotion suffix is rejected off the back rank")
    (assert-signals invalid-move (uci-to-move "e2e4x" board)
                    "invalid promotion suffix is rejected"))
  (let ((board (parse-fen "8/P7/8/8/8/8/8/K6k w - - 0 1")))
    (assert-equal 'q (move-promotion (uci-to-move "a7a8q" board))
                  "queen promotion parses")
    (assert-signals invalid-move (uci-to-move "a7a8x" board)
                    "invalid promotion piece is rejected on back rank")))


(defun test-checkmate ()
    (declare (optimize (speed 3) (safety 2)))
(let ((board (parse-fen "r1bqkb1r/pppp1Qpp/2n2n2/4p3/2B1P3/8/PPPP1PPP/RNB1K1NR b KQkq - 0 4")))
    (assert-true (checkmatep board 'black) "scholar's mate is checkmate")))


(defun test-stalemate ()
    (declare (optimize (speed 3) (safety 2)))
(let ((board (parse-fen "k7/8/1Q6/8/8/8/8/7K b - - 0 1")))
    (assert-true (stalematep board 'black) "queen vs lone king stalemate")
    (assert-true (not (checkp board 'black)) "stalemate not check"))
  (let ((board (parse-fen "k7/8/1Q6/8/8/8/8/7K b - - 100 1")))
    (assert-equal 'stalemate (game-over-p board 'black)
                  "stalemate takes precedence over 50-move draw")))


(defun test-castling ()
    (declare (optimize (speed 3) (safety 2)))
(let ((board (parse-fen "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 0 1")))
    (assert-true (not (board-white-king-moved board)) "white king unmoved")
    (assert-true (not (board-white-kingside-rook-moved board)) "white ks rook unmoved")
    (let ((castles (moves-matching (legal-moves board 'white) 7 4 7 6)))
      (assert-equal 1 (length castles) "white can castle kingside"))))


(defun test-promotion ()
    (declare (optimize (speed 3) (safety 2)))
(let ((board (parse-fen "8/P7/8/8/8/8/8/K6k w - - 0 1")))
    (execute-move board (uci-to-move "a7a8q" board))
    (assert-equal 'db (aref (board-board board) 0 0) "promote to white queen"))
  (let ((board (parse-fen "7K/8/8/8/8/8/p7/7k b - - 0 1")))
    (execute-move board (uci-to-move "a2a1n" board))
    (assert-equal 'cn (aref (board-board board) 7 0) "promote to black knight")))


(defun test-special-moves ()
  ;; Underpromotions are generated as separate legal moves.
    (declare (optimize (speed 3) (safety 2)))
(let* ((board (parse-fen "8/P7/8/8/8/8/8/K6k w - - 0 1"))
         (promos (moves-matching (legal-moves board 'white) 1 0 0 0))
         (promo-syms (sort (mapcar #'move-promotion promos) #'string<)))
    (assert-equal 4 (length promos) "four white promotion choices")
    (assert-equal '(b n q r) promo-syms "white promotions are q r b n"))
  (let* ((board (parse-fen "7K/8/8/8/8/8/p7/7k b - - 0 1"))
         (promos (moves-matching (legal-moves board 'black) 6 0 7 0))
         (promo-syms (sort (mapcar #'move-promotion promos) #'string<)))
    (assert-equal 4 (length promos) "four black promotion choices")
    (assert-equal '(b n q r) promo-syms "black promotions are q r b n"))
  ;; En passant: white captures a black pawn that just pushed d7-d5.
  (let* ((board (parse-fen "rnbqkbnr/ppp1pppp/8/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 1"))
         (ep-moves (moves-matching (legal-moves board 'white) 3 4 2 3)))
    (assert-equal 1 (length ep-moves) "white can capture en passant on d6"))
  ;; En passant: black captures a white pawn that just pushed d2-d4.
  (let* ((board (parse-fen "rnbqkbnr/pppp1ppp/8/8/3Pp3/8/PPP1PPPP/RNBQKBNR b KQkq e3 0 1"))
         (ep-moves (moves-matching (legal-moves board 'black) 4 4 5 4)))
    (assert-equal 1 (length ep-moves) "black can capture en passant on e3"))
  ;; Malformed/non-capturable en-passant rights are canonicalized away so legal
  ;; move generation cannot fabricate the captured pawn and hashes match the
  ;; equivalent no-EP position.
  (let* ((bad (parse-fen "4k3/8/8/4P3/8/8/8/4K3 w - d6 0 1"))
         (key-before (board-key bad)))
    (assert-true (not (move-among-p (legal-moves bad 'white) 3 4 2 3))
                 "invalid en-passant FEN produces no phantom capture")
    (assert-equal 'vv (aref (board-board bad) 3 3)
                  "invalid en-passant generation does not fabricate captured pawn")
    (assert-equal key-before (board-key bad)
                  "invalid en-passant generation leaves board key unchanged")
    (assert-equal (compute-board-key bad) (board-key bad)
                  "invalid en-passant generation leaves hash consistent"))
  (let ((with-ep (parse-fen "4k3/8/8/8/7P/8/8/4K3 b - h3 0 1"))
        (without-ep (parse-fen "4k3/8/8/8/7P/8/8/4K3 b - - 0 1")))
    (assert-equal (board-key without-ep) (board-key with-ep)
                  "non-capturable en-passant FEN hashes like no en-passant"))
  ;; Pinned adjacent pawn: an e.p. right is pseudo-legal but the capture is
  ;; illegal -- here a horizontal pin, where the capturer (b4) and the captured
  ;; pawn (c4) share rank 4 with the capturer's king (a4) and an enemy rook
  ;; (h4), so bxc3 removes both pawns and exposes the king.  The old
  ;; adjacency-only test kept the flag and folded the e.p. file into the key,
  ;; so the position hashed differently from the same placement reached without
  ;; the fresh double push -- under-counting repetitions and missing TT
  ;; transpositions.  Pin-aware ENPASSANT-RIGHT-CAPTURABLE-P must clear it.
  (let ((pinned-ep (parse-fen "8/8/8/8/kpP4R/8/8/4K3 b - c3 0 1"))
        (no-ep (parse-fen "8/8/8/8/kpP4R/8/8/4K3 b - - 0 1")))
    (assert-equal '0 (aref (board-whites-enpass pinned-ep) 2)
                  "pinned e.p. right is cleared by normalize")
    (assert-true (not (move-among-p (legal-moves pinned-ep 'black) 4 1 5 2))
                 "pinned e.p. capture is not generated")
    (assert-equal (board-key no-ep) (board-key pinned-ep)
                  "pinned e.p. position hashes like no-e.p. position"))
  ;; A double push that creates a LEGAL e.p. capture (no pin) must still set
  ;; the flag, so MAYBE-SET-ENPASSANT-RIGHT does not over-restrict.
  (let ((board (parse-fen "4k3/8/8/8/3p4/8/4P3/4K3 w - - 0 1")))
    (make-move-on-board board "e2e4")
    (assert-equal '1 (aref (board-whites-enpass board) 4)
                  "double push with legal adjacent e.p. sets the flag"))
  (let ((board (initial-board)))
    (make-move-on-board board "e2e4")
    (assert-equal '0 (aref (board-whites-enpass board) 4)
                  "double push without adjacent enemy pawn creates no en-passant key"))
  ;; Kingside castling is illegal if any square the king crosses is attacked.
  (let ((board (parse-fen "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 0 1")))
    (assert-true (move-among-p (legal-moves board 'white) 7 4 7 6)
                 "white can castle kingside in clean position"))
  (let ((board (parse-fen "r3k2r/pppppppp/8/8/8/6n1/PPPPPPPP/R3K2R w KQkq - 0 1")))
    (assert-true (not (move-among-p (legal-moves board 'white) 7 4 7 6))
                 "white kingside castle illegal when f1 attacked"))
  (let ((board (parse-fen "r3k2r/pppppppp/8/8/8/8/PPPPPPPb/R3K2R w KQkq - 0 1")))
    (assert-true (not (move-among-p (legal-moves board 'white) 7 4 7 6))
                 "white kingside castle illegal when g1 attacked"))
  ;; Queenside castling is illegal if any square the king crosses is attacked.
  (let ((board (parse-fen "r3k2r/pppppppp/8/8/8/4n3/PPPPPPPP/R3K2R w KQkq - 0 1")))
    (assert-true (not (move-among-p (legal-moves board 'white) 7 4 7 2))
                 "white queenside castle illegal when d1 attacked"))
  (let ((board (parse-fen "r3k2r/pppppppp/8/8/8/2n5/PPPPPPPP/R3K2R w KQkq - 0 1")))
    (assert-true (not (move-among-p (legal-moves board 'white) 7 4 7 2))
                 "white queenside castle illegal when c1 attacked"))
  ;; Black queenside castling: c8 must be safe as well as d8.
  (let ((board (parse-fen "r3k2r/pppppppp/2N5/8/8/8/PPPPPPPP/R3K2R b KQkq - 0 1")))
    (assert-true (not (move-among-p (legal-moves board 'black) 0 4 0 2))
                 "black queenside castle illegal when c8 attacked"))
  ;; Black kingside castling: g8 and f8 must be safe.
  (let ((board (parse-fen "r3k2r/pppppppB/8/8/8/8/PPPPPPPP/R3K2R b KQkq - 0 1")))
    (assert-true (not (move-among-p (legal-moves board 'black) 0 4 0 6))
                 "black kingside castle illegal when g8 attacked"))
  ;; Capturing a rook on its original square must clear that side's castling
  ;; right, just like moving that rook would.
  (let ((board (parse-fen "r3k2r/8/8/8/8/8/8/R3K2Q w Qkq - 0 1")))
    (execute-move board (uci-to-move "h1h8" board))
    (assert-true (board-black-kingside-rook-moved board)
                 "captured h8 rook clears black kingside castling right")
    (assert-equal (compute-board-key board) (board-key board)
                  "rook-capture castling-right key stays consistent")
    (assert-true (not (move-among-p (legal-moves board 'black) 0 4 0 6))
                 "black cannot castle kingside after h8 rook capture")))


(defun test-draw-rules ()
  ;; 50-move rule: halfmove-clock of 100 is a draw.
    (declare (optimize (speed 3) (safety 2)))
(let ((board (parse-fen "4k3/8/8/8/8/8/8/4K3 w - - 100 1")))
    (assert-true (draw-by-50-move-rule-p board) "50-move rule draw"))
  ;; Insufficient material cases.
  (assert-true (insufficient-material-p (parse-fen "4k3/8/8/8/8/8/8/4K3 w - - 0 1"))
               "K-K is insufficient")
  (assert-true (insufficient-material-p (parse-fen "4k3/8/8/8/8/8/1B6/4K3 w - - 0 1"))
               "KB-K is insufficient")
  (assert-true (insufficient-material-p (parse-fen "4k3/8/8/8/8/8/1N6/4K3 w - - 0 1"))
               "KN-K is insufficient")
  ;; KNN-K is NOT a dead position: two knights can cooperatively mate a lone
  ;; king (the mate merely cannot be forced), so it must not be an automatic draw.
  (assert-true (not (insufficient-material-p (parse-fen "4k3/8/8/8/8/1N6/1N6/4K3 w - - 0 1")))
               "KNN-K is not automatic insufficient material")
  ;; KB-KB: dead draw iff both bishops share a colour complex (mate impossible).
  ;; Opposite-colour bishops admit a helpmate, so that case is NOT insufficient.
  (assert-true (not (insufficient-material-p (parse-fen "4k3/8/8/8/8/8/1B6/4Kb2 w - - 0 1")))
               "KB-KB opposite-colour bishops is not insufficient")
  (assert-true (insufficient-material-p (parse-fen "4k3/8/8/8/8/8/1B6/4K1b1 w - - 0 1"))
               "KB-KB same-colour bishops is insufficient")
  (assert-true (not (insufficient-material-p (parse-fen "4k3/8/8/8/8/8/1Q6/4K3 w - - 0 1")))
               "KQ-K is not insufficient")
  ;; Positions where checkmate is still possible with cooperative play are
  ;; not automatic FIDE draws.
  (assert-true (not (insufficient-material-p (parse-fen "4k3/8/8/8/8/8/4N3/4K2n w - - 0 1")))
               "KN-KN is not insufficient")
  (assert-true (not (insufficient-material-p (parse-fen "4k3/8/8/8/8/4b3/4N2n/4K1B1 w - - 0 1")))
               "BN-BN is not insufficient")
  ;; A real KNN-K checkmate must be scored as mate, not masked as a draw.
  ;; Black Ka8 is mated: White Nc7 checks a8; escapes a7/b7 are covered by Ka6
  ;; and a7/b8 by Nc6, so Black has no legal move. Because DRAWP is tested before
  ;; terminal-mate detection, the removed KNN-K draw clause would have returned 0
  ;; here; the search must instead return a mate-magnitude score.
  (let* ((board (parse-fen "k7/2N5/K1N5/8/8/8/8/8 b - - 0 1"))
         (score (pvs-search board 1 (- +mate-score+) +mate-score+ 0)))
    (assert-true (not (insufficient-material-p board))
                 "KNN-K mate position is not treated as insufficient material")
    (assert-true (>= (abs score) +tt-mate-threshold+)
                 "KNN-K checkmate is scored as mate, not as a draw"))
  ;; Checkmate overrides draw predicates: a mated side does not get a draw score
  ;; just because the halfmove clock also reached 100.
  (let ((board (parse-fen "7k/6Q1/5K2/8/8/8/8/8 b - - 100 1")))
    (assert-true (checkmatep board 'black)
                 "halfmove-100 position is still checkmate")
    (assert-equal 'checkmate (game-over-p board 'black)
                  "game-over reports checkmate before draw")
    (multiple-value-bind (score move)
        (pvs-search board 1 (- +mate-score+) +mate-score+ 0)
      (declare (ignore move))
      (assert-true (<= score (- +tt-mate-threshold+))
                   "search scores halfmove-100 checkmate as mate, not draw")))
  ;; Threefold repetition: repeat a position twice to reach three occurrences.
  (let ((board (parse-fen "8/8/8/8/8/8/8/4K2k w - - 0 1")))
    ;; Cycle: Ke1-d1 Kh1-h2 Kd1-e1 Kh2-h1.
    (dolist (alg '("e1d1" "h1h2" "d1e1" "h2h1"))
      (execute-move board (uci-to-move alg board)))
    (assert-true (not (draw-by-repetition-p board)) "second occurrence is not threefold")
    (dolist (alg '("e1d1" "h1h2" "d1e1" "h2h1"))
      (execute-move board (uci-to-move alg board)))
    (assert-true (draw-by-repetition-p board) "third occurrence is threefold draw")))


(defun test-uci-identification ()
    (declare (optimize (speed 3) (safety 2)))
(assert-equal "Miguedrez UCI 0.95.11.5" +engine-name+ "engine name")
  (assert-equal "Manuel Felipe Gamallo Rivero, 0.95 and Arthur Matheus further releases"
                +engine-author+ "engine author")
  (let ((stream (make-string-output-stream)))
    (let ((*standard-output* stream))
      (send-uci-identification))
    (let ((out (get-output-stream-string stream)))
      (assert-true (search "option name Hash type spin" out :test #'char-equal)
                   "uci options include Hash"))))

(defun test-uci-position-move-validation ()
    (declare (optimize (speed 3) (safety 2)))
(let ((board (initial-board)))
    (assert-signals invalid-move (apply-uci-moves-to-board board '("e2e5"))
                    "illegal UCI position move is rejected")
    (assert-equal 'pb (aref (board-board board) 6 4)
                  "illegal UCI move leaves from-square intact")
    (assert-equal 'vv (aref (board-board board) 3 4)
                  "illegal UCI move leaves destination empty")
    (assert-equal 'white (board-side-to-move board)
                  "illegal UCI move leaves side to move unchanged"))
  (let ((*uci-board* (initial-board)))
    (assert-signals invalid-move
        (handle-uci-position "position startpos moves e2e4 e7e5 a3a4")
      "uci position rollback rejects invalid later moves")
    (assert-equal 'white (board-side-to-move *uci-board*)
                  "failed position command leaves side to move unchanged")
    (assert-equal 'pb (aref (board-board *uci-board*) 6 4)
                  "failed position command rolls back first move")
    (assert-equal 'pn (aref (board-board *uci-board*) 1 4)
                  "failed position command rolls back second move"))
  (let ((board (initial-board)))
    (assert-signals invalid-move (make-move-on-board board "a3a4")
                    "make-move-on-board rejects illegal source squares")
    (assert-equal 'vv (aref (board-board board) 5 0)
                  "rejected make-move-on-board leaves source empty")
    (assert-equal 'vv (aref (board-board board) 4 0)
                  "rejected make-move-on-board leaves destination empty")
    (assert-equal 'white (board-side-to-move board)
                  "rejected make-move-on-board leaves side unchanged")))

(defun test-time-management ()
  ;; go depth N
    (declare (optimize (speed 3) (safety 2)))
(let ((params (parse-go-arguments "go depth 5")))
    (assert-equal 5 (getf params :depth) "parse go depth"))
  ;; go movetime N
  (let ((params (parse-go-arguments "go movetime 500")))
    (assert-equal 500 (getf params :movetime) "parse go movetime"))
  ;; go infinite
  (let ((params (parse-go-arguments "go infinite")))
    (assert-true (getf params :infinite) "parse go infinite"))
  ;; go searchmoves preserves and restricts root move choices.
  (let ((params (parse-go-arguments "go searchmoves a2a3 b1c3 depth 5")))
    (assert-true (getf params :searchmoves-provided) "parse go searchmoves marker")
    (assert-equal '("a2a3" "b1c3") (getf params :searchmoves)
                  "parse go searchmoves list")
    (assert-equal 5 (getf params :depth) "parse depth after searchmoves"))
  (let* ((board (initial-board))
         (root (legal-searchmoves board 'white '("a2a3")))
         (move (nth-value 1 (choose-move-iterative board 'white 2 nil root))))
    (assert-equal "a2a3" (and move (move-to-uci move))
                  "searchmoves restricts iterative root search"))
  (let ((board (initial-board)))
    (assert-true (null (legal-searchmoves board 'white '("a2a5")))
                 "illegal searchmoves filter to an empty root list"))
  ;; tournament time control
  (let ((params (parse-go-arguments "go wtime 60000 btime 60000 winc 100 binc 100 movestogo 40")))
    (assert-equal 60000 (getf params :wtime) "parse wtime")
    (assert-equal 60000 (getf params :btime) "parse btime")
    (assert-equal 100 (getf params :winc) "parse winc")
    (assert-equal 100 (getf params :binc) "parse binc")
    (assert-equal 40 (getf params :movestogo) "parse movestogo"))
  ;; Time allocation: tournament mode uses remaining/movestogo + increment.
  (let ((ms (allocate-uci-time '(:wtime 60000 :btime 60000 :winc 100 :binc 100 :movestogo 40)
                               'white)))
    (assert-true (> ms 0) "tournament allocation positive")
    (assert-true (<= ms 36000) "tournament allocation capped at 60% of remaining"))
  ;; Time allocation: sudden death uses a fraction of remaining time.
  (let ((ms (allocate-uci-time '(:wtime 60000 :btime 60000 :winc 100 :binc 100) 'white)))
    (assert-true (> ms 0) "sudden-death allocation positive")
    (assert-true (<= ms 36000) "sudden-death allocation capped"))
  ;; movetime is passed through unchanged.
  (let ((ms (allocate-uci-time '(:movetime 500) 'white)))
    (assert-equal 500 ms "movetime passthrough"))
  (let ((*uci-search-stop-requested* t)
        (*uci-search-deadline* 0))
    (multiple-value-bind (score move)
        (choose-move-iterative (initial-board) 'white 1)
      (declare (ignore score))
      (assert-true move "direct iterative search ignores stale UCI stop/deadline")))
  ;; score formatting for UCI info lines.
  (assert-true (search "score cp" (uci-score->string 12) :test #'char-equal)
               "cp score formatting")
  (assert-true (search "score mate" (uci-score->string (+ +tt-mate-threshold+ 10)) :test #'char-equal)
               "mate score formatting")
  ;; Exact score clauses.  cp keeps its sign; a mate is reported in MOVES via the
  ;; UCI convention (plies+1)/2, so a mate encoded as +mate-score+-(2N-1) prints
  ;; as mate N (not N-1).  Guards the +1 in uci-score->string against regression.
  (assert-true (string= "score cp -12" (uci-score->string -12)) "cp negative sign")
  (assert-true (string= "score mate 1" (uci-score->string (- +mate-score+ 1)))
               "mate-in-1 move count")
  (assert-true (string= "score mate 2" (uci-score->string (- +mate-score+ 3)))
               "mate-in-2 move count")
  (assert-true (string= "score mate 3" (uci-score->string (- +mate-score+ 5)))
               "mate-in-3 move count")
  (assert-true (string= "score mate -2" (uci-score->string (- (- +mate-score+ 3))))
               "mated-in-2 sign and count")
  ;; go depth ceiling policy (the depth-cap fix): explicit depth honoured; infinite
  ;; and timed searches use the max ceiling (stopped by stop/deadline); a bare go
  ;; uses the default.  Replaces the former hard 6-ply cap on every timed game.
  (assert-equal 4 (uci-go-depth '(:depth 4) nil) "go depth n honoured")
  (assert-equal +uci-max-go-depth+ (uci-go-depth '(:infinite t) nil)
                "go infinite uses max ceiling")
  (assert-equal +uci-max-go-depth+ (uci-go-depth '() 12345)
                "timed go uses max ceiling (deadline stops it)")
  (assert-equal +uci-default-go-depth+ (uci-go-depth '() nil)
                "bare go uses default ceiling"))

(defun test-hash-option ()
  ;; Normalization boundaries.
    (declare (optimize (speed 3) (safety 2)))
(assert-equal 1 (normalize-hash-mb 0) "hash min clamp")
  (assert-equal 4096 (normalize-hash-mb 5000) "hash max clamp")
  (assert-equal 128 (normalize-hash-mb 128) "hash keep valid")
  ;; setoption parser.
  (let ((args (parse-setoption-arguments "setoption name Hash value 256")))
    (assert-equal "Hash" (getf args :name) "parse setoption name")
    (assert-equal "256" (getf args :value) "parse setoption value"))
  ;; Resize behavior.
  (let ((old (tt-current-hash-mb)))
    (unwind-protect
         (progn
           (handle-uci-setoption "setoption name Hash value 1")
           (assert-equal 1 (tt-current-hash-mb) "hash set to 1")
           (handle-uci-setoption "setoption name Hash value 4096")
           (assert-equal 4096 (tt-current-hash-mb) "hash set to 4096")
           (handle-uci-setoption "setoption name Hash value 99999")
           (assert-equal 4096 (tt-current-hash-mb) "hash clamps above max"))
      (tt-resize old))))

(defun test-iterative-info-search ()
    (declare (optimize (speed 3) (safety 2)))
(let ((board (initial-board))
        (infos '()))
    (setf *uci-search-stop-requested* nil)
    (setf *uci-search-deadline* nil)
    (multiple-value-bind (score move)
        (choose-move-iterative board 'white 2
                               (lambda (depth seldepth info-score nodes elapsed-ms pv)
                                 (declare (ignore info-score))
                                 (push (list depth seldepth nodes elapsed-ms pv) infos)))
      (declare (ignore score))
      (assert-true move "iterative search returns move")
      (assert-true (>= (length infos) 1) "iterative search emits info callbacks")
      (let ((top (first infos)))
        (assert-true (>= (first top) 1) "info depth >= 1")
        (assert-true (>= (third top) 1) "info nodes positive")))))


(defun test-pv-reporting-fen ()
  ;; Regression requested by user: ensure search reports a legal PV from this opening position.
    (declare (optimize (speed 3) (safety 2)))
(let* ((board (parse-fen "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq e6 0 2"))
         (captured-pv nil))
    (setf *uci-search-stop-requested* nil)
    (setf *uci-search-deadline* nil)
    (multiple-value-bind (score move)
        (choose-move-iterative board 'white 3
                               (lambda (depth seldepth info-score nodes elapsed-ms pv)
                                 (declare (ignore depth seldepth info-score nodes elapsed-ms))
                                 (setf captured-pv pv)))
      (declare (ignore score))
      (assert-true move "search returns move on requested fen")
      (when captured-pv
        (let ((legal (legal-moves board 'white)))
          (assert-true (member (uci-to-move (first captured-pv) board) legal :test #'equal-moves)
                       "first pv move is legal"))))))

(defun test-pvs-vs-alpha-beta-entrypoint ()
  ;; Ensure compatibility wrapper remains functional.
    (declare (optimize (speed 3) (safety 2)))
(let ((board (initial-board)))
    (multiple-value-bind (score move)
        (alpha-beta board 2 (- +mate-score+) +mate-score+)
      (declare (ignore score))
      (assert-true move "alpha-beta compatibility wrapper returns move"))))

(defun test-go-argument-robustness ()
  ;; Unknown/partial go arguments should not crash parsing or consume the next
  ;; keyword as a bogus value.
  (declare (optimize (speed 3) (safety 2)))
  (let ((params (parse-go-arguments "go depth X movetime ??? wtime 1000")))
    (assert-equal 1000 (getf params :wtime) "parser keeps valid tokens")
    (assert-true (null (getf params :depth)) "invalid depth ignored"))
  (let ((params (parse-go-arguments "go depth movetime 50")))
    (assert-true (null (getf params :depth)) "missing depth value ignored")
    (assert-equal 50 (getf params :movetime)
                  "missing depth value does not consume next keyword"))
  (let ((params (parse-go-arguments "go movetime 10abc wtime -1 btime 200")))
    (assert-true (null (getf params :movetime)) "partial numeric value ignored")
    (assert-true (null (getf params :wtime)) "negative time value ignored")
    (assert-equal 200 (getf params :btime) "valid value after bad values kept")))

(defun test-mate-score-sign ()
  ;; Negamax invariant: a side-to-move that is checkmated scores (- +mate-score+)
  ;; at root ply 0, regardless of colour -- there is no white/black split.
    (declare (optimize (speed 3) (safety 2)))
(let ((white-mated (parse-fen "rnb1kbnr/pppp1ppp/8/4p3/6Pq/5P2/PPPPP2P/RNBQKBNR w KQkq - 1 3")))
    (multiple-value-bind (score move)
        (pvs-search white-mated 1 (- +mate-score+) +mate-score+ 0)
      (declare (ignore move))
      (assert-true (< score 0) "white to move and mated => negative (side-to-move) score")
      (assert-equal (- +mate-score+) score "root mate score uses ply distance")))
  (let ((black-mated (parse-fen "r1bqkb1r/pppp1Qpp/2n2n2/4p3/2B1P3/8/PPPP1PPP/RNB1K1NR b KQkq - 0 4")))
    (multiple-value-bind (score move)
        (pvs-search black-mated 1 (- +mate-score+) +mate-score+ 0)
      (declare (ignore move))
      (assert-true (< score 0) "black to move and mated => negative (side-to-move) score")
      (assert-equal (- +mate-score+) score "root mate score uses ply distance"))))

(defun test-no-rim-knight-blunder-fens ()
  ;; Generic-evaluation regression gates for requested opening blunder family.
    (declare (optimize (speed 3) (safety 2)))
(let ((fen-white "rnbqkb1r/pppppppp/7n/8/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 1 2")
        (fen-black "rnbqkbnr/pppppppp/8/8/8/5N2/PPPPPPPP/RNBQKB1R b KQkq - 1 1")
        (fen-hpawn "rnbqkbnr/ppppppp1/8/7p/3P4/8/PPP1PPPP/RNBQKBNR w KQkq h6 0 2")
        (fen-pre-bxd1 "rn1qkbnr/1pp2ppp/p2p4/4N3/2B1P1b1/2N5/PPPP1PPP/R1BQK2R b KQkq - 0 5"))
    (labels ((bestmove-uci (fen depth)
               (let* ((b (parse-fen fen))
                      (side (board-side-to-move b)))
                 (multiple-value-bind (score move)
                     (choose-move-iterative b side depth)
                   (declare (ignore score))
                   (when move (move-to-uci move))))))
      (let ((m1 (bestmove-uci fen-white 4))
            (m2 (bestmove-uci fen-black 4))
            (m3 (bestmove-uci fen-hpawn 4))
            (m4 (bestmove-uci fen-pre-bxd1 4)))
        ;; White to move must avoid Nh3 in these structures.
        (assert-true (not (equal m1 "g1h3")) "avoid Nh3 in FEN1")
        ;; Black to move must avoid Nh6 in these structures.
        (assert-true (not (equal m2 "g8h6")) "avoid Nh6 in FEN2")
        ;; Black-side h-pawn push should not be top preference in this early setup.
        (assert-true (not (equal m3 "h7h5")) "avoid ...h5 in FEN3")
        ;; Check-evasion quiescence must expose the Bxd1 tactical failure by depth 4.
        (assert-true (not (equal m4 "g4d1")) "avoid ...Bxd1 tactical line")))))

(defun test-see-and-ordering-heuristics ()
  ;; SEE approximation returns centipawn material swing, not just a sign.
    (declare (optimize (speed 3) (safety 2)))
(let* ((winning (parse-fen "k7/8/8/3q4/4P3/8/8/K7 w - - 0 1"))
         (losing (parse-fen "k7/8/8/3p4/4Q3/8/8/K7 w - - 0 1"))
         (quiet (parse-fen "k7/8/8/8/4P3/8/8/K7 w - - 0 1")))
    (assert-equal 800 (see-sign winning (uci-to-move "e4d5" winning))
                  "pawn captures queen SEE swing")
    (assert-equal -800 (see-sign losing (uci-to-move "e4d5" losing))
                  "queen captures pawn SEE swing")
    (assert-equal 0 (see-sign quiet (uci-to-move "e4e5" quiet))
                  "quiet move SEE swing is zero"))
  ;; En-passant captures land on an empty square but are still tactical moves for
  ;; move ordering and quiescence; otherwise qsearch can stop before seeing the
  ;; forced pawn capture.
  (let* ((board (parse-fen "4k3/8/8/3pP3/8/8/8/4K3 w - d6 0 1"))
         (move (uci-to-move "e5d6" board))
         (legal (legal-moves board 'white)))
    (assert-true (member move legal :test #'equal-moves)
                 "en-passant move is legal")
    (assert-true (capture-move-p board move)
                 "en-passant move is classified as a capture")
    (assert-equal 0 (see-sign board move)
                  "en-passant pawn-for-pawn SEE swing")
    (assert-true (member move (next-qsearch-moves board legal) :test #'equal-moves)
                 "en-passant move is searched in quiescence"))
  ;; History updates are bounded and resettable.
  (let* ((board (initial-board))
         (move (uci-to-move "e2e4" board)))
    (clear-ordering-heuristics)
    (assert-equal 0 (history-score 'white move) "history starts clear")
    (dotimes (i 8)
      (bounded-history-update 'white move +history-max+))
    (assert-true (<= (history-score 'white move) +history-max+)
                 "history upper bound is enforced")
    (dotimes (i 16)
      (bounded-history-update 'white move (- +history-max+)))
    (assert-true (>= (history-score 'white move) (- +history-max+))
                 "history lower bound is enforced")
    (clear-ordering-heuristics)
    (assert-equal 0 (history-score 'white move) "history clears to zero"))
  ;; Killer moves accept only quiet non-duplicates.
  (let* ((board (initial-board))
         (quiet1 (uci-to-move "e2e4" board))
         (quiet2 (uci-to-move "d2d4" board))
         (capture-board (parse-fen "8/8/8/3p4/4P3/8/8/K6k w - - 0 1"))
         (capture (uci-to-move "e4d5" capture-board)))
    (clear-ordering-heuristics)
    (push-killer 0 quiet1 board)
    (push-killer 0 quiet1 board)
    (push-killer 0 quiet2 board)
    (push-killer 0 capture capture-board)
    (assert-true (equal-moves quiet2 (aref *killers* 0 0))
                 "new quiet killer is first")
    (assert-true (equal-moves quiet1 (aref *killers* 0 1))
                 "old quiet killer is second")
    (assert-true (not (equal-moves capture (aref *killers* 0 0)))
                 "capture is not inserted as killer"))
  ;; Countermove table stores a quiet reply keyed by the previous destination piece.
  (let* ((board (initial-board))
         (prev (uci-to-move "e2e4" board)))
    (execute-move board prev)
    (let ((reply (uci-to-move "e7e5" board)))
      (clear-ordering-heuristics)
      (store-countermove board prev reply)
      (assert-true (equal-moves reply (countermove-for board prev))
                   "countermove round trip"))))

(defun test-clustered-tt-and-tt-move-ordering ()
  ;; Mate score normalization must round-trip relative to the probing ply.
    (declare (optimize (speed 3) (safety 2)))
(assert-equal (+ +tt-mate-threshold+ 7)
                (score-from-tt (score-to-tt (+ +tt-mate-threshold+ 7) 4) 4)
                "positive mate score TT normalization")
  (assert-equal (- (- +tt-mate-threshold+) 7)
                (score-from-tt (score-to-tt (- (- +tt-mate-threshold+) 7) 4) 4)
                "negative mate score TT normalization")
  ;; Clustered TT stores and probes multiple keys sharing one cluster, then replaces
  ;; the weakest same-cluster entry.
  (let ((old-hash (tt-current-hash-mb)))
    (unwind-protect
         (progn
           (tt-resize 1)
           (tt-clear)
           (setf *tt-generation* 0)
           (let* ((stride (1+ *tt-mask*))
                  (keys (loop for i from 0 below 5 collect (+ 7 (* i stride))))
                  (boards (mapcar (lambda (key)
                                    (let ((b (initial-board)))
                                      (setf (board-key b) key)
                                      b))
                                  keys)))
             (loop for b in (subseq boards 0 4)
                   for depth from 1 do
                   (tt-store b depth (+ 100 depth) 'exact nil 0 nil))
             (loop for b in (subseq boards 0 4)
                   for depth from 1 do
                   (multiple-value-bind (hit score move)
                       (tt-probe b depth (- +mate-score+) +mate-score+ 0)
                     (declare (ignore move))
                     (assert-true hit "clustered TT probes stored key")
                     (assert-equal (+ 100 depth) score "clustered TT score round trip")))
             (tt-store (fifth boards) 8 508 'exact nil 0 nil)
             (multiple-value-bind (hit score move)
                 (tt-probe (fifth boards) 8 (- +mate-score+) +mate-score+ 0)
               (declare (ignore move))
               (assert-true hit "clustered TT probes replacement key")
               (assert-equal 508 score "clustered TT replacement score"))
             (multiple-value-bind (hit score move)
                 (tt-probe (first boards) 1 (- +mate-score+) +mate-score+ 0)
               (declare (ignore score move))
               (assert-true (not hit) "clustered TT replaces weakest entry"))))
      (tt-resize old-hash)))
  ;; A stored best move is still returned for move ordering when the stored
  ;; entry is too shallow to provide a score cutoff.
  (let* ((board (initial-board))
         (tt-move (uci-to-move "e2e4" board)))
    (tt-clear)
    (tt-store board 1 25 'exact tt-move 0 nil)
    (multiple-value-bind (hit score move)
        (tt-probe board 3 (- +mate-score+) +mate-score+ 0)
      (declare (ignore score))
      (assert-true (not hit) "shallow TT entry is not a score hit")
      (assert-true (equal-moves tt-move move) "shallow TT entry still returns move")))
  ;; Null-move TT clobber (#6): a null-move cutoff stores MOVE=NIL with a
  ;; LOWER bound.  When it replaces an existing entry at a DEEPER depth (so the
  ;; exact-demotion guard allows the replace), it must keep the existing real
  ;; hash move instead of clobbering it with NIL -- else the TT-move-first
  ;; ordering stage is lost and the tree re-searches.
  (let* ((board (initial-board))
         (tt-move (uci-to-move "e2e4" board)))
    (tt-clear)
    (tt-store board 4 80 'lower tt-move 0 nil)
    (tt-store board 5 90 'lower nil 0 nil)   ; deeper null-move fail-high
    (multiple-value-bind (hit score move)
        (tt-probe board 5 (- +mate-score+) +mate-score+ 0)
      (declare (ignore hit score))
      (assert-true (equal-moves tt-move move)
                   "null-move bound does not clobber existing hash move")))
  ;; Exact-demotion (#7): a non-exact bound at EQUAL depth must not overwrite an
  ;; existing EXACT entry.  Store exact 100, then a lower bound 50 at the same
  ;; depth; probe with window [75,76].  The exact score 100 is usable (cuts),
  ;; but a lower bound of 50 is not (50 < beta=76) -- so a preserved exact entry
  ;; yields a hit where a demoted lower bound would yield none.
  (let ((board (initial-board)))
    (tt-clear)
    (tt-store board 4 100 'exact nil 0 nil)
    (tt-store board 4 50 'lower nil 0 nil)
    (multiple-value-bind (hit score move)
        (tt-probe board 4 75 76 0)
      (declare (ignore move))
      (assert-true hit "equal-depth lower bound does not demote exact entry")
      (assert-equal 100 score "exact score preserved against equal-depth bound")))
  ;; TT move is legal-checked and removed from later stages exactly once.
  (let* ((board (initial-board))
         (legal (legal-moves board 'white))
         (tt-move (uci-to-move "e2e4" board)))
    (clear-ordering-heuristics)
    (multiple-value-bind (tt-first good-captures bad-captures killers counter quiets)
        (classify-moves board 'white legal tt-move nil 0)
      (assert-true (equal-moves tt-move tt-first) "legal TT move is first stage")
      (assert-true (not (move-in-list-p tt-move good-captures)) "TT move removed from good captures")
      (assert-true (not (move-in-list-p tt-move bad-captures)) "TT move removed from bad captures")
      (assert-true (not (move-in-list-p tt-move killers)) "TT move removed from killers")
      (assert-true (or (null counter) (not (equal-moves tt-move counter)))
                   "TT move removed from countermove")
      (assert-true (not (move-in-list-p tt-move quiets)) "TT move removed from quiets"))))

(defun test-hash-consistency ()
  "G9 gate (a): the incrementally-maintained Zobrist key must always equal the
from-scratch recompute, across side-to-move, castling, en-passant, promotion,
capture and repetition state; UNMAKE must restore the key exactly; and a
duplicated board must inherit the parent's key (regression guard for the
DUPLICATE-BOARD :key omission)."
    (declare (optimize (speed 3) (safety 2)))
(let ((startpos "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
        (kiwipete "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1")
        (ep-target "4k3/8/8/3pP3/8/8/8/4K3 w - d6 0 1")
        (black-to-move "r1bqk2r/pppp1ppp/2n2n2/2b1p3/2B1P3/2N2N2/PPPP1PPP/R1BQK2R b KQkq - 0 1"))
    ;; (A) DUPLICATE-BOARD must copy the key, and both the parent and the copy
    ;; must agree with the from-scratch recompute.  Before the fix the copy's
    ;; key defaulted to 0, so the second assertion here failed on every board.
    (dolist (fen (list startpos kiwipete ep-target black-to-move))
      (let* ((orig (parse-fen fen))
             (copy (duplicate-board orig)))
        (assert-equal (compute-board-key orig) (board-key orig)
                      "parsed board key matches recompute")
        (assert-equal (board-key orig) (board-key copy)
                      "duplicate-board copies the parent key")
        (assert-equal (compute-board-key copy) (board-key copy)
                      "duplicate-board key matches recompute")))
    ;; (B) The exact defect: an incremental update on a *duplicated* board must
    ;; still track the recompute.  With a key-0 copy this diverged immediately.
    (let* ((orig (parse-fen startpos))
           (copy (duplicate-board orig))
           (move (uci-to-move "e2e4" copy)))
      (execute-move copy move)
      (assert-equal (compute-board-key copy) (board-key copy)
                    "incremental key on a duplicated board matches recompute")))
  ;; (C) Make / incremental / unmake round-trip for each gate-named component.
  ;; Each entry plays one move and asserts: incremental == recompute after the
  ;; move, and UNMAKE restores the exact pre-move key.
  (flet ((check-move-hash (fen alg description)
           (let* ((board (parse-fen fen))
                  (key-before (board-key board))
                  (move (uci-to-move alg board)))
             (assert-equal (compute-board-key board) key-before
                           (format nil "~A: key consistent before move" description))
             (multiple-value-bind (b undo) (execute-move board move)
               (declare (ignore b))
               (assert-equal (compute-board-key board) (board-key board)
                             (format nil "~A: incremental key matches recompute" description))
               (unmake-move board move undo)
               (assert-equal key-before (board-key board)
                             (format nil "~A: unmake restores key" description))
               (assert-equal (compute-board-key board) (board-key board)
                             (format nil "~A: recompute consistent after unmake" description))))))
    ;; Quiet double-push: flips side to move and creates an en-passant file.
    (check-move-hash "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
                     "e2e4" "quiet pawn double-push (side + ep creation)")
    ;; Plain capture (rook takes rook).
    (check-move-hash "4k3/8/8/3r4/3R4/8/8/4K3 w - - 0 1"
                     "d4d5" "capture")
    ;; White kingside castling: changes castling rights and moves the rook.
    (check-move-hash "4k3/8/8/8/8/8/8/R3K2R w KQ - 0 1"
                     "e1g1" "white kingside castling")
    ;; Black queenside castling.
    (check-move-hash "r3k2r/8/8/8/8/8/8/4K3 b kq - 0 1"
                     "e8c8" "black queenside castling")
    ;; En-passant capture: removes a pawn from a square other than the target.
    (check-move-hash "4k3/8/8/3pP3/8/8/8/4K3 w - d6 0 1"
                     "e5d6" "en-passant capture")
    ;; Quiet promotion to queen.
    (check-move-hash "4k3/P7/8/8/8/8/8/4K3 w - - 0 1"
                     "a7a8q" "promotion")
    ;; Capture-promotion (pawn takes knight, promotes to queen).
    (check-move-hash "1n2k3/P7/8/8/8/8/8/4K3 w - - 0 1"
                     "a7b8q" "capture-promotion"))
  ;; (D) Repetition state: the incremental key must stay consistent across a
  ;; full repetition cycle and the history-based repetition detector must agree.
  (let ((board (parse-fen "8/8/8/8/8/8/8/4K2k w - - 0 1")))
    (dolist (alg '("e1d1" "h1h2" "d1e1" "h2h1"))
      (execute-move board (uci-to-move alg board))
      (assert-equal (compute-board-key board) (board-key board)
                    "incremental key consistent through repetition cycle"))
    (assert-true (not (draw-by-repetition-p board))
                 "second occurrence is not yet threefold (hash-based)")
    (dolist (alg '("e1d1" "h1h2" "d1e1" "h2h1"))
      (execute-move board (uci-to-move alg board))
      (assert-equal (compute-board-key board) (board-key board)
                    "incremental key consistent through second cycle"))
    (assert-true (draw-by-repetition-p board)
                 "third occurrence detected via consistent incremental keys"))
  ;; (E) Null move: the incrementally-maintained key after MAKE-NULL-MOVE must
  ;; equal the from-scratch recompute, and UNMAKE-NULL-MOVE must restore the
  ;; exact pre-null key.  The en-passant target FEN is the landmine -- a null
  ;; move forfeits the en-passant right, so that file's Zobrist value must be
  ;; XORed out of the key (the user's critical null-move correctness constraint).
  (flet ((check-null-hash (fen description)
           (let* ((board (parse-fen fen))
                  (key-before (board-key board)))
             (assert-equal (compute-board-key board) key-before
                           (format nil "~A: key consistent before null" description))
             (multiple-value-bind (b undo) (make-null-move board)
               (declare (ignore b))
               (assert-equal (compute-board-key board) (board-key board)
                             (format nil "~A: incremental key matches recompute after null" description))
               (unmake-null-move board undo)
               (assert-equal key-before (board-key board)
                             (format nil "~A: unmake-null restores key" description))
               (assert-equal (compute-board-key board) (board-key board)
                             (format nil "~A: recompute consistent after unmake-null" description))))))
    (check-null-hash "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
                     "null move (startpos)")
    (check-null-hash "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
                     "null move (kiwipete)")
    (check-null-hash "4k3/8/8/3pP3/8/8/8/4K3 w - d6 0 1"
                     "null move (en-passant target -- ep key must clear)")))

(defun test-fixed-seed-reproducibility ()
  "G9 gate (b): with a fixed Zobrist seed the pseudo-random stream is identical
run-to-run, key computation is a pure function of position, and a fixed-depth
search yields identical bestmove / node count / TT statistics on repeat runs
(determinism, plan rule 8)."
  ;; (1) The xorshift64* stream is a pure function of the seed: reset to the
  ;; documented constant, draw a sequence, reset again, draw again -- identical.
    (declare (optimize (speed 3) (safety 2)))
(let ((saved-seed *zobrist-seed*))
    (unwind-protect
         (flet ((draw-sequence (seed n)
                  (setf *zobrist-seed* seed)
                  (loop repeat n collect (zobrist-random))))
           (let ((seq-a (draw-sequence 88172645463325252 32))
                 (seq-b (draw-sequence 88172645463325252 32)))
             (assert-true (equal seq-a seq-b)
                          "fixed-seed Zobrist stream is reproducible")
             (assert-true (> (length (remove-duplicates seq-a)) 1)
                          "Zobrist stream is not degenerate (varies)")
             ;; A different seed must produce a different stream.
             (let ((seq-c (draw-sequence 1 32)))
               (assert-true (not (equal seq-a seq-c))
                            "different seed yields a different stream"))))
      ;; Restore the live seed so the global Zobrist state is untouched.
      (setf *zobrist-seed* saved-seed)))
  ;; (2) COMPUTE-BOARD-KEY is a pure function of position: the same FEN parsed
  ;; twice, and a duplicate, all yield the same key.
  (let ((fen "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"))
    (let ((k1 (board-key (parse-fen fen)))
          (k2 (board-key (parse-fen fen)))
          (k3 (compute-board-key (duplicate-board (parse-fen fen)))))
      (assert-equal k1 k2 "compute-board-key deterministic across parses")
      (assert-equal k1 k3 "compute-board-key deterministic across duplication")))
  ;; (3) End-to-end search reproducibility: a fixed-depth search from a fixed
  ;; position, run twice with a freshly-cleared TT and heuristics, must return
  ;; the same bestmove and the same node/TT-probe/TT-hit counts.
  (flet ((run-fixed-search (fen color depth)
           (let ((board (parse-fen fen))
                 (*uci-search-deadline* nil)
                 (*uci-search-stop-requested* nil))
             (reset-search-state)
             (multiple-value-bind (score move)
                 (choose-move-iterative board color depth)
               (declare (ignore score))
               (multiple-value-bind (probes hits move-hits misses)
                   (tt-statistics)
                 (declare (ignore move-hits misses))
                 (list (and move (move-to-uci move))
                       *search-nodes* probes hits))))))
    (let* ((fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
           (run-1 (run-fixed-search fen 'white 5))
           (run-2 (run-fixed-search fen 'white 5)))
      (assert-equal (first run-1) (first run-2)
                    "fixed-depth search reproduces bestmove")
      (assert-equal (second run-1) (second run-2)
                    "fixed-depth search reproduces node count")
      (assert-equal (third run-1) (third run-2)
                    "fixed-depth search reproduces TT probe count")
      (assert-equal (fourth run-1) (fourth run-2)
                    "fixed-depth search reproduces TT hit count"))))

(defun test-verified-null-move ()
  "G4 gate: verified null-move pruning (Romstad protocol, R=3).

Guards the *decision* logic of null-move pruning -- the eligibility predicate
and its zugzwang (non-pawn material) guard -- with pure-function assertions that
are stable across the later LMR / check-extension work, plus one behavioural
control proving the material guard renders the whole null-move subsystem inert
on a pawns-only position.  The empirical \"verification on solves, verification
off mis-solves\" node-count evidence on piece-bearing zugzwang positions is a
deterministic bench recorded in compilation/g4-gate.txt (it necessarily shifts
under LMR / extensions and so does not belong in the invariant suite)."
  ;; (A) SIDE-HAS-NON-PAWN-MATERIAL-P -- the zugzwang guard.
    (declare (optimize (speed 3) (safety 2)))
(let ((b (parse-fen +standard-start-position-fen+)))
    (assert-true (side-has-non-pawn-material-p b 'white)
                 "startpos: white has non-pawn material")
    (assert-true (side-has-non-pawn-material-p b 'black)
                 "startpos: black has non-pawn material"))
  (let ((b (parse-fen "4k3/pppppppp/8/8/8/8/PPPPPPPP/4K3 w - - 0 1")))
    (assert-true (not (side-has-non-pawn-material-p b 'white))
                 "king+pawns only: white has no non-pawn material")
    (assert-true (not (side-has-non-pawn-material-p b 'black))
                 "king+pawns only: black has no non-pawn material"))
  ;; Asymmetric: white keeps a knight, black is reduced to king and pawn.
  (let ((b (parse-fen "7k/6p1/8/8/8/8/8/5NK1 w - - 0 1")))
    (assert-true (side-has-non-pawn-material-p b 'white)
                 "asymmetric: white knight counts as material")
    (assert-true (not (side-has-non-pawn-material-p b 'black))
                 "asymmetric: black king+pawn has no material"))
  ;; (B) MAY-TRY-NULL-MOVE-P -- eligibility predicate.  Startpos white to move
  ;; is not in check and has material, so it is the eligible reference node.
  (let* ((b (parse-fen +standard-start-position-fen+))
         (c (board-side-to-move b)))
    (assert-true (not (checkp b c)) "startpos: side to move not in check")
    (assert-true (may-try-null-move-p t nil +null-move-min-depth+ nil b c t)
                 "eligible node attempts a null move")
    (assert-true (may-try-null-move-p t nil +null-move-min-depth+ nil b c nil)
                 "eligible node attempts a null move (verify off)")
    (assert-true (not (may-try-null-move-p t t +null-move-min-depth+ nil b c t))
                 "PV node never attempts a null move")
    (assert-true (not (may-try-null-move-p nil nil +null-move-min-depth+ nil b c t))
                 "allow-null nil (null child / verification subtree): no null")
    (assert-true (not (may-try-null-move-p t nil (1- +null-move-min-depth+) nil b c t))
                 "below +null-move-min-depth+: no null")
    (assert-true (not (may-try-null-move-p t nil +null-move-min-depth+ t b c t))
                 "side to move in check: no null"))
  ;; Zugzwang guard through the predicate: a pawns-only side must not attempt a
  ;; null move even when every other precondition holds.
  (let* ((b (parse-fen "4k3/pppppppp/8/8/8/8/PPPPPPPP/4K3 w - - 0 1"))
         (c (board-side-to-move b)))
    (assert-true (not (may-try-null-move-p t nil +null-move-min-depth+ nil b c t))
                 "pawns-only side: material guard suppresses null"))
  ;; (C) Behavioural control.  The material guard makes the whole null-move
  ;; subsystem inert on a pawns-only position: with no non-pawn material,
  ;; MAY-TRY-NULL-MOVE-P is nil at every node and the null-move block never runs.
  ;; A non-PV search with verification on, with verification off, and with null
  ;; moves disabled at the root must therefore be byte-identical in bestmove,
  ;; score AND node count -- null is their only differentiator and the guard
  ;; removes it.  This invariant applies equally under LMR and check extensions.
  ;;
  ;; The control FEN MUST keep every pawn far from promotion.  A promoting pawn
  ;; reintroduces non-pawn material mid-search, which re-arms null; and once a
  ;; check extension reshapes the post-promotion checking subtree, verified vs
  ;; unverified null then legitimately diverge in NODE COUNT (same bestmove and
  ;; score, different tree) -- which is precisely what breaks the invariant.  All
  ;; pawns therefore sit on their home rank: six pushes from promotion, which is
  ;; unreachable within a depth-6 search (a quiet pawn advance is never a check,
  ;; so the extension cannot carry a promoting line to depth), guaranteeing that
  ;; no non-pawn material ever appears and null stays genuinely inert.
  (flet ((search-result (fen depth allow-null verify)
           (let ((board (parse-fen fen))
                 (*uci-search-deadline* nil)
                 (*uci-search-stop-requested* nil))
             (reset-search-state)
             (setf *search-nodes* 0 *qsearch-nodes* 0
                   *search-seldepth* 0 *search-aborted* nil)
             (multiple-value-bind (score move)
                 (pvs-search board depth (- +mate-score+) +mate-score+ 0
                             :pv-node-p nil :allow-null allow-null :verify verify)
               (list (and move (move-to-uci move)) score *search-nodes*)))))
    (let* ((fen "4k3/ppp2ppp/8/8/8/8/PPP2PPP/4K3 w - - 0 1")
           (no-null (search-result fen 6 nil t))
           (verify-on (search-result fen 6 t t))
           (verify-off (search-result fen 6 t nil)))
      (assert-equal no-null verify-on
                    "pawns-only (no promotion): verified null == no-null (null inert)")
      (assert-equal no-null verify-off
                    "pawns-only (no promotion): plain null == no-null (null inert)"))))

(defun test-additional-gates ()
    (declare (optimize (speed 3) (safety 2)))
(test-iterative-info-search)
  (test-pv-reporting-fen)
  (test-pvs-vs-alpha-beta-entrypoint)
  (test-go-argument-robustness)
  (test-mate-score-sign)
  (test-no-rim-knight-blunder-fens)
  (test-see-and-ordering-heuristics)
  (test-clustered-tt-and-tt-move-ordering)
  (test-hash-consistency)
  (test-fixed-seed-reproducibility)
  (test-verified-null-move))

;;;; Evaluation colour-symmetry (differential correctness oracle)

(defun swap-fen-case (s)
  "Swap the case of every alphabetic char in S (digits/other unchanged)."
    (declare (optimize (speed 3) (safety 2)))
(map 'string
       (lambda (c)
         (cond ((upper-case-p c) (char-downcase c))
               ((lower-case-p c) (char-upcase c))
               (t c)))
       s))

(defun join-fen-fields (list sep)
    (declare (optimize (speed 3) (safety 2)))
(with-output-to-string (o)
    (loop for x in list for first = t then nil
          do (unless first (write-char sep o))
             (write-string x o))))

(defun flip-fen-ep (ep)
  "Vertically mirror an en-passant square (e.g. e3 <-> e6); pass \"-\" through."
    (declare (optimize (speed 3) (safety 2)))
(if (or (string= ep "-") (< (length ep) 2))
      ep
      (format nil "~A~A" (char ep 0) (- 9 (digit-char-p (char ep 1))))))

(defun flip-fen (fen)
  "Return the colour-and-rank mirror of FEN: reflect ranks (i -> 7-i) and swap
piece colours.  A correct white-relative static eval must satisfy
  (fev (parse-fen (flip-fen f))) = (- (fev (parse-fen f)))
for every legal position F."
    (declare (optimize (speed 3) (safety 2)))
(let* ((parts (split-string (string-trim " " fen)))
         (placement (first parts))
         (stm (second parts))
         (castling (third parts))
         (ep (fourth parts))
         (half (fifth parts))
         (full (sixth parts))
         (ranks (split-string placement #\/)))
    (join-fen-fields
     (list (join-fen-fields (mapcar #'swap-fen-case (reverse ranks)) #\/)
           (cond ((string= stm "w") "b") ((string= stm "b") "w") (t stm))
           (if (string= castling "-") "-" (swap-fen-case castling))
           (flip-fen-ep ep)
           (or half "0")
           (or full "1"))
     #\Space)))

(defparameter *eval-symmetry-fixtures*
  '("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
    "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1"
    "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8"
    "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 0 1"
    "r1bqkbnr/pppp1ppp/2n5/4p3/2B1P3/5N2/PPPP1PPP/RNBQK2R b KQkq - 3 3"
    "r1bqkb1r/pp2pppp/2np1n2/8/3NP3/2N5/PPP2PPP/R1BQKB1R w KQkq - 0 6"
    "8/8/8/4k3/8/4K3/4P3/8 w - - 0 1"
    "8/8/8/8/8/4k3/R7/4K3 w - - 0 1"
    "8/8/8/4k3/8/8/8/1N2K1N1 w - - 0 1"
    "k7/8/8/8/8/2K5/2B5/1N6 w - - 0 1"
    "1n6/2b5/2k5/8/8/8/8/K7 b - - 0 1"
    "k7/8/8/8/8/2K5/8/1Q6 w - - 0 1"
    "1q6/8/2k5/8/8/8/8/K7 b - - 0 1"
    "k7/8/8/8/8/2K5/8/1R6 w - - 0 1"
    "1r6/8/2k5/8/8/8/8/K7 b - - 0 1"
    "k7/8/8/8/8/2K5/3B4/1B6 w - - 0 1"
    "1b6/3b4/2k5/8/8/8/8/K7 b - - 0 1"
    "r1bqk2r/pppp1ppp/2n2n2/2b1p3/2B1P3/2N2N2/PPPP1PPP/R1BQK2R w KQkq - 6 5")
  "Varied legal positions (openings, middlegames, endgames, drawish material)
used as fixtures for the evaluation colour-symmetry check.")

(defun test-eval-symmetry ()
  "The static eval must be exactly antisymmetric under a colour-and-rank mirror:
mirroring a position negates its white-relative score.  This is a differential
correctness oracle (independent of any subjective eval-quality judgement) and
guards future incremental-evaluation work against colour-dependent drift."
    (declare (optimize (speed 3) (safety 2)))
(dolist (fen *eval-symmetry-fixtures*)
    (let* ((original (fev (parse-fen fen)))
           (mirrored (fev (parse-fen (flip-fen fen)))))
      (assert-equal (- original) mirrored
                    (format nil "eval symmetry for ~A" fen)))))


(defun test-kbnk-endgame-heuristic ()
  "KBNK remains a playable endgame, but its static eval should guide the bare
king toward the bishop-coloured mating corner and stay colour-symmetric."
    (declare (optimize (speed 3) (safety 2)))
  (let* ((white-correct (parse-fen "k7/8/8/8/8/2K5/2B5/1N6 w - - 0 1"))
         (white-wrong (parse-fen "7k/8/8/8/8/2K5/2B5/1N6 w - - 0 1"))
         (black-correct (parse-fen "1n6/2b5/2k5/8/8/8/8/K7 b - - 0 1"))
         (black-wrong (parse-fen "1n6/2b5/2k5/8/8/8/8/7K b - - 0 1")))
    (assert-true (not (insufficient-material-p white-correct))
                 "KBNK is not automatic insufficient material")
    (assert-true (> (fev white-correct) (fev white-wrong))
                 "white KBNK prefers the bishop-coloured corner")
    (assert-true (< (fev black-correct) (fev black-wrong))
                 "black KBNK prefers the bishop-coloured corner")
    (assert-equal (- (fev white-correct)) (fev black-correct)
                  "KBNK correct-corner eval is colour-symmetric")
    (assert-equal (- (fev white-wrong)) (fev black-wrong)
                  "KBNK wrong-corner eval is colour-symmetric")))


(defun test-elementary-lone-king-endgame-heuristics ()
  "KQK, KRK and KBBK evals should drive the bare king to edge/corner squares
without leaking into drawn same-colour-bishop material."
    (declare (optimize (speed 3) (safety 2)))
  (labels ((score (fen) (fev (parse-fen fen))))
    (let ((kqk-good "k7/8/8/8/8/2K5/8/1Q6 w - - 0 1")
          (kqk-bad "8/8/8/3k4/8/2K5/8/1Q6 w - - 0 1")
          (black-kqk-good "1q6/8/2k5/8/8/8/8/K7 b - - 0 1"))
      (assert-true (> (score kqk-good) (score kqk-bad))
                   "KQK prefers the bare king on a corner/edge")
      (assert-equal (- (score kqk-good)) (score black-kqk-good)
                    "KQK eval is colour-symmetric"))
    (let ((krk-good "k7/8/8/8/8/2K5/8/1R6 w - - 0 1")
          (krk-bad "8/8/8/3k4/8/2K5/8/1R6 w - - 0 1")
          (black-krk-good "1r6/8/2k5/8/8/8/8/K7 b - - 0 1"))
      (assert-true (> (score krk-good) (score krk-bad))
                   "KRK prefers the bare king on a corner/edge")
      (assert-equal (- (score krk-good)) (score black-krk-good)
                    "KRK eval is colour-symmetric"))
    (let ((kbbk-good "k7/8/8/8/8/2K5/3B4/1B6 w - - 0 1")
          (kbbk-bad "8/8/8/3k4/8/2K5/3B4/1B6 w - - 0 1")
          (black-kbbk-good "1b6/3b4/2k5/8/8/8/8/K7 b - - 0 1")
          (same-colour (parse-fen "k7/8/8/8/8/2K5/2B5/1B6 w - - 0 1")))
      (assert-true (> (score kbbk-good) (score kbbk-bad))
                   "KBBK prefers the bare king on a corner/edge")
      (assert-equal (- (score kbbk-good)) (score black-kbbk-good)
                    "KBBK eval is colour-symmetric")
      (assert-true (insufficient-material-p same-colour)
                   "same-colour KBB-K remains insufficient material"))))


(defun test-conservative-endgame-scaling ()
  "Conservative endgame heuristics should prefer progress but scale down known
fortress/drawish cases instead of treating them as clean tablebase wins."
    (declare (optimize (speed 3) (safety 2)))
  (labels ((score (fen) (fev (parse-fen fen))))
    (let ((bpk-good "4k3/8/3P4/3K4/8/8/2B5/6b1 w - - 0 1")
          (bpk-blocked "8/8/3k4/3P4/3K4/8/2B5/6b1 w - - 0 1")
          (bpk-wrong "7k/P7/K7/8/8/8/1B6/6b1 b - - 0 1"))
      (assert-true (> (score bpk-good) (score bpk-blocked))
                   "BPK-v-BK prefers supported progress over king blockade")
      (assert-true (< (score bpk-wrong) (score bpk-good))
                   "BPK-v-BK wrong rook-pawn confidence is scaled down"))
    (let ((npk-good "4k3/8/3P4/3K4/8/2N5/8/6b1 w - - 0 1")
          (npk-blocked "8/8/3k4/3P4/3K4/2N5/8/6b1 w - - 0 1"))
      (assert-true (> (score npk-good) (score npk-blocked))
                   "NPK-v-BK prefers unblocked supported progress"))
    (let ((bpk-n-good "4k3/8/3P4/3K4/8/8/2B5/6n1 w - - 0 1")
          (bpk-n-blocked "8/8/3k4/3P4/3K4/8/2B5/6n1 w - - 0 1"))
      (assert-true (> (score bpk-n-good) (score bpk-n-blocked))
                   "BPK-v-NK prefers unblocked supported progress"))
    (let ((npk-n-good "4k3/8/3P4/3K4/8/2N5/8/6n1 w - - 0 1")
          (npk-n-blocked "8/8/3k4/3P4/3K4/2N5/8/6n1 w - - 0 1"))
      (assert-true (> (score npk-n-good) (score npk-n-blocked))
                   "NPK-v-NK prefers unblocked supported progress"))
    (let ((krkb "k7/8/8/8/8/2K5/8/1R5b w - - 0 1")
          (krkn "k7/8/8/8/8/2K5/8/1R5n w - - 0 1")
          (kqkr "k7/8/8/8/8/2K5/8/1Q5r w - - 0 1"))
      (assert-true (< (- (score krkb) +rook-value+) 200)
                   "KR-v-BK is draw-scaled, not a large clean win")
      (assert-true (< (- (score krkn) +rook-value+) 200)
                   "KR-v-NK is draw-scaled, not a large clean win")
      (assert-true (> (score kqkr) (score krkb))
                   "KQ-v-KR remains queen-favouring"))
    (let ((qvp-ordinary "8/8/8/4k3/8/4K3/1p6/Q7 w - - 0 1")
          (qvp-drawish "K7/P7/8/8/8/8/8/q5k1 b - - 0 1"))
      (assert-true (> (score qvp-ordinary) (score qvp-drawish))
                   "Q-v-P scales down supported rook-pawn seventh-rank motifs"))))


(defun run-self-tests ()
  "Run all self-tests.  Exit status reflects pass/fail."
    (declare (optimize (speed 3) (safety 2)))
(setf *tests-passed* 0)
  (setf *tests-failed* 0)
  (test-fen-parser)
  (test-starting-position)
  (test-move-generation)
  (test-uci-notation)
  (test-checkmate)
  (test-stalemate)
  (test-castling)
  (test-promotion)
  (test-special-moves)
  (test-draw-rules)
  (test-eval-symmetry)
  (test-kbnk-endgame-heuristic)
  (test-elementary-lone-king-endgame-heuristics)
  (test-conservative-endgame-scaling)
  (test-time-management)
  (test-uci-identification)
  (test-uci-position-move-validation)
  (test-hash-option)
  (test-perft-regression)
  (test-attack-detection)
  (test-additional-gates)
  (format t "~%Self-test results: ~A passed, ~A failed.~%"
          *tests-passed* *tests-failed*)
  (force-output)
  (when (> *tests-failed* 0)
    (exit-process 1)))


(defun perft-count (board color depth)
  "Count legal leaf nodes at DEPTH for COLOR."
    (declare (optimize (speed 3) (safety 2)))
(if (<= depth 0)
      1
      (let ((moves (legal-moves board color))
            (nodes 0)
            (undo (make-move-undo)))
        (dolist (mv moves)
          (execute-move board mv undo)
          (unwind-protect
               (incf nodes (perft-count board (invert-color color) (1- depth)))
            (unmake-move board mv undo)))
        nodes)))


(defun test-perft-regression ()
  ;; Known perft values for the starting position.
    (declare (optimize (speed 3) (safety 2)))
(let ((board (parse-fen +standard-start-position-fen+)))
    (assert-equal 20 (perft-count board 'white 1) "perft depth 1")
    (assert-equal 400 (perft-count board 'white 2) "perft depth 2")
    (assert-equal 8902 (perft-count board 'white 3) "perft depth 3")
    (assert-equal 197281 (perft-count board 'white 4) "perft depth 4")
    (assert-equal 4865609 (perft-count board 'white 5) "perft depth 5"))
  ;; Castling-rich position.
  (let ((board (parse-fen "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 0 1")))
    (assert-equal 25 (perft-count board 'white 1) "r3k2r perft depth 1")
    (assert-equal 625 (perft-count board 'white 2) "r3k2r perft depth 2")
    (assert-equal 15206 (perft-count board 'white 3) "r3k2r perft depth 3")
    (assert-equal 369906 (perft-count board 'white 4) "r3k2r perft depth 4"))
  ;; Kiwipete (Peter McKenzie's standard perft position). This is the primary
  ;; guard against the queenside-castling defect fixed in 0.95.8: the engine
  ;; formerly generated O-O-O without requiring the b-file square (b1/b8) to be
  ;; empty, so after a knight reached b1 (e.g. Nc3-b1) an illegal e1c1 was
  ;; emitted. That defect inflated the depth-3 count from 97862 to 97903 (+41).
  ;; If this ever regresses, the depth-3 assertion below fails immediately.
  (let ((board (parse-fen
                "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1")))
    (assert-equal 48 (perft-count board 'white 1) "kiwipete perft depth 1")
    (assert-equal 2039 (perft-count board 'white 2) "kiwipete perft depth 2")
    (assert-equal 97862 (perft-count board 'white 3) "kiwipete perft depth 3")
    (assert-equal 4085603 (perft-count board 'white 4) "kiwipete perft depth 4"))
  ;; CPW position 3 (endgame with en-passant and pawn edge cases).
  (let ((board (parse-fen "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1")))
    (assert-equal 14 (perft-count board 'white 1) "cpw3 perft depth 1")
    (assert-equal 191 (perft-count board 'white 2) "cpw3 perft depth 2")
    (assert-equal 2812 (perft-count board 'white 3) "cpw3 perft depth 3")
    (assert-equal 43238 (perft-count board 'white 4) "cpw3 perft depth 4"))
  ;; CPW position 5 (promotion-rich, restricted castling rights).
  (let ((board (parse-fen "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8")))
    (assert-equal 44 (perft-count board 'white 1) "cpw5 perft depth 1")
    (assert-equal 1486 (perft-count board 'white 2) "cpw5 perft depth 2")
    (assert-equal 62379 (perft-count board 'white 3) "cpw5 perft depth 3"))
  ;; CPW position 6 (dense middlegame, no castling rights).
  (let ((board (parse-fen
                "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10")))
    (assert-equal 46 (perft-count board 'white 1) "cpw6 perft depth 1")
    (assert-equal 2079 (perft-count board 'white 2) "cpw6 perft depth 2")
    (assert-equal 89890 (perft-count board 'white 3) "cpw6 perft depth 3")))


(defun test-attack-detection ()
  ;; Regression: a rook on a3 must not be treated as attacking e7 diagonally.
    (declare (optimize (speed 3) (safety 2)))
(let ((board (parse-fen "4k3/8/8/8/8/R7/8/4K3 b - - 0 1")))
    (assert-true (member (uci-to-move "e8e7" board) (legal-moves board 'black)
                         :test #'equal-moves)
                 "black king can move to e7 with white rook on a3"))
  ;; Regression: pawn attacks must use the correct source square.
  (let ((board (parse-fen "3k4/8/8/8/1p6/8/3K4/8 w - - 0 1")))
    (assert-true (square-attacked-p board 5 2 'black)
                 "black pawn on b4 attacks c3")
    (assert-true (null (member (uci-to-move "d2c3" board) (legal-moves board 'white)
                               :test #'equal-moves))
                 "white king cannot move into pawn attack on c3")))
