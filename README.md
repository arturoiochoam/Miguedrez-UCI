# Miguedrez UCI 0.95.11.5

Miguedrez is a procedural Common Lisp chess engine by Manuel Felipe Gamallo Rivero. The 2026 line adds a Universal Chess Interface (UCI) front end, Windows release builds, stricter chess-rule handling, and a deeper search core.

> **Authorship**
> - **Manuel Felipe Gamallo Rivero**: original Miguedrez 0.95 (`manuf_81 at yahoo.com`).
> - Legacy Links: https://sourceforge.net/projects/miguedrez/; https://github.com/html/cl-chess/tree/master
> - **Arthur Matheus**: 2026 UCI front end, Windows release work, rule fixes, search work, tests, and later 0.95 releases.

## Licence

- GNU General Public Licence version 3.0. See `COPYING`.

## Language: Common Lisp

## Changelog

### v0.95.11.5

Feature set:
- En-passant hash rights now use `ep-capture-legal-p` before the file enters the Zobrist key. The check covers pinned adjacent pawns, including horizontal and diagonal pin cases.
- `maybe-set-enpassant-right` now runs after the double-pushed pawn reaches its target square, so the legality probe sees the real post-move board.

Fixes:
- `tt-store` keeps an existing hash move when a null-move cutoff stores `move=NIL`.
- `tt-store` no longer replaces an equal-depth exact transposition-table (TT) entry with a non-exact bound. Deeper bounds still replace shallower entries.
- Principal Variation Search (PVS) and late-move reduction re-searches stop when `*search-aborted*` is set.
- `update-board-key-after-move` now treats a nil pawn promotion on the last rank as a queen, matching `promote-if-needed`.
- `stop-search-and-join` reports an `info string` when a search thread does not join inside the timeout.

Code and tests:
- `legal-moves` passes its known in-check status through `children`, `children-piece`, and `possible-king`, removing a repeated king-attack scan.
- The unused `apply-uci-moves` helper was removed. Its test now calls `apply-uci-moves-to-board` on a local board.
- Self-tests increased from 344 to 351. Perft fixtures stayed byte-identical. UCI smoke tests covered `startpos` and pinned-en-passant positions.
- The engine-regression baseline needs a new comparison set because the en-passant hash and TT changes alter Zobrist keys and table contents.

### v0.95.11.4

Feature set:
- Added exact-material endgame terms for KQK, KRK, and KBBK with opposite-coloured bishops.
- Added terms for minor-plus-pawn versus minor, KQKR, KRKB, KRKN, and KQKP.
- KBBK with same-colour bishops remains an insufficient-material draw case.

Fixes:
- The new endgame terms stay inside evaluation. They do not change move generation, draw adjudication, mate scores, TT storage, pruning, or UCI protocol output.
- Drawish patterns receive explicit scaling: wrong-bishop rook pawns, blockades, bare rook-versus-minor endings, and rook-pawn or bishop-pawn seventh-rank queen-versus-pawn motifs with king cover.

Code and tests:
- Added shared square-index helpers: `pos-square-index`, `move-origin-index`, and `move-target-index`.
- `pawn-attack-direction` and `parse-fen-nonnegative-integer` now serve more call sites. Queen ray generation now uses one `scan-rays` call.
- Repetition detection stops after the second prior match. TT allocation now uses `tt-ensure-sized`.
- Principal variation lines are rebuilt from TT entries on the active board. UCI dispatch reads the first token without splitting the full command line.
- Added tests for conservative endgame scaling and kept the colour-symmetry evaluation oracle.
- Build metadata moved to 0.95.11.4.

### v0.95.11.3

Feature set:
- Documented the implemented endgame conversion terms for KNBK, KBBK, KQK, and KRK.
- The notes cover corner driving, board-edge confinement, attacking-king activity, piece coordination, and mating-net pressure.

Fixes:
- Corrected release text around the 0.95.11.3 endgame note set.

Code and tests:
- Version metadata, UCI identity, executable name, and ZIP naming were set for the 0.95.11.3 release.
- `build_uci.bat` was rerun with self-test, perft, tactical, and UCI identity gates.

### v0.95.11.2

Feature set:
- No playing-strength feature entered this release. The work focused on Common Lisp source review.

Fixes:
- Normalised selected declaration and body indentation.
- Documented small legacy helpers that lacked top-level docstrings.
- Made the `assert-true` test macro once-only through a gensym.

Code and tests:
- Reviewed `documentation/doc/lisp.md`, `documentation/doc/lisp/common-lisp-pro/SKILL.md`, and the related `common-lisp-pro/references/` files.
- Kept the Steel Bank Common Lisp (SBCL) compiler policy unchanged for the release build.
- Build metadata moved to 0.95.11.2, followed by self-test, perft, tactical, and UCI identity gates.

### v0.95.11.1

Feature set:
- No search or evaluation feature entered this release. The work focused on Common Lisp review findings.

Fixes:
- Made `board-pos` once-only for its `POS` argument, so macro expansion no longer repeats computed position forms.
- Added docstrings to small move-generation and UCI helpers.

Code and tests:
- Reviewed `documentation/doc/lisp2.md` and `documentation/doc/skill2.md` for macro hygiene, package boundaries, declarations, and release tooling.
- Kept the SBCL compiler policy unchanged from the 0.95.11.2 release note.
- Build metadata moved to 0.95.11.1, followed by self-test, perft, tactical, and UCI identity gates.

### v0.95.11

Feature set:
- This release kept the 0.95.10 search and evaluation shape. The work centred on concurrency, UCI game-over handling, and release audit items.

Fixes:
- `handle-uci-go` now joins the previous search before it calls `game-over-p`, copies the board, or filters `searchmoves`.
- Rules-draw positions no longer return `bestmove (none)` while legal moves remain. Only checkmate and stalemate use the no-move response.
- Process-exit calls now pass through `exit-process`, keeping the SBCL-specific boundary in one helper.
- The README and release metadata were refreshed for the 0.95.11 package.

Code and tests:
- Verified that all 268 top-level `defun` forms in `src/` carry local speed/safety compiler declarations.
- Re-ran the 732-position regression suite. The draw-path fix removed the 12 `played=None` failures tied to bare-king and 50-move fixtures.
- Reviewed the high-failure castling group. The inspected cases showed legal castling choices rejected by search, not by move legality.

### v0.95.10

Feature set:
- Added minor-piece stability and outpost evaluation through `minor-safety-score`, `minor-advance-depth`, `pawn-push-can-attack-p`, and `pawn-can-ever-attack-p`.
- Raised the centre-pawn occupation term and rebalanced development and uncastled-king weights.

Fixes:
- Removed the loose advanced-minor judgement behind the reported Alekhine `...Nf6-e4` and `...Nd5-f4` lines.
- Removed mirrored UCI side-to-move state. UCI now reads the side to move from the active board.
- Hardened Forsyth-Edwards Notation (FEN) and move edge cases: non-capturable en-passant canonicalisation, legal checks in `make-move-on-board`, unbounded clocks, and checkmate precedence over draw adjudication.

Code and tests:
- Added local speed/safety compiler declarations across source functions that lacked them.
- `safe-parse-int`, `pawnp`, `capture-move-p`, and `castling-right-available-p` now serve core paths.
- Removed redundant move-undo promotion state and redundant TT wrapper state.
- Moved `go searchmoves` filtering to root search handling.
- Perft stayed unchanged. The self-test suite stayed at 303 passing tests.
- Release scripts now use a 4096 MB dynamic space and 12 MB control stack. Packaging writes the 0.95.10 Windows executable and ZIP.

### v0.95.9

Feature set:
- Legal move generation, castling, en-passant, Zobrist hashing, TT handling, null-move search, and UCI input all received correctness gates.

Fixes:
- Removed the dead `board-whites-initial-pos` field. UCI positions always use white at the bottom, so the alternate orientation branches in evaluation were unreachable.
- `insufficient-material-p` now returns as soon as it sees a pawn, rook, or queen.
- Fixed console promotion input by passing the board into `read-move` and defaulting a last-rank pawn move to queen promotion.
- Console input now catches reader errors, validates ranks from `1..8`, and reports true draws as draws instead of stalemates.
- Added the missing same-side same-colour-bishop insufficient-material case.
- Castling generation now checks that the king starts on e1 or e8 before emitting a castling move.
- `possible-king` computes the in-check status once and shares it with both castling checks.

Code and tests:
- Reused shared move-direction and ray helpers across mobility, attack, checkers, pin-map, castling, pawn generation, tests, and console input.
- Simplified several duplicated search and evaluation helpers without changing move choice.
- Perft remained byte-identical across the five tracked fixtures. Self-tests and tactical tests passed after the edit batches.
- Rebuilt the 0.95.9 executable and ZIP from the edited source.

### v0.95.8

Feature set:
- Rewrote the search core from a white-relative min-max PVS to a single-perspective negamax search.
- Added a pin-aware and checkers-aware legal-move filter based on `checkers-of` and `pin-map`.

Fixes:
- Corrected a beta-cutoff ordering bug where `tt-store` and quiet-move learning read the child position instead of the parent position.
- `ucinewgame` now resets killer, history, and countermove tables with the TT.
- Countermoves are checked against the legal move list before search uses them.
- Malformed UCI and FEN input now signals typed conditions inside one UCI command handler.
- `go infinite` now ignores clock context and waits for `stop`.
- Missing or malformed `Hash` values no longer wipe the TT.

Code and tests:
- Extracted `scan-rays`, shared knight deltas, and `reset-search-state`.
- Confirmed the G11 Windows build pattern, compiler policy, self-test gate, executable self-test gate, and UCI handshake.
- Rebuilt the 0.95.8 Windows executable and ZIP.

### v0.95.7

Feature set:
- Raised generic positional weights without hardcoded opening moves: knight rim penalties, uncastled-king risk, early wing-pawn-push risk, allocation-free minor mobility, and bishop-development penalties.
- Quiescence search now searches all legal evasions when the side to move is in check.
- TT probing now returns a stored move for ordering even when the entry cannot return a score cutoff.

Fixes:
- Mate scoring now uses ply distance instead of remaining depth.

Code and tests:
- Added tests for root mate-score distance, shallow TT move retrieval, and the reported opening-blunder family.
- Updated Windows build scripts and distribution names for 0.95.7.

### v0.95.6

Feature set:
- Added generic positional terms: knight rim penalties, development balance, uncastled-king and wing-pawn-push risk penalties, castled-king bonus, and centre-pawn bonus.
- Reworked current search ordering around legal hash moves, Most Valuable Victim / Least Valuable Aggressor (MVV-LVA) captures, static-exchange-evaluation buckets, bounded butterfly history, killers, and countermoves.
- Quiescence search uses staged tactical ordering and static-exchange-evaluation pruning for losing captures.
- Replaced the earlier direct/two-slot TT with four-entry clustered TT buckets.

Fixes:
- Corrected UCI identity and version metadata.
- Corrected terminal checkmate score sign so a mated side receives a losing score.

Code and tests:
- Added TT generation ageing, replacement scoring, principal-variation and bound bonuses, and mate-score normalisation.
- Added tests for mate signs, static-exchange-evaluation swing values, history bounds, killer filtering, countermoves, clustered TT replacement, mate-score TT normalisation, and TT move deduplication.
- Removed dead `make-promotion-moves` and `enpassant-square` paths.
- Updated the Windows build and packaging flow with separate-process test gates and SHA-256 output.

### v0.95.5

Feature set:
- Added iterative-deepening PVS reporting over UCI with `info depth`, score, nodes, nodes per second, elapsed time, hash fullness, and principal variation before `bestmove`.
- Added the UCI `Hash` spin option with range `1..4096` MB.
- TT allocation can now resize at run time through `setoption name Hash value <mb>`.

Fixes:
- Hash values are clamped to the legal range and wipe the table only when the size changes.

Code and tests:
- Added tests for Hash parsing, Hash range handling, iterative `info` output, principal-variation legality, and strict insufficient-material cases.
- Rechecked legal move generation, special moves, draw rules, and time-management paths.
- Built the 0.95.5 Windows ZIP with SBCL 2.6.6.

### v0.95.4

Feature set:
- No chess-rule or search feature entered this release.

Fixes:
- No discrete playing-rule fix entered this release.

Code and tests:
- Ran a measured Common Lisp tuning pass.
- Added benchmark scripts and baseline reports under `benchmark/`.

### v0.95.3

Feature set:
- Added MVV-LVA material dispatch for move ordering.
- Added a direct-check bonus for quiet moves.

Fixes:
- `stop` now sets the real UCI stop flag, so `go infinite` can stop.
- Castling attack checks now test both destination and transit squares. Queenside castling checks both c1/c8 and d1/d8.
- Insufficient-material logic now follows the project’s Fédération Internationale des Échecs (FIDE) interpretation for `KN-KN` and `BN-BN` as non-automatic draws.
- `choose-move` no longer repeats move generation unnecessarily.

Code and tests:
- Rechecked the code against the Advanced Common Lisp Programming Guide and the project-specific Common Lisp notes.
- Added regression tests for the insufficient-material cases.
- Aligned `build/build-windows.lisp` with the Windows SBCL delivery settings used by the project.

### v0.95.2

Feature set:
- Added full promotion and underpromotion generation.
- Added draw-rule handling for threefold repetition, the 50-move rule, and insufficient material.
- Added move ordering for captures and promotions.
- Added capture-and-promotion quiescence search.
- Expanded UCI time controls to cover `go depth`, `go movetime`, clock plus increment, `movestogo`, and `go infinite` with deadline polling.

Fixes:
- Fixed queenside castling intermediate-square attack checks.
- Replaced all-moves attack detection with targeted `square-attacked-p` in legality checks.

Code and tests:
- Converted evaluation tables to `load-time-value` arrays and added material-value constants.
- Added tests for special moves, draw rules, and time-control parsing.

### v0.95.1 - 2026 and further releases

Feature set:
- First 2026 release with the UCI front end and the first UCI time-management path.
- Added `uci`, `isready`, `position`, `go`, `stop`, `quit`, and `ucinewgame` command handling.
- Added FEN parsing. The standard start position comes from `rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1`.
- Restructured the project as an ASDF system with explicit package definitions.
- Added per-side rook-moved flags for castling state.
- Added Windows x64 executable builds through SBCL 2.6.6 and `build_uci.bat`.

Fixes:
- Fixed alpha-beta termination on checkmate and stalemate positions.
- Source licence headers were set to GPLv3 to match `COPYING`.

Code and tests:
- Added tests for FEN parsing, move generation, checkmate, stalemate, castling, promotion, and UCI identification.
- Created the first 2026 Windows distribution ZIP.

### v0.95 - 2009-03-07 legacy baseline

- Source code was translated into English.

### v0.92 - 2009-02-26

- Move input changed from numeric coordinates such as `2 3 2 4` to square notation such as `b2 b3`.

### v0.91 - 2009-02-25

- Source-file encoding was corrected.

### v0.9 - 2009-02-21

- First published release.

## Architecture

### Overview

Miguedrez 0.95.11.5 keeps the original procedural Common Lisp style. The source tree is split by engine duty:

- `packages.lisp` declares the `miguedrez` package, exported API, and project-wide SBCL compiler policy.
- `data.lisp` owns board state, FEN parsing, Zobrist hashing, move and undo records, UCI square conversion, and position validation.
- `src/moves/*.lisp` contains piece generators, move execution, make/unmake, castling, en-passant, attack tests, pins, checkers, legal moves, checkmate, and stalemate.
- `draw.lisp` implements rules-draw detection.
- `ai.lisp` contains evaluation, move ordering, TT handling, iterative search, quiescence search, and UCI search counters.
- `uci.lisp` contains the UCI command loop, time allocation, threaded search, `Hash` option handling, `searchmoves`, and `info` output.
- `cli.lisp` and `io.lisp` keep the older console mode available for local play and debugging.
- `tests.lisp` holds the self-test and perft fixtures used by the release build.

Board model:
- The board stores an 8 by 8 piece array plus separate white and black occupancy arrays.
- Pieces use the original Spanish-style symbols: `pb/pn` pawns, `cb/cn` knights, `ab/an` bishops, `tb/tn` rooks, `db/dn` queens, `rb/rn` kings, and `vv` empty squares.
- Coordinates use row `0..7` from rank 8 down to rank 1 and column `0..7` from file a to h.
- `board` also stores side to move, halfmove clock, fullmove number, position-history keys, king positions, castling flags for each king and rook, en-passant flags, and a 64-bit Zobrist key.
- Make/unmake uses `move-undo` records and search-owned undo pools, so recursive search mutates one board and restores it instead of copying a board at every child.

FIDE rule handling:
- FEN parsing validates piece placement, side to move, castling text, en-passant square text, halfmove clock, and fullmove number.
- FEN validation also checks king count, king adjacency, and the rule that the side that just moved may not leave the opponent king in check.
- Castling requires the king on e1 or e8 and the corresponding rook on its home square.
- Castling also requires untouched king and rook flags, empty transit squares, no current check, and no attack on the king’s path or destination.
- En-passant generation checks the marked file and the captured pawn behind the target. En-passant hash rights are kept only when an adjacent pawn has a legal en-passant move after the king check.
- Promotion generation emits queen, rook, bishop, and knight promotions. Console input defaults a last-rank pawn move to queen promotion because the console notation has no promotion suffix.
- Legal moves use `checkers-of` and `pin-map` once per node. King moves and en-passant moves still use exact make/unmake validation because those cases can change attack geometry outside the ray filter.
- Draw detection covers threefold repetition, the 50-move rule, and FIDE dead-position material cases.
- The material cases are K versus K, K+B versus K, K+N versus K, same-colour KB versus KB, and same-side same-colour bishops against a lone king.
- It deliberately does not mark KNN versus K, KN versus KN, BN versus BN, or opposite-colour KB versus KB as automatic draws.
- `game-over-p` separates checkmate, stalemate, and rules-draw status. UCI returns `bestmove (none)` only for checkmate and stalemate.

Endgame handling:
- Terminal mate scores use mate distance, so faster mates score higher and delayed losses score less badly.
- Evaluation contains exact-material mating terms for KBNK, KQK, KRK, and opposite-coloured KBBK. These terms drive the bare king toward the correct corner or a board edge and reward attacking-king proximity.
- Further exact-material scaling handles minor-plus-pawn versus minor, queen versus rook, rook versus bishop, rook versus knight, and queen versus pawn.
- Drawish motifs receive penalties rather than tablebase claims: wrong-bishop rook pawns, frontal blockades, defender control of promotion squares, opposite-coloured bishop pawn endings, rook-versus-minor bare endings, and rook-pawn or bishop-pawn seventh-rank queen-versus-pawn positions with king cover.
- These endgame terms affect static evaluation only. Move legality, draw rules, mate scoring, and UCI output remain separate.

Hash table and Zobrist keys:
- Zobrist hashing uses deterministic 64-bit tables for pieces, side to move, castling rights, and en-passant files.
- The board key updates incrementally after every move and can be recomputed from scratch by `compute-board-key` for verification.
- Position history stores Zobrist keys. Repetition checks compare the active key with prior keys and stop after the second prior match.
- The TT uses four-entry clusters, a power-of-two cluster count, generation ageing, exact/lower/upper flags, mate-score normalisation, stored hash moves, and principal-variation bias in replacement scoring.
- The UCI `Hash` option accepts `1..4096` MB, clamps out-of-range values, and resizes the TT only when the value changes.
- `hashfull` samples same-generation TT entries and reports the UCI per-mille value.

Search:
- The main search is fail-soft negamax PVS with alpha-beta bounds and iterative deepening.
- Move ordering stages hash move, good captures, promotions, quiet checks, killers, countermove, quiet history moves, and bad captures.
- Capture ordering uses MVV-LVA with static-exchange-evaluation bucketing.
- Quiet beta cutoffs update bounded butterfly history, killer slots, and countermoves.
- Quiescence search extends captures and promotions, and searches all legal evasions while in check. Stand-pat is not used in check.
- Verified null-move pruning uses a non-pawn-material zugzwang guard and a verification pass before accepting the cutoff.
- Late-move reductions apply to quiet, late, non-checking moves outside principal-variation nodes, with a mandatory full-depth re-search when the reduced search raises alpha.
- Check extensions add one ply for checking moves and evasions, with a cap tied to root depth.
- Search polls `*uci-search-deadline*` and `*uci-search-stop-requested*` every 1024 nodes in both PVS and quiescence.
- UCI timed search runs on a board copy in an SBCL thread. `stop` requests cooperative exit and joins the thread before a new search can touch shared search state.

Evaluation:
- Material values are pawn 100, knight 300, bishop 300, rook 500, queen 900, and king 100000.
- Piece-square terms cover pawns, rooks, knights, and bishops.
- Opening and middlegame terms include centre-pawn occupation, development balance, castled-king bonus, uncastled-king penalties, wing-pawn-push penalties, minor mobility, bishop development, knight rim penalties, minor stability, pawn kicks, and knight outposts.
- All evaluation terms are white-relative in `fev`; `static-eval-stm` converts the result to side-to-move perspective for negamax.

UCI and release model:
- The engine handles `uci`, `isready`, `ucinewgame`, `position`, `go`, `stop`, `quit`, `setoption name Hash`, and `go searchmoves`.
- `go` accepts depth, movetime, clock plus increment, `movestogo`, and infinite search.
- Each completed iterative-deepening pass emits `info depth`, `seldepth`, score, nodes, nodes per second, `hashfull`, time, and principal variation.
- The Windows build runs separate-process self-tests, executable self-tests, packaging, and checksum writing through `build_uci.bat`.

### Legacy engine v0.95 and before

The archive `legacy/miguedrez_0.95.zip` contains the 2009 Common Lisp engine under `cl-chess-master/`. It is the baseline for the 2026 source tree.

Legacy layout:
- `miguedrez.lisp` defines the `miguedrez` package, positional weight arrays, the `ajz` console loop, and the manual-versus-auto game flow.
- `data.lisp` defines `board`, `player`, `pos`, and `move`, plus board-copy routines and two initial layouts: white at the bottom and white at the top.
- `io.lisp` prints the board and reads two-square console moves such as `A2 A4`.
- `ai.lisp` contains fixed-depth alpha-beta search and a material-plus-piece-square evaluator.
- `moves/` contains one file per piece plus shared move execution, en-passant state, legality checks, check detection, and checkmate detection.
- `cl-chess.asd` declares the ASDF system as `cl-chess` with project name `miguedrez`, version `0.9.5`, and licence `GPL v3`.

Legacy board and move model:
- The 0.95 board stores the piece array, white occupancy, black occupancy, white and black en-passant arrays, the initial white orientation, king positions, and king-moved flags.
- Moves store only source and target squares. Promotion piece choice does not exist in the move record.
- `copy-board` duplicates the full board for search and legality testing.
- The console game loop alternates a manual white player with an automatic black player by default.

Legacy rules and limitations:
- Castling checks use king-moved flags, empty squares, rook presence, and current king attack status. The old code does not keep per-rook moved flags.
- En-passant uses per-file arrays set after a double pawn push and wiped on the next move.
- Promotion always becomes a queen through `queen-if-promotion`.
- Legality testing copies the board, executes the move, and checks whether the king remains threatened.
- Checkmate detection searches all legal replies when the king is in check.
- The legacy code has no FEN parser, no UCI protocol, no halfmove or fullmove counters, no repetition table, no 50-move rule, and no insufficient-material adjudication.
- It also has no Zobrist key, no TT, no iterative deepening, no quiescence search, no null-move pruning, no late-move reductions, and no threaded search.

Legacy search and evaluation:
- `choose-move` calls `alpha-beta` at fixed depth 3.
- The search copies the board for every child and alternates white maximisation with black minimisation.
- Evaluation is white-relative and sums material plus piece-square weights for pawns, rooks, bishops, and knights.
- King material is valued at 1000 in the legacy evaluator, unlike the 2026 evaluator’s mate-score-separated search model.

Legacy release history from the archive:
- `0.9` on 2009-02-21: first published release.
- `0.91` on 2009-02-25: source encoding corrected.
- `0.92` on 2009-02-26: move notation changed from numeric coordinates to square notation.
- `0.95` on 2009-03-07: source code translated into English.

## Compilation and build

### Build requirements

- Windows x64.
- Steel Bank Common Lisp (SBCL) 2.6.6 x86-64. `build_uci.bat` looks for `compiler\sbcl\sbcl.exe` first and then `C:\Lisp\sbcl.exe`.
- Python 3 for packaging and checksum writing.

### Build command

Run from the project root:

```bat
build_uci.bat
```

The build script runs SBCL with these release settings:

```bat
sbcl.exe --lose-on-corruption ^
  --dynamic-space-size 4096 ^
  --control-stack-size 12 ^
  --noinform --disable-debugger --non-interactive ^
  --load build\build-windows-optimized.lisp
```

A release build writes these paths:

- `compilation/miguedrez-0.95.11.5-win64.exe`
- `compilation/miguedrez-0.95.11.5-win64.exe.sha256`
- `compilation/build.log`
- `distribution/miguedrez-0.95.11.5-win64.zip`
- `distribution/miguedrez-0.95.11.5-win64.zip.sha256`

The ZIP is flat at archive root and contains:

- `miguedrez-0.95.11.5-win64.exe`
- `README.md`
- `COPYING`
- `src/` with the Common Lisp source tree

## UCI protocol and options

Hash Option (spin, default 16, min 1, max 4096, megabytes)
- `setoption name Hash value <mb>` with range `1..4096`
- Resizes the transposition table via setoption name Hash value <MB>. Out-of-range values are clamped; malformed values are ignored and the current size is kept. 
- Safe to change between searches since the engine is single-threaded. 

Command set:
- No Threads, Ponder, or Contempt options exist.
- `uci`
- `isready`
- `ucinewgame`
- `position startpos [moves ...]`
- `position fen <fen> [moves ...]`
- `go depth <n>`
- `go movetime <ms>`
- `go wtime <ms> btime <ms> [winc <ms>] [binc <ms>] [movestogo <n>]`
- `go infinite`
- `go searchmoves <move...>`
- `stop`
- `quit`
Search output includes iterative `info` lines and a final `bestmove` line.

