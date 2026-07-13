# MOTP ↔ mistyR Bridge — To-Do Ledger

> Companion to [`motp-concordance.md`](motp-concordance.md). Every insight from the
> 2026-07-10 audit/review/cross-reference that implies future work, in one place.
> Check items off with the commit/date that resolved them. Items carry enough context
> to act on cold.

Safety tag for this work series: **`pre-accidental-refactor`** (mistyR repo).
Each pass is its own commit for selective revert.

---

## A. mistyR — code side

### First pass (this series)

- [x] **A1. Rename incidental → accidental** (done 2026-07-10, mistyR `69d1379`) across live code (`R/`). Functions
  (`.addIncidental`, `.detectIncidental`, `.dropIncidental`, `.incidentalSemitones`,
  `.incToSymbol`), infix `%INC%` → `%ACC%`, globals (`INC_HASH`, `.INC_HASH_EXT`),
  locals/comments, and `git mv R/incidental.R R/accidental.R`. Leave `misty-stub/`,
  `archive/`, `dev/archive/` frozen (history). Update concordance fossil #1 to
  past tense.
- [x] **A2. Fix `SCALE_DEGREES` missing `b6`** (done 2026-07-10, mistyR `7ce17f0`) (`data.R`): the list jumps `6` → `b7`,
  so any scale containing degree `b6` (natural minor!) silently mis-spells it —
  `NULL` propagates through `.intervalNote()` to a zero-length accidental and the
  white letter is returned unchanged. Symptom: `minor_scale("D")` yields `B`
  instead of `B-`. Fix: add `b6 = 8`.
- [x] **A3. Fix Phrygian construction** (done 2026-07-10, mistyR `7ce17f0`) (`data.R`): built from *dorian* with only
  degree 1 flattened, leaving a natural 6 — Phrygian is `1 b2 b3 4 5 b6 b7`.
  Build from `minor` instead.
- [x] **A4. Restore `OTHER_SCALES`** (done 2026-07-10, mistyR `7ce17f0`) (`data.R`): commented out, but `blues_scale()`
  (`scale.R`) still references it and errors if called. Uncomment (degrees all
  exist in `SCALE_DEGREES` once A2 lands).
- [x] **A5. Fix stale `.srcp` preset2** (done 2026-07-10, mistyR `69d1379`) (`header.R`): lists `misty.R` and
  `enharmonic.R`, which no longer exist; also references `incidental.R` (renamed
  by A1). Point at the real core files.

### Deferred

- [ ] **A6. `.complexCase()` bug** (`harmony.R`): comment admits *"doesn't quite do
  the trick; misses white keys..."*. The book's Preservation Principle (Thm 2.5.1,
  degree-aware d̃) is the spec for the clean fix: reduce via same-accidental
  transposition instead of ad-hoc enharmonic cycling.
- [ ] **A7. `%ST+%` / `%ST-%` are partial** (`interval.R`): silently return `NULL`
  for semitone arguments outside {1,2}. Either make total (loop `%ACC%`) or
  assert.
- [ ] **A8. Den-key helpers**: the book's den keys {A,D,G} / white neighbours
  {B,C,E,F} have no code counterpart (`HAS_FL & HAS_SH` never intersected).
  Cheap to add; would let code comments cite the book's definitions.
- [ ] **A9. Quality dyad/triad constructors** (△/∇ algebra, book §2.3): mistyR has
  *no chord builder* — the biggest book-only concept. A `dyad(L, quality)` /
  `triad(L, quality)` layer would close the loop and make §2.3 retroactively true.
- [ ] **A10. `dev/manual.Rmd`** documents the pre-rename API; update text + re-knit
  `manual.pdf` after A1 settles (or stamp it as historical).
- [ ] **A11. Double-accidental regex fragility**: `.INC_HASH_EXT` comment notes `##`
  / `bb` were dropped from symbols because of regex confounding (hence `&`/`x`).
  Consider exact-count matching so `bb`/`##` input spellings parse.
- [ ] **A12. Inconsistent flat symbol on output** (found during A1 smoke tests):
  `enharmonic("F#")` returns `"Gb"` (letter b, via `.addAccidental`'s `"b"` branch)
  while `.intervalNote()`/`scale()` emit `"G-"` (kern `-`, via
  `.accidentalSemitones`' symbol vector) — and `POSSIBLE_NOTES` only recognizes
  `-`. Both parse back fine, but pick one output convention (kern `-` suggested)
  so round-trips through `collapseNotes()` don't drop enharmonic() output.

## B. MOTP — book side (`music-of-the-primes` repo)

### First pass (this series)

- [x] **B1. "Phyrgian" → "Phrygian"** (done 2026-07-10, book `400c574`) (`preamble/commands.tex`, `\Phg` macro) —
  renders misspelled in Ch. 3 via `\Phg` at the mode-rotation display.
- [x] **B2. `\AEOLIAN` = "whwwwhww"** (done 2026-07-10, book `400c574`) (`preamble/scales.tex`, not modes.tex as first logged) — wrong pattern
  (8 intervals, sums to 14 semitones). Aeolian is `whwwhww`. Currently only
  referenced in a comment, but it's a landmine.
- [x] **B3. "the tile of fifths" → "the tiling of fifths"** (done 2026-07-10, book `400c574`) (`chapters/chapter2.tex`,
  Circle of Fifths subsection).
- [x] **B4. "the field ℤ₇" → "the group ℤ₇"** (done 2026-07-10, book `400c574`) (`chapters/chapter2.tex`, §2.1 note) —
  only the additive group structure is ever used.
- [x] **B5. `F^{{\bf{x}}}` → `F\dsh`** (done 2026-07-10, book `400c574`) (`chapters/chapter2.tex`, cursed-shell-spellings
  display) — use the existing `\dsharp` macro instead of the fossilized ASCII bold x
  (see concordance, Vocabulary Fossils #3).

### Deferred (design decisions — need Ali's call)

- [ ] **B6. Symbol overloads** (the big one):
  `⊕` = letter addition *and* dyad stacking; `κ` = dyad operator (`κ₂Δ[L^α]`) *and*
  chord variable *and* frequency scalar (macro comment says "Freq. Scalar (?)");
  `𝕊` = letter-stack operator *and* neutral Sixth variable (Sixth is live in ch3
  mode tables). Pick distinct symbols; update `preamble/commands.tex` comments.
- [ ] **B7. Distance-function naming**: Def. *Accidentals* promises "d₁, defined
  below" but the rule defines d_α; d₁ reappears in the Circle of Fifths derivation;
  d_𝒯 is used once (Furry Cat Thm) without in-chapter definition; d₃ typed unary,
  used binary; d₅ typed on ℒ×ℒ. Unify the family.
- [ ] **B8. One dyad, three spellings**: `κ₂Δ[A]` = `Δ₂[A]` = `Δ[A]` within a page
  (§2.3). Pick one; same for `𝔻𝕖𝕟` vs `Blk_D` and `Blk[♯]` vs `Blk_♯`.
- [ ] **B9. Dangling 𝔸 reference**: "we could have just used 𝔸 = (♮,♭)" points at
  the commented-out Accidental Vector definition (Tritones page). Restore the
  definition or cut the sentence.
- [ ] **B10. Block-header arithmetic**: "Steps: 1+1=2" and "Thirds: 2+2=3" can't
  both hold in one convention (semitones: 2+2=4; degrees: 1+1≠2). Pick one
  arithmetic and rename.
- [ ] **B11. Proofs**: Furry Cat Theorem and Preservation Principle are asserted;
  both admit two-line proofs (chain-of-fifths maximality via the single B–F gap;
  transposition invariance of step counts).
- [ ] **B12. Ch. 2 tail (pp. 43–47)**: near-empty Tritones page + header-only table
  runs. Promote to a chapter-end "white-key tables" reference appendix, or add
  connective prose. Re-enabling `\TABLEintervalsfifths` (verified correct) with a
  paragraph would anchor it.
- [ ] **B13. Notation ledger**: the chapter introduces ~15 notational devices, some
  used once (the `∫` stack). Add a notation index/ledger to front- or back-matter.
- [ ] **B14. Double accidentals**: 𝔸 = {♭,♮,♯} truncates to singles while 𝄫/𝄪 appear
  in the accidental-family tables. Add a remark extending the Semitonal-Accidental
  Norm to ±2 (the code already does this — `.accidentalSemitones`).
- [ ] **B15. Rule of 9 demo tables** (`\TABLErulenine*`) are written and commented
  out — decide in or out.
- [ ] **B16. Renewed LaTeX built-ins** (`\and`, `\L`, `\P`, `\S`, `\d`, `\k`, `\a`,
  `\th`, …): fine for a solo manuscript; alias layer needed before any publisher
  class file. `\and` specifically breaks multi-author `\author`.

## D. Module development (kern & midi) — active track

Ali's directive (2026-07-12): keep and develop the kern and midi modules;
everything archive-flavored deleted from working trees (git history retains).

- [x] **D1. Kern pipeline repaired & verified** (2026-07-12, `336ab8e`): fixed
  stringr rot in `.k2df_resolve_chords`, the `[,1]`→`[,i]` voice bug in
  `scale_degree_freq`, phantom `top_rhythm_freq`, and a crash guard in
  `rhythm_entropy`. Full run green on `data/bach-prelude-C.krn`.
- [x] **D2. `midi2df()` parity importer** (2026-07-12, `abb9894`): kern2df
  column contract from .mid files; whole analysis suite runs on MIDI
  (verified: clairedelune.mid incl. Db→E→Db key windows, blame.mid).
- [ ] **D3. Rests/continuations**: kern frames carry `rest`/`.` tokens; MIDI
  silence is just absence. Consider synthesizing rest rows from inter-attack
  gaps so density/rhythm features see silence equally in both formats.
- [ ] **D4. Multi-instrument `piece_df` for MIDI**: kern has
  `piece_df(files, instruments)`; MIDI tracks (`.getTrackNames`) could yield
  the same instrument-prefixed columns from a single file.
- [ ] **D5. Triplets & quantization tolerance**: `.m2df_rhythm` snaps to the
  duple lattice only; add triplet tokens (and expose the snap tolerance)
  before feeding performed (unquantized) MIDI.
- [ ] **D6. Tests**: a small testthat suite pinning both importers' column
  contract and the analysis outputs on the bundled data files.
- [ ] **D7. MIDI-only features**: velocity/dynamics and tempo-map stylometry —
  dimensions kern cannot see; natural extension of the modality feature set.
- [ ] **D8. Corpus run**: `data/composer/` (6×~15 kern pieces) through the
  repaired pipeline to regenerate/validate `analysis/` CSVs; then a mixed
  kern+MIDI corpus becomes possible via the shared frame.

## C. Bridge documents

- [ ] **C1. Chapter-by-chapter concordance index** once Ch. 1/3 get the same
  treatment (Ch. 1 ↔ tuning tables; Ch. 3 ↔ `DIATONIC_SCALES`, mode tables,
  `analysis.R` stylometry).
- [ ] **C2. `misty-stub` lineage note**: one page on mistyPy / museR (vendored
  upstream) / museR-reconstructed and what each contributed. Update (2026-07-10,
  later): the book repo's third mistyR copy (`music-prog/MST-R/`) is now tracked
  at `~archive/music-prog/` there (book commit `f9e7790`), verified byte-identical
  before the move; its `tentative/scale.R` held no unique logic — only a
  `quality = "minor"` default for `blues_scale()`, ported to live code here.
  `music-python/` (graphics.py, music.py) also lives in that archive; fold both
  into the lineage note when written.
- [ ] **C3. Keep concordance truthful after renames** (A1 makes fossil #1
  historical; book fixes B1–B5 retire matching "Known Issues" entries).
- [x] **C4. Expository bridge PDF** (done 2026-07-12): `docs/bridge-paper/`
  — 17-page typeset paper ("From Code to Theorem") explaining every
  concordance entry with derivations (`.WKsemitones` ≡ dₙ), worked examples,
  the Thm-2.5.1-is-`scale()` argument, fossils, and the stylometry call-chain
  diagram. Source is self-contained LaTeX (does NOT `\input` the book's
  preamble — see B16); recompile with two `pdflatex` passes after renames.

---

*Ledger created 2026-07-10 during the first bridge session. House rule for this
series: safety tag, then one commit per pass, pushed immediately.*
