# MOTP ↔ mistyR Concordance

> **Bridge document** between *Music of the Primes* (the book) and this repository (the code).
> mistyR (2022) is the executable prototype; MOTP Chapter 2 (2026) is its retroactive
> formalization. This file records the mapping in both directions so that neither artifact
> has to be reverse-engineered from the other again.

| | |
|---|---|
| **Book** | `~/Developer/~Inactive/music-of-the-primes/` — `chapters/chapter2.tex` + `preamble/` |
| **Book scope** | Ch. 2 *Chromatic-Accidental Arithmetic* (pp. 21–47) |
| **Code scope** | `R/` (core library); `kern-module/R/` and `analysis/` where noted |
| **Provenance** | Cross-reference audit, 2026-07-10 |
| **Anchoring** | Concepts are keyed to **function names** and **definition/theorem titles**, not line numbers, so the mapping survives edits on both sides. |

---

## 1. Notation Translation Key

How to read the book if you know the code (and vice versa).

| Book notation | Meaning | Code construct |
|---|---|---|
| ℒ = {A,…,G} | Musical alphabet | `MUS_ALPH <- LETTERS[1:7]` (`data.R`) |
| ℤ₇ arithmetic, ⊕ (looping) | Letter arithmetic mod 7 | `mod7()`, `.ALPH_RANGE()`, `.IDX_STATIC()` (`data.R`) |
| Λₖ(L) = L ⊕ (k−1) | k-degree letter operator | `%STEP+%`, `%STEPUP%` (`scale.R`); `tonic %STEP+% (degree_n − 1)` in `.intervalNote()` |
| 𝕃ₖ[L] | k-degree letter sequence | `.ALPH_RANGE(note_index(L), k)` |
| S₇[L] | Heptatonic scale template | `.baseScale()` (`harmony.R`) |
| π = (L, α) = L^α | Note-letter (pitch label) | Note string, e.g. `"F#"` — letter via `.dropIncidental()`, accidental via `.detectIncidental()` |
| α ∈ 𝔸 = {♭, ♮, ♯} | Accidental | "**incidental**" throughout the code; `INC_HASH` (`data.R`) |
| π ⊙ α | Apply accidental to pitch | `%INC%` / `.addIncidental()` (`incidental.R`) |
| (L♯)♭ = L (neutralization) | Accidental neutralization | sharp+flat → `""` branch of `.addIncidental()` |
| \|α\| ∈ {−1, 0, +1} (Distance Rule: Semitonal-Accidental Norm) | Accidental semitone value | `.incidentalSemitones()` — code extends to doubles: `(&, -, "", #, x) ↔ (−2, −1, 0, +1, +2)` |
| Blk[♭] = {A,B,D,E,G} | Black flat-supported letters | `ALPH_HASH()$HAS_FL`; global `HAS_FLATS` (`data.R`) |
| Blk[♯] = {A,C,D,F,G} | Black sharp-supported letters | `ALPH_HASH()$HAS_SH`; global `HAS_SHARPS` (`data.R`) |
| Color(L^α) = Blk/Wht | Key color predicate | `.isBlackNote()` / `.isWhiteNote()` (`incidental.R`) |
| Proper / improper enharmonics (Def. Chromatic Enharmonic) | Enharmonic pairs | `.cycle_enharmonic()` (proper), `.simpler_enharmonic()` (improper / white-key); the word *improper* is shared vocabulary |
| dₙ(L) = Σ d₂(Λⱼ) (Distance Rules: Step / n-Degree) | White-key semitone distance | `.WKsemitones()` (`interval.R`): `semitones <- FLATS + WHITES` — one semitone per step plus one extra per flat-supported letter in range |
| "Chromatic Algorithm" (borrowing) | Spell any interval: white-key skeleton + accidental correction | `.intervalNote()` (`interval.R`): `SCALE_DEGREES[[degree]] − .WKsemitones(...)` → `.incidentalSemitones()` |
| **Thm. Accidental-Transpositional Preservation Principle** | Same accidental on both notes preserves distance | `scale()` (`scale.R`): build white-key base scale, then `map_chr(base_scale, .addIncidental, incidental)` — the theorem is this function's correctness argument |
| White-key sufficiency reduction (§ Chromatic Pitch Neighbours) | Reduce accidentalized problems to white keys | `scaleDegree()` (`harmony.R`): sharpens flat tonics / flattens sharp tonics, then delegates to `.scaleDegreeWK()` |
| Furry-cat chain F–C–G–D–A–E–B; tiled circle Φ | Chain of fifths; key signatures | `.KEYS_TABLE()` (`data.R`): sharps accrue as `f# c# g# d# a# e# b#` (the chain, sharped); flats as `b- e- a- d- g- c- f-` (BEADGCF = circle of fourths ℛ) |
| qₖ interval table; ℤ₁₂ | Semitone space, enharmonic collapse | `SCALE_DEGREES` list; `KRN_NOTE_HASH()` note values (enharmonics share values) (`data.R`) |
| Modes (Ch. 3) | Diatonic modes | `DIATONIC_SCALES` list (`data.R`) |

---

## 2. Vocabulary Fossils

Shared language proving the lineage:

1. **"Incidental."** The code's universal term (`INC_HASH`, `incidental.R`, `%INC%`).
   The book adopted the standard *accidental*, but Defs. *Quality Dyad* and *Quality Triad*
   still read "an assignment of an **incidental** pair/triple (α, α_T)" — mistyR vocabulary
   surviving inside the formalization.
2. **"Improper."** `.improperStepDegree()` (code) ↔ "improper enharmonics"
   (book, Def. Chromatic Enharmonic).
3. **The bold x.** The code encodes double-sharp as ASCII `"x"` (chosen because `##`
   confounded regex — see the comment in `.INC_HASH_EXT()`). The book writes
   φ(B♯) = F^**x** with literal `{\bf{x}}` even though `preamble/commands.tex` defines a
   proper `\dsh` macro. The R workaround fossilized into the book's typography.

---

## 3. Asymmetries

### Book-only (formalism beyond the code)

- **Den keys** {A,D,G} and **white neighbours** {B,C,E,F}: the code stores `HAS_FL` and
  `HAS_SH` but never intersects them — the classification is a book-native abstraction.
- **Rule of 9** (interval inversion pairing): the code computes degrees directly and never
  uses inversion.
- **Furry Cat Theorem** as a stated result (the code encodes only its consequence, the
  key-signature table).
- **Quality dyad/triad algebra** (△/∇, complementation |Q^c| = 7 − |Q|, ⊕ stacking, slash
  chords): **no chord constructor exists anywhere in mistyR.** `kern-module/R/kern-harmony.R`
  only extracts chords from scores and diffs them mod 12 (`one_chord_harms()`). Book § *Tertiary
  Harmony* is new theory, not retroactive implementation.
- The **dual (sharp-side) formulation of step distance**: the code only ever uses the
  flat-side criterion (`HAS_FL` in `.WKsemitones()`); the book's equivalent condition on
  Wht[♯]/Blk[♯] is added symmetry.

### Code-only (implementation beyond the book)

- **Double accidentals as first-class** (±2 in `.incidentalSemitones()`; symbols `&`/`x`).
  The book's 𝔸 = {♭,♮,♯} truncates to singles even though 𝄫/𝄪 appear in its own tables —
  a natural candidate for a book remark extending the Semitonal-Accidental Norm.
- **Enharmonic simplification as an operation** (`.simpler_enharmonic()`); the book defines
  the enharmonic pairs statically only.
- **KRN parsing layer** (`KRN_NOTE_HASH`, kern-module) and **modal stylometry**
  (`analysis.R`: blues / interchange / borrowed-subdominant / consonance / dissonance
  features) — outside Ch. 2's scope; prospective material for later chapters.

---

## 4. Convention Divergences

| Axis | mistyR | MOTP |
|---|---|---|
| Letter indexing | 1-indexed (`LETTERS[1:7]`, recurring `mod7(x−1)+1` gymnastics) | 0-indexed ℤ₇ (A = 0) — the cleanup of the code's off-by-one dance |
| Degree operator | `%STEP+% (degree − 1)` | Λₖ = L ⊕ (k−1) — same 1-indexed musical-degree convention, formalized |
| Flat symbol | `-` (kern convention) and `b` both accepted (`INC_HASH`) | ♭ only |
| Double accidentals | `&` (𝄫), `x` (𝄪) | `\dflat` / `\dsharp` macros (but see fossil #3) |
| Step-distance criterion | flat-side only (`HAS_FL`) | dual flat/sharp formulation |

---

## 5. Known Issues Logged During the Audit (2026-07-10)

Code side (this repo):

- `blues_scale()` (`scale.R`) references `OTHER_SCALES`, which is commented out in
  `data.R` — the function errors if called.
- `header.R` `preset2` lists `misty.R` and `enharmonic.R`, which no longer exist in `R/`
  (enharmonic logic lives in `incidental.R`).
- `.complexCase()` (`harmony.R`) carries the comment *"doesn't quite do the trick; misses
  white keys..."* — this is precisely the edge case that the book's Preservation Principle
  (with the degree-aware distance d̃) resolves cleanly. If the code is ever revived, the
  theorem is the spec for the fix.

Book side (logged here for cross-visibility; details in the Ch. 2 review):

- Symbol overloads: ⊕ (letter addition vs. dyad stacking), κ (dyad operator vs. chord
  variable vs. frequency scalar), 𝕊 (letter stack vs. neutral Sixth variable).
- Naming drift in the distance family (d₁ promised, d_α defined; d_𝒯 used once).
- Dangling reference: "𝔸 = (♮,♭)" points at the commented-out Accidental Vector definition.

---

## Maintenance

- Update this file when either side **renames a function, definition, or symbol** listed
  above; the mapping is the artifact, the prose is secondary.
- If MOTP later formalizes the stylometry layer (`analysis.R`) or the KRN parsing layer,
  add new concordance sections rather than growing §1.
- Companion doc candidates (not yet written): a chapter-by-chapter index once more MOTP
  chapters have code counterparts; a `misty-stub` lineage note (mistyPy / museR /
  museR-reconstructed).
