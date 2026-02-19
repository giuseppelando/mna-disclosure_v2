# Module 3 — Index Construction Report (v6)

Generated: 2026-02-19 02:09:01.585364
Observations: 4186

## v6 Changes (feedback-driven)

### High-impact fixes applied:
1. **Coherent denominators**: dictionary densities use wc_alpha; numeric densities use wc_total
2. **No max_docfreq trimming** on dictionary DFMs (handled in Module 2 v4)
3. **Multiword rejection**: preprocess_dict() errors on multiword entries
4. **Tone formula**: (Pos-Neg)/(Pos+Neg+1), always defined, no endogenous NA
5. **Modal separation**: strong (commitment) vs weak (hedging) tracked separately

### New constructs:
A. **Forward-looking (sentence-based)**: share of MD&A sentences with prospective markers
B. **Risk transparency (cosine similarity)**: 1 - cosine_sim(doc, industry-year centroid)
C. **Corrected tone + separate Pos/Neg densities**

## Forward-looking intensity (v6)
- Sentence markers: 16 patterns
- FL sentence share: mean=0.0841 | sd=0.0409
- FL precision (FL sents with numbers): mean=0.4958
- Commitment density (strong modals): mean=2.1332
- Hedging density (weak modals): mean=3.9222

## Risk transparency (v6)
- Cosine sim to industry-year centroid: mean=0.4381
- Risk transparency (1-sim): mean=0.5619 | sd=0.1911
- Legacy LM risk density (per 1000 alpha): mean=69.1414

## Tone (v6 — corrected)
- Formula: (Pos-Neg)/(Pos+Neg+1)
- ToneLM: mean=-0.3981 | sd=0.2164 | NAs=0

## Normalisation
- min_cell_size: 10
- Levels: sic2×year → sic2 → year → global
- NOTE: normalised indices are for descriptive use. Regressions should
  use raw indices + FE (avoids double de-meaning efficiency loss).

