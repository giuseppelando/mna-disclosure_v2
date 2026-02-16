# Module 09 — Econometric Models Report (v6)

Generated: 2026-02-16 20:41:35.533813

## Variable mapping (v6)
| Role | Variable | Description |
|------|----------|-------------|
| Core NLP | operational_specificity_raw_z | Numeric + compound density (standardized) |
| Core NLP | fl_sentence_share_z | FL sentences / total sentences (standardized) |
| Core NLP | risk_transparency_z | 1 - cosine(doc, peer centroid) (standardized) |
| Core NLP | tone_lm_z | (Pos-Neg)/(Pos+Neg+1) (standardized) |
| Supplementary | fl_precision_share_z | FL sentences with numbers / FL sentences |
| Supplementary | commitment_density_z | Strong modals per 1000 words |
| Supplementary | hedging_density_z | Weak modals per 1000 words |
| Processing cost | mda_fog_z | Gunning Fog (MD&A) |
| Processing cost | mda_log_length_z | log(word count) (MD&A) |
| Outcome | premium_1d_w | Premium 1-day prior (winsorized) |
| Outcome | completion | 1=Completed, 0=Withdrawn |
| Outcome | time_to_close_w | Days to close (winsorized) |
| FE | fe_year | Year FE |
| FE | fe_ind_year | Industry × Year FE |
| Clustering | sic2 | 2-digit SIC |

## Model specifications
### Premium (OLS via feols, clustered at sic2)
- P1: Core 4 + controls + Year FE
- P2: Core 4 + controls + Ind×Year FE
- P3: Core 4 + supplementary + controls + Year FE
- P4: Core 4 + processing costs + controls + Year FE
- P5_*: Individual indices (one-at-a-time)

### Completion (Logit via feglm, clustered at sic2)
- C1: Core 4 + controls + Year FE
- C2: Core 4 + controls + Ind×Year FE
- C3: Extended + controls + Year FE
- C4: Core 4 + processing costs + controls + Year FE

### Duration
**Primary: AFT log-normal** (does not require PH assumption)
- AFT1: Core 4 + controls (log-normal)
- AFT2: Extended + controls (log-normal)
- AFT3: Core 4 + processing costs (log-normal)
- AFT4: Core 4 + controls (Weibull, distributional robustness)
  - Coefficients: exp(β) = time ratio (>1 means longer duration)
  - Interpretation: '1 SD increase → X% change in days-to-close'

**Robustness: Cox PH with proper censoring**
- Cox1: Core 4 + strata(year), withdrawn deals censored at withdrawal date
  - Addresses PH violation from prior specification (no censoring, all events=1)
  - Per Research Design: 'withdrawn deals treated as censored observations'

## Notes
- All NLP indices standardized (_z) for coefficient comparability
- Raw indices used + FE absorbs industry×year means (avoids double de-meaning)
- Winsorization at 1%/99% for continuous outcomes and deal value
- Premium bounded [-100, 300] with sample flag
