# Module 4 - KWIC Validation Report (v2)

Generated: 2026-02-15 15:53:12

## Inputs
- tokens: `C:/Users/giuse/Documents/GitHub/mna-disclosure/data/interim/tokens_objects.rds`
- indices: `C:/Users/giuse/Documents/GitHub/mna-disclosure/data/interim/disclosure_indices.rds`

## Index columns used for tails
- operational: `operational_specificity_norm`
- forward-looking: `forward_looking_norm`
- risk disclosure: `risk_disclosure_tfidf_norm`
- tone: `tone_lm_norm`

## Parameters
- N_TAIL_DOCS: 10
- WINDOW: 10
- N_MAX_ROWS: 50
- SEED: 2025

## KWIC patterns
- operational: capital_expenditure, gross_margin, operating_cash_flow
- forward: expect*, anticipat*, forecast*
- risk: risk, risks, uncertain*, adverse*
- tone: improv*, strong*, declin*
- negation: not, no, never

## Total KWIC rows exported: 1019

### Rows by construct / pattern / group

| construct | pattern | group | n_rows |
|---|---|---|---|
| forward | expect* | bottom | 50 |
| forward | expect* | top | 50 |
| forward | anticipat* | bottom | 17 |
| forward | anticipat* | top | 45 |
| forward | forecast* | bottom |  4 |
| forward | forecast* | top | 21 |
| negation | not | bottom | 50 |
| negation | not | top | 50 |
| negation | no | bottom | 50 |
| negation | no | top | 50 |
| negation | never | top |  1 |
| operational | capital_expenditure | bottom |  1 |
| operational | capital_expenditure | top |  2 |
| operational | gross_margin | bottom | 26 |
| operational | gross_margin | top | 38 |
| operational | operating_cash_flow | bottom |  2 |
| risk | risk | bottom | 50 |
| risk | risk | top | 50 |
| risk | risks | bottom | 50 |
| risk | risks | top | 50 |
| risk | uncertain* | bottom | 50 |
| risk | uncertain* | top | 36 |
| risk | adverse* | bottom | 50 |
| risk | adverse* | top | 50 |
| tone | improv* | bottom | 10 |
| tone | improv* | top | 50 |
| tone | strong* | bottom |  4 |
| tone | strong* | top | 33 |
| tone | declin* | bottom | 47 |
| tone | declin* | top | 32 |

## Revision Notes (v2)
- Fixed dplyr `all_of()` rename issue
- Report numbered as 04 (was incorrectly 03)
- Added negation-context KWIC sheet for tone validation
- Reproducible seed for KWIC sampling

