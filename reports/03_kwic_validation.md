# Module 4 — KWIC validation

Generated: 2026-02-07 13:49:12

## Inputs
- tokens: `C:/Users/giuse/Documents/GitHub/mna-disclosure/data/interim/tokens_objects.rds`
- cleaned text: `C:/Users/giuse/Documents/GitHub/mna-disclosure/data/interim/cleaned_text.rds`
- indices: `C:/Users/giuse/Documents/GitHub/mna-disclosure/data/interim/disclosure_indices.rds`

## Index columns used for tails
- operational: `operational_specificity_raw_norm`
- forward-looking: `forward_looking_density_norm`
- risk disclosure: `risk_disclosure_tfidf_norm`
- tone: `tone_lm_norm`

## Parameters
- N_TAIL_DOCS: 10
- WINDOW: 10
- N_MAX_ROWS per (construct×pattern×group): 50

## KWIC patterns
- operational: capital_expenditure, gross_margin, operating_cash_flow
- forward: expect*, anticipat*, forecast*
- risk: risk, risks, uncertain*, adverse*
- tone: improv*, strong*, declin*

## Extraction summary
- total KWIC rows exported: 720

### Rows by construct / pattern / group

| construct | pattern | group | n_rows |
|---|---|---:|---:|
| forward | expect* | bottom | 50 |
| forward | expect* | top | 50 |
| forward | anticipat* | bottom | 17 |
| forward | anticipat* | top | 42 |
| forward | forecast* | top | 10 |
| operational | capital_expenditure | bottom |  3 |
| operational | capital_expenditure | top |  2 |
| operational | gross_margin | bottom | 50 |
| operational | gross_margin | top | 32 |
| operational | operating_cash_flow | bottom |  4 |
| risk | risk | bottom | 50 |
| risk | risk | top | 50 |
| risk | risks | bottom | 31 |
| risk | risks | top | 50 |
| risk | uncertain* | bottom | 13 |
| risk | uncertain* | top | 40 |
| risk | adverse* | bottom | 50 |
| risk | adverse* | top | 50 |
| tone | improv* | bottom | 10 |
| tone | improv* | top | 50 |
| tone | strong* | bottom |  1 |
| tone | strong* | top | 21 |
| tone | declin* | bottom | 33 |
| tone | declin* | top | 11 |
