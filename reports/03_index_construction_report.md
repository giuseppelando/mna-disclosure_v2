# Module 3 - Index Construction Report (v5)

Generated: 2026-02-09 22:58:23.818716
Observations: 2391

## Forward-looking intensity construct
- Definition: forward-looking language = modal-based detection + regex-pattern detection
- Modal tokens (configured): anticipate, anticipated, anticipates, forecast, forecasted, forecasting, forecasts, expect, expects, expected, project, projects, projected, projecting, plan, plans, planned, planning, intend, intends, intended, intending, aim, aims, aimed, aiming, target, targets, targeted, targeting, outlook, guidance, will, would, shall, future, forthcoming, upcoming, next
- Modal tokens active in DFM: anticipate, anticipated, anticipates, forecast, forecasted, forecasting, forecasts, expect, expects, project, projects, projected, projecting, plan, plans, planned, planning, intend, intends, intended, intending, aim, aims, aimed, aiming, target, targets, targeted, targeting, outlook, guidance, will, would, shall, forthcoming, upcoming, next
- Regex patterns source: config
- Active regex patterns: 12
- Total modal matches (corpus): 157786
- Total regex matches (corpus): 64677
- Contribution shares: modal=0.709 | regex=0.291

## Risk dictionary transparency
- Source label: LM(Uncertainty + Negative)
- Preprocessing: lowercase -> trim -> drop NA/empty -> keep single-token -> unique
- Raw sizes: Uncertainty=297 | Negative=2345 | Combined=2642
- Final usable dictionary size: 2602
- Matched in DFM (unigram): 1882/2602
- Unmatched (not in DFM feature set): 720/2602
- Examples matched: abandon, abandoned, abandoning, abandonment, abandonments, abetting, abeyance, abnormal, abnormalities, abnormality
- Examples unmatched: abandons, abdicated, abdicates, abdicating, abdication, abdications, aberrant, aberration, aberrational, aberrations

## Normalisation audit (hierarchical fallback)
- min_cell_size: 10
- Levels: sic2×year -> sic2 -> year -> global

