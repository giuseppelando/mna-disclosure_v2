# Module 3 - Index Construction Report (v5)

Generated: 2026-02-15 15:52:20.493307
Observations: 4186

## Forward-looking intensity construct
- Definition: forward-looking language = modal-based detection + regex-pattern detection
- Modal tokens (configured): anticipate, anticipated, anticipates, forecast, forecasted, forecasting, forecasts, expect, expects, expected, project, projects, projected, projecting, plan, plans, planned, planning, intend, intends, intended, intending, aim, aims, aimed, aiming, target, targets, targeted, targeting, outlook, guidance, will, would, shall, future, forthcoming, upcoming, next
- Modal tokens active in DFM: anticipate, anticipated, anticipates, forecast, forecasted, forecasting, forecasts, expect, expects, project, projects, projected, projecting, plan, plans, planned, planning, intend, intends, intended, intending, aim, aims, aimed, aiming, target, targets, targeted, targeting, outlook, guidance, will, would, shall, forthcoming, upcoming, next
- Regex patterns source: config
- Active regex patterns: 12
- Total modal matches (corpus): 281282
- Total regex matches (corpus): 117596
- Contribution shares: modal=0.705 | regex=0.295

## Risk dictionary transparency
- Source label: LM(Uncertainty + Negative)
- Preprocessing: lowercase -> trim -> drop NA/empty -> keep single-token -> unique
- Raw sizes: Uncertainty=297 | Negative=2345 | Combined=2642
- Final usable dictionary size: 2602
- Matched in DFM (unigram): 1996/2602
- Unmatched (not in DFM feature set): 606/2602
- Examples matched: abandon, abandoned, abandoning, abandonment, abandonments, aberrations, abetting, abeyance, abnormal, abnormalities
- Examples unmatched: abandons, abdicated, abdicates, abdicating, abdication, abdications, aberrant, aberration, aberrational, abeyances

## Normalisation audit (hierarchical fallback)
- min_cell_size: 10
- Levels: sic2×year -> sic2 -> year -> global

