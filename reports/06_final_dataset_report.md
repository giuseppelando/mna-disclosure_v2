# Module 6 — Final Dataset Report (v5)

Generated: 2026-02-09 22:59:13.778824

## Dataset dimensions
- Rows: 2391
- Columns (RDS): 95
- Columns (CSV, no text): 93

## Sample sizes
- sample_core (all 3 strict indices non-NA): 2391
- sample_tone (core + tone non-NA): 2384
- sample_premium (core + premium_1d + valid price): 2374
- sample_completion (core + terminal outcome): 2391
- sample_duration (completed + time_to_close > 0): 2388

## Outcome variables
- premium_1d: n=2374 | mean=119.92 | median=28.60 | sd=3833.73
- completion: Completed=2391 | Withdrawn=0 | NA=0
- time_to_close (completed only): n=2391 | mean=128.3 | median=104.0

## Normalisation fallback shares
- operational_specificity_norm_level: sic2 = 1456 | sic2_year = 711 | year = 224
- forward_looking_norm_level: sic2 = 1456 | sic2_year = 711 | year = 224
- risk_disclosure_tfidf_norm_level: sic2 = 1456 | sic2_year = 711 | year = 224
- tone_lm_norm_level: missing = 7 | sic2 = 1452 | sic2_year = 708 | year = 224

## Outlier counts (|z| > 5 SD)
- operational_specificity_norm_outlier: 1
- forward_looking_norm_outlier: 0
- risk_disclosure_tfidf_norm_outlier: 0
- tone_lm_norm_outlier: 0

## Index missingness (core normalised)
# A tibble: 4 × 4
  variable                         n  n_na na_share
  <chr>                        <int> <int>    <dbl>
1 operational_specificity_norm  2391     0  0      
2 forward_looking_norm          2391     0  0      
3 risk_disclosure_tfidf_norm    2391     0  0      
4 tone_lm_norm                  2391     7  0.00293

## Notes
- Raw text columns (mda_text, risk_factors_text) excluded from CSV but retained in RDS.
- Outlier flags are informational; no observations dropped.
- tone_lm_norm has small missingness due to sparse-denominator rule (pos+neg < 10).
