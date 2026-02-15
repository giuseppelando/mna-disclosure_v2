# Module 6 — Final Dataset Report (v6 REVISED)

Generated: 2026-02-15 16:29:25.409212

## v6 Changes
- **Premium**: `premium` (=premium_1d) is PRIMARY outcome; `premium_4w` for robustness
- **time_to_close**: Now uses SDC's `number_of_days_between_*` (fixes negative values)
- **Column cleanup**: Removed 17 redundant columns (flags, duplicates)

## Dataset dimensions
- Rows: 4186
- Columns (RDS): 83
- Columns (CSV, no text): 81

## Sample sizes
- sample_core (all 3 strict indices non-NA): 4186
- sample_tone (core + tone non-NA): 4175
- sample_premium (core + premium + valid price): 3102
- sample_completion (core + terminal outcome): 4186
- sample_duration (completed + time_to_close > 0): 3181

## Outcome variables
- premium (1d): n=3102 | mean=38.26 | median=24.73 | sd=152.38
- premium_4w: n=3102 | mean=41.99 | median=28.25 | sd=128.03
- completion: Completed=3371 | Withdrawn=815 | NA=0
- time_to_close (completed, >0): n=3181 | mean=173.3 days | median=108.0 days

## Normalisation fallback shares
- operational_specificity_norm_level: sic2 = 2171 | sic2_year = 1849 | year = 166
- forward_looking_norm_level: sic2 = 2171 | sic2_year = 1849 | year = 166
- risk_disclosure_tfidf_norm_level: sic2 = 2171 | sic2_year = 1849 | year = 166
- tone_lm_norm_level: missing = 11 | sic2 = 2167 | sic2_year = 1842 | year = 166

## Outlier counts (|z| > 5 SD)
- operational_specificity_norm_outlier: 1
- forward_looking_norm_outlier: 0
- risk_disclosure_tfidf_norm_outlier: 0
- tone_lm_norm_outlier: 0

## Index missingness (core normalised)
# A tibble: 4 × 4
  variable                         n  n_na na_share
  <chr>                        <int> <int>    <dbl>
1 operational_specificity_norm  4186     0  0      
2 forward_looking_norm          4186     0  0      
3 risk_disclosure_tfidf_norm    4186     0  0      
4 tone_lm_norm                  4186    11  0.00263

## Columns removed in v6
- logical_deal_id, premium_paid_1_day_prior_to_announcement, premium_paid_1_week_prior_to_announcement, premium_paid_4_weeks_prior_to_announcement, flag_has_deal_id, flag_has_target_name, flag_has_announce_date, flag_terminal_outcome, flag_has_premium_inputs, has_premium, has_premium_1d, has_premium_4w, has_completion_outcome, has_ticker, has_cusip, deal_completed, deal_outcome_terminal

## Notes
- Raw text columns (mda_text, risk_factors_text) excluded from CSV but retained in RDS.
- Outlier flags are informational; no observations dropped.
- tone_lm_norm has small missingness due to sparse-denominator rule (pos+neg < 10).
- Premium uses 1-day prior price (standard in M&A literature); 4-week for robustness.
