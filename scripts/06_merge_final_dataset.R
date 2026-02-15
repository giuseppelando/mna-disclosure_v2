# ==============================================================================
# Step 6: Merge Final Dataset
# Combines deals with parsed 10-K sections
#
# REQUIREMENTS (enforced by previous steps):
#   - deals_filing_matched.rds: ONE row per logical deal
#   - parsed_sections.rds: ONE row per accession_number
#
# OUTPUT: Final dataset with one row per deal, ready for NLP analysis
# ==============================================================================

library(tidyverse)
library(glue)

# Detect project root
current_dir <- getwd()
if (dir.exists(file.path(current_dir, "config")) && 
    dir.exists(file.path(current_dir, "src"))) {
  PROJECT_ROOT <- current_dir
} else {
  PROJECT_ROOT <- dirname(current_dir)
}

# Load configuration and utilities
source(file.path(PROJECT_ROOT, "config/pipeline_config.R"))
source(file.path(PROJECT_ROOT, "src/99_utils/utils.R"))

# Initialize logging
log_file <- init_logging(PATHS$log_dir, "06_merge_final_dataset")
log_section("STEP 6: MERGE FINAL DATASET", log_file)

# ==============================================================================
# Configuration
# ==============================================================================

# Maximum allowed lag (in days) - set to Inf to disable filtering
MAX_LAG_DAYS <- 1095  # 3 years

if (is.finite(MAX_LAG_DAYS)) {
  log_message(glue("Lag filter enabled: max = {MAX_LAG_DAYS} days ({round(MAX_LAG_DAYS/365, 1)} years)"), log_file)
} else {
  log_message("Lag filter disabled: all lags accepted", log_file)
}

# ==============================================================================
# Load data
# ==============================================================================

log_message("\nLoading data...", log_file)

deals_filing_matched <- readRDS(PATHS$deals_filing_matched)
parsed_sections <- readRDS(PATHS$parsed_sections)

log_message(glue("  → deals_filing_matched: {nrow(deals_filing_matched)} rows"), log_file)
log_message(glue("  → parsed_sections: {nrow(parsed_sections)} rows"), log_file)

# ==============================================================================
# Validate inputs (critical checks)
# ==============================================================================

log_message("\nValidating inputs...", log_file)

# Check 1: deals_filing_matched has one row per deal
n_deals <- nrow(deals_filing_matched)
n_unique_deals <- n_distinct(deals_filing_matched$logical_deal_id)

if (n_deals != n_unique_deals) {
  log_message(glue("  ✗ ERROR: deals_filing_matched has duplicates!"), log_file, "ERROR")
  log_message(glue("    Rows: {n_deals}, Unique deals: {n_unique_deals}"), log_file, "ERROR")
  stop("deals_filing_matched must have exactly one row per deal. Re-run Step 3.")
} else {
  log_message(glue("  ✓ deals_filing_matched: one row per deal ({n_deals} deals)"), log_file)
}

# Check 2: parsed_sections has one row per accession_number
n_parsed <- nrow(parsed_sections)
n_unique_accessions <- n_distinct(parsed_sections$accession_number)

if (n_parsed != n_unique_accessions) {
  log_message(glue("  ✗ ERROR: parsed_sections has duplicates!"), log_file, "ERROR")
  log_message(glue("    Rows: {n_parsed}, Unique accessions: {n_unique_accessions}"), log_file, "ERROR")
  stop("parsed_sections must have exactly one row per accession_number. Re-run Step 5.")
} else {
  log_message(glue("  ✓ parsed_sections: one row per accession ({n_parsed} filings)"), log_file)
}

# ==============================================================================
# Standardize column names (handle days_lag vs days_before)
# ==============================================================================

if ("days_before" %in% names(deals_filing_matched) && !"days_lag" %in% names(deals_filing_matched)) {
  deals_filing_matched <- deals_filing_matched %>%
    rename(days_lag = days_before)
  log_message("  → Renamed 'days_before' to 'days_lag'", log_file)
}

# ==============================================================================
# Apply lag filter (optional)
# ==============================================================================

if (is.finite(MAX_LAG_DAYS)) {
  log_message("\nApplying lag filter...", log_file)
  
  n_before <- nrow(deals_filing_matched)
  
  # Count extreme lags
  extreme_lags <- deals_filing_matched %>%
    filter(match_status == "matched", days_lag > MAX_LAG_DAYS)
  
  n_extreme <- nrow(extreme_lags)
  
  if (n_extreme > 0) {
    log_message(glue("  → Found {n_extreme} deals with lag > {MAX_LAG_DAYS} days"), log_file)
    
    # Show examples
    log_message("  → Examples (top 5 by lag):", log_file)
    examples <- extreme_lags %>%
      arrange(desc(days_lag)) %>%
      select(target_name, date_announced, filing_date, days_lag) %>%
      head(5)
    
    for (i in 1:nrow(examples)) {
      log_message(glue("      {examples$target_name[i]}: {examples$days_lag[i]} days"), log_file)
    }
    
    # Apply filter - keep unmatched deals + matched deals within lag limit
    deals_filing_matched <- deals_filing_matched %>%
      filter(match_status == "no_valid_filing" | days_lag <= MAX_LAG_DAYS)
    
    log_message(glue("  → After filter: {nrow(deals_filing_matched)} deals"), log_file)
    log_message(glue("  → Excluded: {n_before - nrow(deals_filing_matched)} deals"), log_file)
  } else {
    log_message(glue("  ✓ No deals exceed {MAX_LAG_DAYS} days lag"), log_file)
  }
}

# ==============================================================================
# Merge deals with parsed sections
# ==============================================================================

log_message("\nMerging datasets...", log_file)

# Left join: keep all deals, add text where available
deals_final <- deals_filing_matched %>%
  left_join(
    parsed_sections %>% 
      select(accession_number, mda_text, risk_factors_text, 
             mda_word_count, risk_word_count, parse_status),
    by = "accession_number",
    relationship = "many-to-one"  # Multiple deals CAN reference same 10-K
  )

# Verify row count preserved
if (nrow(deals_final) != nrow(deals_filing_matched)) {
  log_message("  ✗ ERROR: Row count changed during merge!", log_file, "ERROR")
  log_message(glue("    Before: {nrow(deals_filing_matched)}, After: {nrow(deals_final)}"), log_file, "ERROR")
  stop("Merge created unexpected rows - check for duplicate accession_numbers")
} else {
  log_message(glue("  ✓ Merge successful: {nrow(deals_final)} rows preserved"), log_file)
}

# Create overall data status
deals_final <- deals_final %>%
  mutate(
    data_status = case_when(
      match_status == "no_valid_filing" ~ "no_filing",
      match_status == "matched" & is.na(parse_status) ~ "not_parsed",
      match_status == "matched" & parse_status == "success" ~ "complete",
      match_status == "matched" & !is.na(parse_status) ~ parse_status,
      TRUE ~ "unknown"
    )
  )

# ==============================================================================
# Data availability statistics
# ==============================================================================

log_message("\nData availability:", log_file)

availability <- deals_final %>%
  count(data_status) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  arrange(desc(n))

for (i in 1:nrow(availability)) {
  log_message(glue("  → {availability$data_status[i]}: {availability$n[i]} ({availability$pct[i]}%)"), log_file)
}

# Complete cases
complete_cases <- deals_final %>% filter(data_status == "complete")
log_message(glue("\nComplete cases (ready for NLP): {nrow(complete_cases)}"), log_file)

# ==============================================================================
# Statistics for complete cases
# ==============================================================================

if (nrow(complete_cases) > 0) {
  log_message("\nLag statistics (complete cases):", log_file)
  lag_stats <- complete_cases %>%
    summarise(
      mean_lag = round(mean(days_lag), 1),
      median_lag = median(days_lag),
      min_lag = min(days_lag),
      max_lag = max(days_lag)
    )
  log_message(glue("  Mean: {lag_stats$mean_lag}, Median: {lag_stats$median_lag}"), log_file)
  log_message(glue("  Range: [{lag_stats$min_lag}, {lag_stats$max_lag}] days"), log_file)
  
  log_message("\nText statistics (complete cases):", log_file)
  text_stats <- complete_cases %>%
    summarise(
      mean_mda = round(mean(mda_word_count)),
      median_mda = median(mda_word_count),
      mean_risk = round(mean(risk_word_count)),
      median_risk = median(risk_word_count),
      total_words = sum(mda_word_count) + sum(risk_word_count)
    )
  log_message(glue("  MD&A: mean={text_stats$mean_mda}, median={text_stats$median_mda}"), log_file)
  log_message(glue("  Risk: mean={text_stats$mean_risk}, median={text_stats$median_risk}"), log_file)
  log_message(glue("  Total corpus: {scales::comma(text_stats$total_words)} words"), log_file)
}

# ==============================================================================
# Validation checks
# ==============================================================================

log_message("\nValidation checks:", log_file)

checks <- list(
  one_row_per_deal = n_distinct(deals_final$logical_deal_id) == nrow(deals_final),
  lag_min_respected = all(complete_cases$days_lag >= TIMING$lag_min),
  lag_max_respected = if (is.finite(MAX_LAG_DAYS)) all(complete_cases$days_lag <= MAX_LAG_DAYS) else TRUE,
  temporal_order = all(complete_cases$filing_date < complete_cases$date_announced),
  text_present = all(!is.na(complete_cases$mda_text) & !is.na(complete_cases$risk_factors_text))
)

all_passed <- TRUE
for (name in names(checks)) {
  status <- if (checks[[name]]) "✓" else "✗"
  log_message(glue("  {status} {name}"), log_file)
  if (!checks[[name]]) all_passed <- FALSE
}

if (!all_passed) {
  log_message("\n  ⚠ Some checks failed!", log_file, "WARNING")
}

# ==============================================================================
# Save outputs
# ==============================================================================

log_message("\nSaving outputs...", log_file)

# Full dataset
ensure_dir(dirname(PATHS$final_dataset), log_file)
safe_save(deals_final, PATHS$final_dataset, log_file)

# Analysis-ready subset
deals_analysis <- deals_final %>%
  filter(data_status == "complete") %>%
  select(
    # Core identifiers
    logical_deal_id,
    target_name,
    date_announced,
    
    # Filing info
    matched_cik,
    accession_number,
    filing_date,
    days_lag,
    
    # Text
    mda_text,
    risk_factors_text,
    mda_word_count,
    risk_word_count,
    
    # All other variables
    everything(),
    
    # Remove redundant (use any_of to handle missing columns)
    -any_of(c("target_cik", "match_status", "parse_status", "data_status", 
              "filing_url", "company_name", "n_eligible_filings"))
  )

analysis_path <- str_replace(PATHS$final_dataset, "\\.rds$", "_analysis.rds")
safe_save(deals_analysis, analysis_path, log_file)

# ==============================================================================
# Summary
# ==============================================================================

log_section("PIPELINE COMPLETE", log_file)

log_message("Final dataset:", log_file)
log_message(glue("  → Total deals: {nrow(deals_final)}"), log_file)
log_message(glue("  → Complete (with text): {nrow(deals_analysis)} ({round(100*nrow(deals_analysis)/nrow(deals_final), 1)}%)"), log_file)
log_message(glue("  → Corpus size: {scales::comma(sum(deals_analysis$mda_word_count) + sum(deals_analysis$risk_word_count))} words"), log_file)

log_message("\nOutput files:", log_file)
log_message(glue("  → Full dataset: {PATHS$final_dataset}"), log_file)
log_message(glue("  → Analysis subset: {analysis_path}"), log_file)

log_message("\nReady for NLP analysis!", log_file)
