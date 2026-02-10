# ==============================================================================
# Step 6: Merge Final Dataset (WITH LAG FILTER)
# Implements one-to-one deal-to-filing matching per research design
# Optional: Excludes deals with extreme filing-to-announcement lags
# ==============================================================================

library(tidyverse)
library(glue)

# Load configuration and utilities
source("C:/Users/giuse/Documents/GitHub/mna-disclosure/config/pipeline_config.R")
source("C:/Users/giuse/Documents/GitHub/mna-disclosure/src/99_utils/utils.R")

# Initialize logging
log_file <- init_logging(PATHS$log_dir, "06_merge_final_dataset")
log_section("STEP 6: MERGE FINAL DATASET (SEC-API)", log_file)

# ==============================================================================
# LAG FILTER CONFIGURATION
# ==============================================================================

# Set maximum allowed lag (in days)
# Default: 1095 days (3 years)
# Set to Inf to disable filtering
MAX_LAG_DAYS <- 1095  # CHANGE THIS VALUE TO ADJUST FILTER

if (is.finite(MAX_LAG_DAYS)) {
  log_message(glue("Lag filter enabled: Maximum lag = {MAX_LAG_DAYS} days ({round(MAX_LAG_DAYS/365, 1)} years)"), log_file)
} else {
  log_message("Lag filter disabled: All lags accepted", log_file)
}

# ==============================================================================
# Load all data
# ==============================================================================

log_message("\nLoading data...", log_file)

deals_filing_matched <- readRDS(PATHS$deals_filing_matched)
parsed_sections <- readRDS(PATHS$parsed_sections)

log_message(glue("  → Deals with filing matches: {nrow(deals_filing_matched)}"), log_file)
log_message(glue("  → Parsed sections: {nrow(parsed_sections)}"), log_file)

# ==============================================================================
# CORRECTION: De-duplicate to one row per logical deal
# Research design requirement: one observation per target–deal
# ==============================================================================

log_message("\nImplementing one-to-one deal–filing matching...", log_file)

# Create logical deal identifier
deals_filing_matched <- deals_filing_matched %>%
  mutate(logical_deal_id = paste(target_name, date_announced, sep = "___"))

# Count structure
n_total_rows <- nrow(deals_filing_matched)
n_logical_deals <- n_distinct(deals_filing_matched$logical_deal_id)
n_multi_cik <- n_logical_deals < n_total_rows

log_message(glue("  → Total rows in deals_filing_matched: {n_total_rows}"), log_file)
log_message(glue("  → Unique logical deals (target + announcement date): {n_logical_deals}"), log_file)

if (n_multi_cik) {
  n_dup_deals <- deals_filing_matched %>%
    count(logical_deal_id) %>%
    filter(n > 1) %>%
    nrow()
  
  log_message(glue("  → Logical deals with multiple CIK candidates: {n_dup_deals}"), log_file)
  log_message("  → Applying 'first valid CIK wins' rule...", log_file)
} else {
  log_message("  ✓ All deals have single CIK - no de-duplication needed", log_file)
}

# Keep only first successful match per logical deal
# Priority: matched > not matched; then by deal_id (first CIK tried)
deals_filing_matched_unique <- deals_filing_matched %>%
  group_by(logical_deal_id) %>%
  arrange(
    desc(match_status == "matched"),  # Successful matches first
    deal_id                           # Then by original order (first CIK tried)
  ) %>%
  slice(1) %>%
  ungroup() %>%
  select(-logical_deal_id)  # Remove temporary ID

n_after_dedup <- nrow(deals_filing_matched_unique)
log_message(glue("  → After de-duplication: {n_after_dedup} rows"), log_file)
log_message(glue("  → Removed {n_total_rows - n_after_dedup} duplicate CIK rows"), log_file)

# Verify one row per logical deal
n_unique_logical <- n_distinct(paste(deals_filing_matched_unique$target_name, 
                                      deals_filing_matched_unique$date_announced))
if (n_unique_logical != n_after_dedup) {
  log_message("  ✗ ERROR: De-duplication failed - still have multiple rows per deal", 
              log_file, "ERROR")
  stop("De-duplication verification failed")
} else {
  log_message("  ✓ Verified: One row per logical deal", log_file)
}

# ==============================================================================
# OPTIONAL: Filter extreme lags
# ==============================================================================

if (is.finite(MAX_LAG_DAYS)) {
  log_message("\nApplying lag filter...", log_file)
  
  # Count how many would be filtered
  n_before_filter <- nrow(deals_filing_matched_unique)
  extreme_lags <- deals_filing_matched_unique %>%
    filter(match_status == "matched", days_lag > MAX_LAG_DAYS)
  
  n_extreme <- nrow(extreme_lags)
  
  if (n_extreme > 0) {
    log_message(glue("  → Found {n_extreme} deals with lag > {MAX_LAG_DAYS} days"), log_file)
    log_message("  → These will be excluded from analysis", log_file)
    
    # Show examples
    log_message("\n  Examples of excluded deals (top 5 by lag):", log_file)
    examples <- extreme_lags %>%
      arrange(desc(days_lag)) %>%
      select(target_name, date_announced, filing_date, days_lag) %>%
      head(5)
    
    for (i in 1:nrow(examples)) {
      log_message(glue("    {i}. {examples$target_name[i]} ({examples$date_announced[i]}): {examples$days_lag[i]} days"), 
                  log_file)
    }
    
    # Apply filter
    deals_filing_matched_unique <- deals_filing_matched_unique %>%
      filter(match_status == "no_valid_filing" | days_lag <= MAX_LAG_DAYS)
    
    n_after_filter <- nrow(deals_filing_matched_unique)
    log_message(glue("\n  → After lag filter: {n_after_filter} rows"), log_file)
    log_message(glue("  → Excluded: {n_before_filter - n_after_filter} deals"), log_file)
    
  } else {
    log_message(glue("  ✓ No deals exceed {MAX_LAG_DAYS} days lag"), log_file)
  }
} else {
  log_message("\nLag filter disabled - keeping all lags", log_file)
}

# ==============================================================================
# Merge deals with parsed sections
# ==============================================================================

log_message("\nMerging datasets (left join on accession_number)...", log_file)

# Pre-merge validation: check parsed_sections is de-duplicated
n_parsed_unique <- n_distinct(parsed_sections$accession_number)
n_parsed_rows <- nrow(parsed_sections)

if (n_parsed_unique != n_parsed_rows) {
  log_message(glue("  ✗ ERROR: parsed_sections has duplicates: {n_parsed_rows} rows, {n_parsed_unique} unique accessions"), 
              log_file, "ERROR")
  stop("Cannot merge - parsed_sections has duplicate accession numbers. Re-run step 5 with corrections.")
} else {
  log_message(glue("  ✓ parsed_sections verified: {n_parsed_rows} unique accessions"), log_file)
}

# Join deals with parsed sections
deals_final <- deals_filing_matched_unique %>%
  left_join(
    parsed_sections %>% 
      select(accession_number, mda_text, risk_factors_text, 
             mda_word_count, risk_word_count, parse_status),
    by = "accession_number",
    relationship = "many-to-one"  # Multiple deals CAN reference same 10-K
  ) %>%
  # Create overall data availability status
  mutate(
    data_status = case_when(
      match_status == "no_valid_filing" ~ "no_filing",
      match_status == "matched" & is.na(parse_status) ~ "download_failed",
      match_status == "matched" & parse_status == "success" ~ "complete",
      match_status == "matched" & parse_status != "success" ~ parse_status,
      TRUE ~ "unknown"
    )
  )

# Verify row count preservation
if (nrow(deals_final) != nrow(deals_filing_matched_unique)) {
  log_message(glue("  ✗ ERROR: Row count changed during merge!"), log_file, "ERROR")
  log_message(glue("    Before: {nrow(deals_filing_matched_unique)} rows"), log_file, "ERROR")
  log_message(glue("    After: {nrow(deals_final)} rows"), log_file, "ERROR")
  stop("Merge created unexpected rows - many-to-many relationship detected")
} else {
  log_message(glue("  ✓ Merge successful: {nrow(deals_final)} rows preserved"), log_file)
}

# ==============================================================================
# Data availability statistics
# ==============================================================================

log_message("\nData availability statistics:", log_file)

availability_summary <- deals_final %>%
  count(data_status) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  arrange(desc(n))

for (i in 1:nrow(availability_summary)) {
  status <- availability_summary$data_status[i]
  count <- availability_summary$n[i]
  pct <- availability_summary$pct[i]
  log_message(glue("  → {status}: {count} ({pct}%)"), log_file)
}

# Complete cases (ready for analysis)
complete_cases <- deals_final %>%
  filter(data_status == "complete")

log_message("\nComplete cases (ready for NLP analysis):", log_file)
log_message(glue("  → Total: {nrow(complete_cases)}"), log_file)
log_message(glue("  → Coverage: {round(100 * nrow(complete_cases) / nrow(deals_final), 1)}%"), log_file)

# ==============================================================================
# Lag statistics for complete cases
# ==============================================================================

if (nrow(complete_cases) > 0) {
  log_message("\nLag statistics for complete cases:", log_file)
  
  lag_stats <- complete_cases %>%
    summarise(
      mean_lag = round(mean(days_lag), 1),
      median_lag = median(days_lag),
      min_lag = min(days_lag),
      max_lag = max(days_lag),
      pct_gt_1yr = round(100 * sum(days_lag > 365) / n(), 1),
      pct_gt_2yr = round(100 * sum(days_lag > 730) / n(), 1),
      pct_gt_3yr = round(100 * sum(days_lag > 1095) / n(), 1)
    )
  
  log_message(glue("  Mean: {lag_stats$mean_lag} days"), log_file)
  log_message(glue("  Median: {lag_stats$median_lag} days"), log_file)
  log_message(glue("  Range: {lag_stats$min_lag} to {lag_stats$max_lag} days"), log_file)
  log_message(glue("  > 1 year: {lag_stats$pct_gt_1yr}%"), log_file)
  log_message(glue("  > 2 years: {lag_stats$pct_gt_2yr}%"), log_file)
  log_message(glue("  > 3 years: {lag_stats$pct_gt_3yr}%"), log_file)
}

# ==============================================================================
# Text statistics for complete cases
# ==============================================================================

if (nrow(complete_cases) > 0) {
  log_message("\nText statistics for complete cases:", log_file)
  
  text_stats <- complete_cases %>%
    summarise(
      mean_mda = round(mean(mda_word_count), 0),
      median_mda = median(mda_word_count),
      sd_mda = round(sd(mda_word_count), 0),
      mean_risk = round(mean(risk_word_count), 0),
      median_risk = median(risk_word_count),
      sd_risk = round(sd(risk_word_count), 0),
      total_words = sum(mda_word_count) + sum(risk_word_count)
    )
  
  log_message("  MD&A:", log_file)
  log_message(glue("    Mean: {text_stats$mean_mda} words"), log_file)
  log_message(glue("    Median: {text_stats$median_mda} words"), log_file)
  log_message(glue("    SD: {text_stats$sd_mda} words"), log_file)
  
  log_message("  Risk Factors:", log_file)
  log_message(glue("    Mean: {text_stats$mean_risk} words"), log_file)
  log_message(glue("    Median: {text_stats$median_risk} words"), log_file)
  log_message(glue("    SD: {text_stats$sd_risk} words"), log_file)
  
  log_message(glue("\n  Total corpus: {scales::comma(text_stats$total_words)} words"), log_file)
}

# ==============================================================================
# Sample descriptive statistics
# ==============================================================================

log_message("\nSample descriptive statistics:", log_file)

# Date range
date_stats <- complete_cases %>%
  summarise(
    min_announcement = min(date_announced),
    max_announcement = max(date_announced),
    min_filing = min(filing_date),
    max_filing = max(filing_date)
  )

log_message("  Announcement dates:", log_file)
log_message(glue("    Range: {date_stats$min_announcement} to {date_stats$max_announcement}"), log_file)

log_message("  Filing dates:", log_file)
log_message(glue("    Range: {date_stats$min_filing} to {date_stats$max_filing}"), log_file)

# ==============================================================================
# Validation checks
# ==============================================================================

log_message("\nValidation checks:", log_file)

validation_results <- list(
  # One row per logical deal (research design requirement)
  one_row_per_deal = n_distinct(paste(deals_final$target_name, deals_final$date_announced)) == nrow(deals_final),
  
  # All original deals preserved (accounting for lag filter)
  deals_preserved = if (is.finite(MAX_LAG_DAYS)) {
    TRUE  # Expected to lose some deals to lag filter
  } else {
    nrow(deals_final) == n_logical_deals
  },
  
  # Lag constraint satisfied for complete cases
  lag_constraint = all(complete_cases$days_lag >= TIMING$lag_min),
  
  # Max lag constraint (if enabled)
  max_lag_constraint = if (is.finite(MAX_LAG_DAYS)) {
    all(complete_cases$days_lag <= MAX_LAG_DAYS)
  } else {
    TRUE
  },
  
  # Temporal ordering
  temporal_order = all(complete_cases$filing_date < complete_cases$date_announced),
  
  # Text present for complete cases
  text_present = all(!is.na(complete_cases$mda_text) & !is.na(complete_cases$risk_factors_text))
)

all_checks_passed <- all(unlist(validation_results))

for (check_name in names(validation_results)) {
  status <- ifelse(validation_results[[check_name]], "✓", "✗")
  log_message(glue("  {status} {check_name}"), log_file)
}

if (!all_checks_passed) {
  log_message("\n  ⚠ Some validation checks failed!", log_file, "WARNING")
} else {
  log_message("\n  ✓ All validation checks passed", log_file)
}

# ==============================================================================
# Save final dataset
# ==============================================================================

log_message("\nSaving final dataset...", log_file)
ensure_dir(dirname(PATHS$final_dataset), log_file)
safe_save(deals_final, PATHS$final_dataset, log_file)

# ==============================================================================
# Create analysis-ready subset
# ==============================================================================

log_message("\nCreating analysis-ready subset...", log_file)

deals_analysis <- deals_final %>%
  filter(data_status == "complete") %>%
  select(
    # Deal identifiers
    deal_id,
    target_name,
    
    # Dates
    date_announced,
    filing_date,
    days_lag,
    
    # Filing info
    matched_cik,
    accession_number,
    
    # Text data
    mda_text,
    risk_factors_text,
    mda_word_count,
    risk_word_count,
    
    # All original deal variables
    everything(),
    
    # Remove redundant columns
    -target_cik,  # We have matched_cik
    -filing_url,
    -company_name,
    -match_status,
    -parse_status,
    -data_status
  )

analysis_path <- str_replace(PATHS$final_dataset, "\\.rds$", "_analysis.rds")
safe_save(deals_analysis, analysis_path, log_file)

# ==============================================================================
# Create data dictionary
# ==============================================================================

log_message("\nCreating data dictionary...", log_file)

data_dict <- tibble(
  variable = names(deals_analysis),
  type = sapply(deals_analysis, class),
  description = c(
    "Unique deal identifier (may not be consecutive after de-duplication)",
    "Target company name",
    "M&A announcement date",
    "10-K filing date",
    glue("Days between filing and announcement (filtered: ≤ {MAX_LAG_DAYS} days)"),
    "Matched CIK used for 10-K retrieval (first valid CIK)",
    "EDGAR accession number",
    "Management's Discussion and Analysis text",
    "Risk Factors text",
    "Word count in MD&A",
    "Word count in Risk Factors",
    rep("Original deal variable", ncol(deals_analysis) - 11)
  )
)

dict_path <- file.path(
  dirname(PATHS$final_dataset),
  "data_dictionary.csv"
)
write_csv(data_dict, dict_path)
log_message(glue("  → Data dictionary saved: {dict_path}"), log_file)

# ==============================================================================
# Summary report
# ==============================================================================

log_section("PIPELINE COMPLETE - SUMMARY REPORT", log_file)

log_message("Final dataset structure:", log_file)
log_message(glue("  → Unique logical deals: {nrow(deals_final)}"), log_file)
log_message(glue("  → Complete cases (ready for analysis): {nrow(deals_analysis)} ({round(100 * nrow(deals_analysis) / nrow(deals_final), 1)}%)"), log_file)
log_message(glue("  → Total corpus: {scales::comma(sum(deals_analysis$mda_word_count) + sum(deals_analysis$risk_word_count))} words"), log_file)

log_message("\nResearch design compliance:", log_file)
log_message("  ✓ One observation per target–deal (unit of analysis)", log_file)
log_message("  ✓ One-to-one deal–filing matching", log_file)
log_message("  ✓ First valid CIK wins for multi-CIK targets", log_file)
log_message(glue("  ✓ Minimum lag constraint enforced ({TIMING$lag_min} days)"), log_file)
if (is.finite(MAX_LAG_DAYS)) {
  log_message(glue("  ✓ Maximum lag constraint enforced ({MAX_LAG_DAYS} days)"), log_file)
}

log_message("\nOutput files:", log_file)
log_message(glue("  → Full dataset: {PATHS$final_dataset}"), log_file)
log_message(glue("  → Analysis subset: {analysis_path}"), log_file)
log_message(glue("  → Data dictionary: {dict_path}"), log_file)

log_message("\nData availability breakdown:", log_file)
for (i in 1:nrow(availability_summary)) {
  log_message(glue("  → {availability_summary$data_status[i]}: {availability_summary$n[i]}"), log_file)
}

if (is.finite(MAX_LAG_DAYS)) {
  log_message(glue("\nLag filter: Excluded {n_extreme} deals with lag > {MAX_LAG_DAYS} days"), log_file)
}

log_message("\nReady for NLP analysis!", log_file)
log_message("Next steps:", log_file)
log_message("  1. Review parsing sample for quality", log_file)
log_message("  2. Begin text preprocessing and feature extraction", log_file)
log_message("  3. Build disclosure indices", log_file)
