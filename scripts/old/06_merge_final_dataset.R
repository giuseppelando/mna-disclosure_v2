# ==============================================================================
# Step 6: Merge Final Dataset
# Combine deals, filing matches, and parsed sections into final dataset
# ==============================================================================

library(tidyverse)
library(glue)

# Load configuration and utilities
source("C:/Users/giuse/Documents/GitHub/mna-disclosure/config/pipeline_config.R")
source("C:/Users/giuse/Documents/GitHub/mna-disclosure/src/99_utils/utils.R")

# Initialize logging
log_file <- init_logging(PATHS$log_dir, "06_merge_final_dataset")
log_section("STEP 6: MERGE FINAL DATASET", log_file)

# ==============================================================================
# Load all data
# ==============================================================================

log_message("Loading data...", log_file)

deals_filing_matched <- readRDS(PATHS$deals_filing_matched)
parsed_sections <- readRDS(PATHS$parsed_sections)

log_message(glue("  → Deals with filing matches: {nrow(deals_filing_matched)}"), log_file)
log_message(glue("  → Parsed sections: {nrow(parsed_sections)}"), log_file)

# ==============================================================================
# Merge deals with parsed sections
# ==============================================================================

log_message("\nMerging datasets...", log_file)

# Join deals with parsed sections
deals_final <- deals_filing_matched %>%
  left_join(
    parsed_sections %>% 
      select(accession_number, mda_text, risk_factors_text, 
             mda_word_count, risk_word_count, parse_status),
    by = "accession_number"
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

# Lag statistics
lag_stats <- complete_cases %>%
  summarise(
    mean_lag = round(mean(days_lag), 1),
    median_lag = median(days_lag),
    min_lag = min(days_lag),
    max_lag = max(days_lag)
  )

log_message("  Filing-to-announcement lag:", log_file)
log_message(glue("    Mean: {lag_stats$mean_lag} days"), log_file)
log_message(glue("    Median: {lag_stats$median_lag} days"), log_file)
log_message(glue("    Range: {lag_stats$min_lag} to {lag_stats$max_lag} days"), log_file)

# ==============================================================================
# Validation checks
# ==============================================================================

log_message("\nValidation checks:", log_file)

validation_results <- list(
  # All original deals preserved
  deals_preserved = nrow(deals_final) == nrow(deals_filing_matched),
  
  # No duplicate deals
  no_duplicates = n_distinct(deals_final$deal_id) == nrow(deals_final),
  
  # Lag constraint satisfied for complete cases
  lag_constraint = all(complete_cases$days_lag >= TIMING$lag_min),
  
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
    "Unique deal identifier",
    "Target company name",
    "M&A announcement date",
    "10-K filing date",
    "Days between filing and announcement",
    "Matched CIK used for 10-K retrieval",
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

log_message("Final dataset:", log_file)
log_message(glue("  → Total deals: {nrow(deals_final)}"), log_file)
log_message(glue("  → Complete cases: {nrow(deals_analysis)} ({round(100 * nrow(deals_analysis) / nrow(deals_final), 1)}%)"), log_file)
log_message(glue("  → Total corpus: {scales::comma(sum(deals_analysis$mda_word_count) + sum(deals_analysis$risk_word_count))} words"), log_file)

log_message("\nOutput files:", log_file)
log_message(glue("  → Full dataset: {PATHS$final_dataset}"), log_file)
log_message(glue("  → Analysis subset: {analysis_path}"), log_file)
log_message(glue("  → Data dictionary: {dict_path}"), log_file)

log_message("\nData availability breakdown:", log_file)
for (i in 1:nrow(availability_summary)) {
  log_message(glue("  → {availability_summary$data_status[i]}: {availability_summary$n[i]}"), log_file)
}

log_message("\nReady for NLP analysis!", log_file)
log_message("Next steps:", log_file)
log_message("  1. Review parsing sample for quality", log_file)
log_message("  2. Begin text preprocessing and feature extraction", log_file)
log_message("  3. Build disclosure indices", log_file)
