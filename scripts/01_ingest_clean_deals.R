# ==============================================================================
# Step 1: Load and Clean Deals
# Remove deals without CIK, prepare for matching
# 
# INPUT:  data/interim/deals_with_cik_matches.xlsx (from CIK matching process)
# OUTPUT: data/interim/deals_clean.rds
#
# NOTE: Input may have multiple rows per deal (multiple CIK candidates).
#       This script preserves all CIK candidates - deduplication happens in Step 3
#       after we know which CIK actually has a valid filing.
# ==============================================================================

library(tidyverse)
library(readxl)
library(glue)

# Detect project root
current_dir <- getwd()
if (dir.exists(file.path(current_dir, "config")) && 
    dir.exists(file.path(current_dir, "src"))) {
  PROJECT_ROOT <- current_dir
} else {
  PROJECT_ROOT <- dirname(current_dir)
}

# Load configuration
source(file.path(PROJECT_ROOT, "config/pipeline_config.R"))
source(file.path(PROJECT_ROOT, "src/99_utils/utils.R"))

# Initialize logging
log_file <- init_logging(PATHS$log_dir, "01_ingest_clean_deals")
log_section("STEP 1: LOAD AND CLEAN DEALS", log_file)

# ==============================================================================
# Load data
# ==============================================================================

log_message("Loading deals from Excel...", log_file)
deals_raw <- read_xlsx(PATHS$deals_matched)
log_message(glue("  → Loaded {nrow(deals_raw)} rows, {ncol(deals_raw)} columns"), log_file)

# ==============================================================================
# Data cleaning and validation
# ==============================================================================

log_message("\nCleaning and validating data...", log_file)

deals_clean <- deals_raw %>%
  # Ensure critical columns are correct type
  mutate(
    target_cik = as.character(target_cik),
    date_announced = as.Date(date_announced)
  ) %>%
  # Remove deals without CIK
  filter(!is.na(target_cik) & nzchar(target_cik)) %>%
  # Remove deals without announcement date
  filter(!is.na(date_announced))

# Summary statistics
n_original <- nrow(deals_raw)
n_removed_no_cik <- sum(is.na(deals_raw$target_cik) | !nzchar(as.character(deals_raw$target_cik)))
n_removed_no_date <- sum(is.na(deals_raw$date_announced) & !is.na(deals_raw$target_cik))
n_final <- nrow(deals_clean)

log_message("\nSummary:", log_file)
log_message(glue("  → Original rows: {n_original}"), log_file)
log_message(glue("  → Removed (no CIK): {n_removed_no_cik}"), log_file)
log_message(glue("  → Removed (no announcement date): {n_removed_no_date}"), log_file)
log_message(glue("  → Final rows: {n_final}"), log_file)
log_message(glue("  → Coverage: {round(100 * n_final / n_original, 1)}%"), log_file)

# ==============================================================================
# Analyze deal structure (important for understanding duplicates)
# ==============================================================================

log_message("\nAnalyzing deal structure...", log_file)

# Identify the correct deal_id column (from SDC)
# Look for existing deal identifier columns
deal_id_candidates <- c("deal_id", "dealid", "deal_number", "sdc_deal_id", "transaction_id")
existing_deal_id <- intersect(tolower(names(deals_clean)), deal_id_candidates)

if (length(existing_deal_id) > 0) {
  # Use existing deal_id
  deal_id_col <- names(deals_clean)[tolower(names(deals_clean)) == existing_deal_id[1]]
  log_message(glue("  → Using existing deal identifier: {deal_id_col}"), log_file)
} else {
  # Create logical deal identifier based on target + date
  log_message("  → No deal_id found, creating logical_deal_id from target_name + date_announced", log_file)
  deal_id_col <- "logical_deal_id"
}

# Create a canonical deal identifier for analysis
deals_clean <- deals_clean %>%
  mutate(
    logical_deal_id = paste(target_name, as.character(date_announced), sep = "___")
  )

# Count unique deals vs rows
n_unique_deals <- n_distinct(deals_clean$logical_deal_id)
n_rows <- nrow(deals_clean)

log_message(glue("  → Total rows (including CIK candidates): {n_rows}"), log_file)
log_message(glue("  → Unique logical deals (target + date): {n_unique_deals}"), log_file)

if (n_rows > n_unique_deals) {
  n_multi_cik_deals <- deals_clean %>%
    count(logical_deal_id) %>%
    filter(n > 1) %>%
    nrow()
  
  cik_distribution <- deals_clean %>%
    count(logical_deal_id) %>%
    count(n, name = "num_deals")
  
  log_message(glue("  → Deals with multiple CIK candidates: {n_multi_cik_deals}"), log_file)
  log_message("  → CIK candidates per deal distribution:", log_file)
  for (i in 1:nrow(cik_distribution)) {
    log_message(glue("      {cik_distribution$n[i]} CIK(s): {cik_distribution$num_deals[i]} deals"), log_file)
  }
  
  log_message("\n  ⚠ Multiple CIK candidates will be resolved in Step 3 (first valid CIK wins)", log_file)
}

# ==============================================================================
# Remove pure duplicates (same deal + same CIK appearing multiple times)
# ==============================================================================

log_message("\nRemoving pure duplicates (same deal + same CIK)...", log_file)

n_before_dedup <- nrow(deals_clean)

deals_clean <- deals_clean %>%
  distinct(logical_deal_id, target_cik, .keep_all = TRUE)

n_after_dedup <- nrow(deals_clean)
n_pure_dups_removed <- n_before_dedup - n_after_dedup

if (n_pure_dups_removed > 0) {
  log_message(glue("  → Removed {n_pure_dups_removed} pure duplicate rows"), log_file)
} else {
  log_message("  → No pure duplicates found", log_file)
}

# ==============================================================================
# Date range analysis
# ==============================================================================

log_message("\nDate range analysis...", log_file)

date_range <- deals_clean %>%
  summarise(
    min_date = min(date_announced, na.rm = TRUE),
    max_date = max(date_announced, na.rm = TRUE),
    span_years = as.numeric(difftime(max_date, min_date, units = "days")) / 365.25
  )

log_message(glue("  → Earliest announcement: {date_range$min_date}"), log_file)
log_message(glue("  → Latest announcement: {date_range$max_date}"), log_file)
log_message(glue("  → Span: {round(date_range$span_years, 1)} years"), log_file)

# ==============================================================================
# Save cleaned data
# ==============================================================================

log_message("\nSaving cleaned data...", log_file)
ensure_dir(dirname(PATHS$deals_clean), log_file)
safe_save(deals_clean, PATHS$deals_clean, log_file)

# ==============================================================================
# Final validation
# ==============================================================================

log_message("\nFinal validation checks...", log_file)

validation_results <- list(
  all_have_cik = all(!is.na(deals_clean$target_cik) & nzchar(deals_clean$target_cik)),
  all_have_date = all(!is.na(deals_clean$date_announced)),
  all_have_logical_id = all(!is.na(deals_clean$logical_deal_id)),
  date_range_valid = date_range$min_date >= as.Date("1990-01-01") & 
                     date_range$max_date <= Sys.Date(),
  no_pure_duplicates = nrow(deals_clean) == nrow(distinct(deals_clean, logical_deal_id, target_cik))
)

all_checks_passed <- all(unlist(validation_results))

if (all_checks_passed) {
  log_message("  ✓ All validation checks passed", log_file)
} else {
  log_message("  ✗ Some validation checks failed:", log_file, "ERROR")
  for (check_name in names(validation_results)) {
    status <- ifelse(validation_results[[check_name]], "✓", "✗")
    log_message(glue("    {status} {check_name}"), log_file)
  }
}

# ==============================================================================
# Summary for next step
# ==============================================================================

log_section("STEP 1 COMPLETE", log_file)
log_message(glue("Output: {PATHS$deals_clean}"), log_file)
log_message(glue("Rows: {nrow(deals_clean)} (with CIK candidates)"), log_file)
log_message(glue("Unique deals: {n_unique_deals}"), log_file)
log_message("\nNext step: Run 02_ingest_build_edgar_index.R", log_file)
