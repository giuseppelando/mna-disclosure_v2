# ==============================================================================
# Step 1: Load and Clean Deals
# Remove deals without CIK, prepare for matching
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
source(file.path(PROJECT_ROOT,"config/pipeline_config.R"))
source(file.path(PROJECT_ROOT,"src/99_utils/utils.R"))

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
  filter(!is.na(target_cik)) %>%
  # Remove deals without announcement date
  filter(!is.na(date_announced)) %>%
  # Ensure each deal has a unique identifier
  mutate(
    deal_id = row_number()
  )

# Summary statistics
n_original <- nrow(deals_raw)
n_removed_no_cik <- sum(is.na(deals_raw$target_cik))
n_removed_no_date <- sum(is.na(deals_raw$date_announced) & !is.na(deals_raw$target_cik))
n_final <- nrow(deals_clean)

log_message("\nSummary:", log_file)
log_message(glue("  → Original deals: {n_original}"), log_file)
log_message(glue("  → Removed (no CIK): {n_removed_no_cik}"), log_file)
log_message(glue("  → Removed (no announcement date): {n_removed_no_date}"), log_file)
log_message(glue("  → Final clean deals: {n_final}"), log_file)
log_message(glue("  → Coverage: {round(100 * n_final / n_original, 1)}%"), log_file)

# ==============================================================================
# Analyze CIK structure
# ==============================================================================

log_message("\nAnalyzing CIK structure...", log_file)

# Count deals with multiple CIKs
# Note: Multiple CIKs appear as duplicate rows for the same deal (from matching process)
cik_counts <- deals_clean %>%
  group_by(target_name, date_announced) %>%
  summarise(
    n_ciks = n(),
    ciks = paste(unique(target_cik), collapse = ", "),
    .groups = "drop"
  ) %>%
  mutate(has_multiple_ciks = n_ciks > 1)

n_unique_deals <- nrow(cik_counts)
n_single_cik <- sum(!cik_counts$has_multiple_ciks)
n_multiple_cik <- sum(cik_counts$has_multiple_ciks)

log_message(glue("  → Unique deals (target_name + announcement_date): {n_unique_deals}"), log_file)
log_message(glue("  → Deals with single CIK: {n_single_cik} ({round(100 * n_single_cik / n_unique_deals, 1)}%)"), log_file)
log_message(glue("  → Deals with multiple CIKs: {n_multiple_cik} ({round(100 * n_multiple_cik / n_unique_deals, 1)}%)"), log_file)

if (n_multiple_cik > 0) {
  max_ciks <- max(cik_counts$n_ciks)
  avg_ciks <- mean(cik_counts$n_ciks[cik_counts$has_multiple_ciks])
  log_message(glue("  → Average CIKs per multi-CIK deal: {round(avg_ciks, 2)}"), log_file)
  log_message(glue("  → Maximum CIKs for a single deal: {max_ciks}"), log_file)
}

log_message(glue("  → Total rows in dataset: {n_final} (includes duplicates for multiple CIKs)"), log_file)

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
  total_deals = n_final,
  all_have_cik = all(!is.na(deals_clean$target_cik)),
  all_have_date = all(!is.na(deals_clean$date_announced)),
  unique_deal_ids = n_distinct(deals_clean$deal_id) == nrow(deals_clean),
  date_range_valid = date_range$min_date >= as.Date("1990-01-01") & 
                     date_range$max_date <= Sys.Date()
)

all_checks_passed <- all(unlist(validation_results[2:5]))

if (all_checks_passed) {
  log_message("  ✓ All validation checks passed", log_file)
} else {
  log_message("  ✗ Some validation checks failed:", log_file, "ERROR")
  for (check_name in names(validation_results)[2:5]) {
    status <- ifelse(validation_results[[check_name]], "✓", "✗")
    log_message(glue("    {status} {check_name}"), log_file)
  }
}

# ==============================================================================
# Summary for next step
# ==============================================================================

log_section("STEP 1 COMPLETE", log_file)
log_message(glue("Output: {PATHS$deals_clean}"), log_file)
log_message(glue("Clean deals ready for EDGAR matching: {n_final}"), log_file)
log_message("\nNext step: Run 02_ingest_build_edgar_index.R", log_file)

