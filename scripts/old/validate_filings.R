# ==============================================================================
# Validation Script: Check Downloaded 10-K Filings
# Verify filings match expected CIK, date, and form type
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

cat("\n")
cat("================================================================================\n")
cat("10-K FILING VALIDATION\n")
cat("================================================================================\n\n")

# ==============================================================================
# Load data
# ==============================================================================

cat("Loading data...\n")

deals_matched <- readRDS(file.path(PROJECT_ROOT, "data/interim/deals_filing_matched.rds"))
edgar_index <- readRDS(file.path(PROJECT_ROOT, "data/interim/edgar_10k_index.rds"))

filings_dir <- file.path(PROJECT_ROOT, "data/raw/10k_filings")
downloaded_files <- list.files(filings_dir, full.names = FALSE)

cat(glue("  → Deals matched: {nrow(deals_matched)}\n"))
cat(glue("  → EDGAR index entries: {nrow(edgar_index)}\n"))
cat(glue("  → Downloaded files: {length(downloaded_files)}\n\n"))

# ==============================================================================
# Extract metadata from filenames
# ==============================================================================

cat("Extracting metadata from downloaded files...\n")

# Parse filename: {ACCESSION}.txt (simple format)
file_metadata <- tibble(filename = downloaded_files) %>%
  mutate(
    # Remove .txt extension to get accession number
    accession_from_file = str_remove(filename, "\\.txt$")
  )

cat(glue("  → Parsed {nrow(file_metadata)} filenames\n\n"))

# ==============================================================================
# Check 1: CIK validation
# ==============================================================================

cat("CHECK 1: CIK VALIDATION\n")
cat("------------------------\n")

# Get expected CIKs from deals (these are unpadded)
expected_ciks <- deals_matched %>%
  filter(match_status == "matched") %>%
  pull(matched_cik) %>%
  unique()

# Get CIKs from EDGAR index by matching accession numbers
downloaded_ciks <- edgar_index %>%
  filter(accession_number %in% file_metadata$accession_from_file) %>%
  pull(cik) %>%
  unique()

# Compare
ciks_matched <- sum(downloaded_ciks %in% expected_ciks)
ciks_unexpected <- sum(!downloaded_ciks %in% expected_ciks)

cat(glue("Expected unique CIKs: {length(expected_ciks)}\n"))
cat(glue("Downloaded unique CIKs: {length(downloaded_ciks)}\n"))
cat(glue("  → CIKs matching expectations: {ciks_matched} ({round(100*ciks_matched/length(downloaded_ciks), 1)}%)\n"))

if (ciks_unexpected > 0) {
  cat(glue("  ⚠ Unexpected CIKs found: {ciks_unexpected}\n"))
  unexpected <- setdiff(downloaded_ciks, expected_ciks)
  cat("  Unexpected CIKs (showing first 10):\n")
  print(head(unexpected, 10))
} else {
  cat("  ✓ All CIKs match expectations\n")
}

cat("\n")

# ==============================================================================
# Check 2: Filing dates validation
# ==============================================================================

cat("CHECK 2: FILING DATE VALIDATION\n")
cat("--------------------------------\n")

# Join downloaded files with EDGAR index to get filing dates
files_with_dates <- file_metadata %>%
  left_join(
    edgar_index %>% select(accession_number, filing_date, form_type, cik),
    by = c("accession_from_file" = "accession_number")
  )

# Check form type
form_10k_count <- sum(files_with_dates$form_type == "10-K", na.rm = TRUE)
other_forms <- sum(files_with_dates$form_type != "10-K", na.rm = TRUE)

cat(glue("Form type distribution:\n"))
cat(glue("  → 10-K: {form_10k_count} ({round(100*form_10k_count/nrow(files_with_dates), 1)}%)\n"))

if (other_forms > 0) {
  cat(glue("  ⚠ Non-10-K forms: {other_forms}\n"))
  other_form_types <- files_with_dates %>% 
    filter(form_type != "10-K") %>% 
    count(form_type)
  print(other_form_types)
} else {
  cat("  ✓ All filings are 10-K\n")
}

# Check filing date range
filing_date_range <- files_with_dates %>%
  filter(!is.na(filing_date)) %>%
  summarise(
    min_date = min(filing_date),
    max_date = max(filing_date),
    span_years = as.numeric(difftime(max_date, min_date, units = "days")) / 365.25
  )

cat(glue("\nFiling date range:\n"))
cat(glue("  → Earliest: {filing_date_range$min_date}\n"))
cat(glue("  → Latest: {filing_date_range$max_date}\n"))
cat(glue("  → Span: {round(filing_date_range$span_years, 1)} years\n"))

# Check if any filings are before 2005 (should not exist)
pre_2005 <- sum(files_with_dates$filing_date < as.Date("2005-01-01"), na.rm = TRUE)
if (pre_2005 > 0) {
  cat(glue("  ⚠ Filings before 2005: {pre_2005} (Risk Factors not mandatory)\n"))
} else {
  cat("  ✓ All filings are from 2005 or later\n")
}

cat("\n")

# ==============================================================================
# Check 3: Deal-level validation
# ==============================================================================

cat("CHECK 3: DEAL-LEVEL VALIDATION\n")
cat("-------------------------------\n")

# For each matched deal, check if the filing was downloaded
deal_validation <- deals_matched %>%
  filter(match_status == "matched") %>%
  mutate(
    # Files are named simply: {accession}.txt
    expected_filename = glue("{accession_number}.txt"),
    file_exists = expected_filename %in% downloaded_files
  )

n_expected <- nrow(deal_validation)
n_downloaded <- sum(deal_validation$file_exists)
n_missing <- sum(!deal_validation$file_exists)

cat(glue("Matched deals: {n_expected}\n"))
cat(glue("  → Filings downloaded: {n_downloaded} ({round(100*n_downloaded/n_expected, 1)}%)\n"))
cat(glue("  → Missing filings: {n_missing} ({round(100*n_missing/n_expected, 1)}%)\n"))

if (n_missing > 0) {
  cat("\nSample of missing filings:\n")
  missing_sample <- deal_validation %>%
    filter(!file_exists) %>%
    select(deal_id, target_name, date_announced, matched_cik, filing_date, expected_filename) %>%
    head(10)
  print(missing_sample)
}

cat("\n")

# ==============================================================================
# Check 4: Timing validation (lag >= 90 days)
# ==============================================================================

cat("CHECK 4: TIMING VALIDATION\n")
cat("---------------------------\n")

timing_validation <- deal_validation %>%
  filter(file_exists) %>%
  mutate(
    lag_days = as.numeric(date_announced - filing_date)
  )

lag_summary <- summary(timing_validation$lag_days)

cat("Filing-to-announcement lag (days):\n")
cat(glue("  → Min: {lag_summary[1]}\n"))
cat(glue("  → Q1: {lag_summary[2]}\n"))
cat(glue("  → Median: {lag_summary[3]}\n"))
cat(glue("  → Mean: {round(lag_summary[4], 1)}\n"))
cat(glue("  → Q3: {lag_summary[5]}\n"))
cat(glue("  → Max: {lag_summary[6]}\n"))

# Check minimum lag requirement (should be >= 90)
below_min_lag <- sum(timing_validation$lag_days < 90, na.rm = TRUE)
if (below_min_lag > 0) {
  cat(glue("\n  ⚠ Filings with lag < 90 days: {below_min_lag}\n"))
  cat("  This should not happen - check matching logic!\n")
} else {
  cat("\n  ✓ All filings meet 90-day minimum lag requirement\n")
}

# Check for suspiciously long lags (> 5 years)
very_long_lag <- sum(timing_validation$lag_days > 1825, na.rm = TRUE)
if (very_long_lag > 0) {
  cat(glue("  ⚠ Filings with lag > 5 years: {very_long_lag}\n"))
  cat("  Sample of very old filings:\n")
  old_filings <- timing_validation %>%
    filter(lag_days > 1825) %>%
    select(deal_id, target_name, date_announced, filing_date, lag_days) %>%
    arrange(desc(lag_days)) %>%
    head(5)
  print(old_filings)
}

cat("\n")

# ==============================================================================
# Check 5: Sample inspection
# ==============================================================================

cat("CHECK 5: RANDOM SAMPLE INSPECTION\n")
cat("----------------------------------\n")

# Take 5 random deals and show their details
set.seed(42)
sample_deals <- timing_validation %>%
  filter(file_exists) %>%
  sample_n(min(5, nrow(timing_validation))) %>%
  select(
    deal_id,
    target_name,
    date_announced,
    matched_cik,
    filing_date,
    lag_days,
    expected_filename
  ) %>%
  arrange(date_announced)

cat("Random sample of matched deals:\n")
print(sample_deals, width = 120)

cat("\n")

# ==============================================================================
# Summary
# ==============================================================================

cat("================================================================================\n")
cat("VALIDATION SUMMARY\n")
cat("================================================================================\n\n")

all_checks_passed <- TRUE

if (ciks_unexpected > 0) {
  cat("❌ Unexpected CIKs found\n")
  all_checks_passed <- FALSE
} else {
  cat("✓ All CIKs match expectations\n")
}

if (other_forms > 0) {
  cat("❌ Non-10-K forms found\n")
  all_checks_passed <- FALSE
} else {
  cat("✓ All filings are 10-K\n")
}

if (pre_2005 > 0) {
  cat("❌ Pre-2005 filings found\n")
  all_checks_passed <- FALSE
} else {
  cat("✓ All filings from 2005+\n")
}

if (n_missing > 0) {
  cat(glue("⚠ {n_missing} matched deals missing downloaded files\n"))
  all_checks_passed <- FALSE
} else {
  cat("✓ All matched deals have downloaded files\n")
}

if (below_min_lag > 0) {
  cat("❌ Some filings violate 90-day minimum lag\n")
  all_checks_passed <- FALSE
} else {
  cat("✓ All filings meet 90-day minimum lag\n")
}

if (very_long_lag > 10) {
  cat(glue("⚠ {very_long_lag} filings have very long lags (>5 years)\n"))
}

cat("\n")

if (all_checks_passed) {
  cat("🎉 ALL VALIDATION CHECKS PASSED!\n")
  cat("The downloaded filings match your requirements.\n")
} else {
  cat("⚠ SOME ISSUES FOUND\n")
  cat("Review the checks above and re-run pipeline if needed.\n")
}

cat("\n")
cat("================================================================================\n")
