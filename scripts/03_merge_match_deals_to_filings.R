# ==============================================================================
# Step 3: Match Deals to Filings
# Apply timing rules and match each deal to the most recent eligible 10-K
# 
# CRITICAL: This step implements "first valid CIK wins" deduplication.
#           Input may have multiple rows per deal (CIK candidates).
#           Output has exactly ONE row per logical deal.
#
# INPUT:  data/interim/deals_clean.rds (may have multiple CIK candidates per deal)
#         data/interim/edgar_10k_index.rds
# OUTPUT: data/interim/deals_filing_matched.rds (ONE row per deal)
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
log_file <- init_logging(PATHS$log_dir, "03_merge_match_deals_to_filings")
log_section("STEP 3: MATCH DEALS TO FILINGS", log_file)

# ==============================================================================
# Load data
# ==============================================================================

log_message("Loading data...", log_file)
deals_clean <- readRDS(PATHS$deals_clean)
edgar_index <- readRDS(PATHS$edgar_index)

n_input_rows <- nrow(deals_clean)
n_unique_deals <- n_distinct(deals_clean$logical_deal_id)

log_message(glue("  → Input rows (with CIK candidates): {n_input_rows}"), log_file)
log_message(glue("  → Unique logical deals: {n_unique_deals}"), log_file)
log_message(glue("  → EDGAR filings: {nrow(edgar_index)}"), log_file)

# ==============================================================================
# Matching function for a single deal-CIK combination
# ==============================================================================

match_single_cik <- function(cik, date_announced, edgar_index, lag_min) {
  # Clean CIK format
  cik_clean <- clean_cik(cik)
  
  # Filter filings for this CIK
  cik_filings <- edgar_index %>%
    filter(cik == cik_clean)
  
  if (nrow(cik_filings) == 0) {
    return(list(
      matched = FALSE,
      filing_date = as.Date(NA),
      accession_number = NA_character_,
      filing_url = NA_character_,
      company_name = NA_character_,
      days_lag = NA_real_,
      n_eligible_filings = 0L
    ))
  }
  
  # Apply timing filter: filing must be at least lag_min days before announcement
  max_filing_date <- date_announced - lag_min
  
  eligible_filings <- cik_filings %>%
    filter(filing_date <= max_filing_date) %>%
    arrange(desc(filing_date))
  
  if (nrow(eligible_filings) == 0) {
    return(list(
      matched = FALSE,
      filing_date = as.Date(NA),
      accession_number = NA_character_,
      filing_url = NA_character_,
      company_name = NA_character_,
      days_lag = NA_real_,
      n_eligible_filings = 0L
    ))
  }
  
  # Take the most recent eligible filing
  best_filing <- eligible_filings[1, ]
  
  list(
    matched = TRUE,
    filing_date = best_filing$filing_date,
    accession_number = best_filing$accession_number,
    filing_url = best_filing$filing_url,
    company_name = best_filing$company_name,
    days_lag = as.numeric(date_announced - best_filing$filing_date),
    n_eligible_filings = nrow(eligible_filings)
  )
}

# ==============================================================================
# Match all deal-CIK combinations
# ==============================================================================

log_message(glue("\nMatching deals to filings with lag_min = {TIMING$lag_min} days..."), log_file)
log_message("Processing all CIK candidates for each deal...\n", log_file)

# Create progress bar
progress <- create_progress(n_input_rows, "Matching deals")

# Match each row (deal-CIK combination)
match_results <- vector("list", n_input_rows)

for (i in 1:n_input_rows) {
  row <- deals_clean[i, ]
  
  result <- match_single_cik(
    cik = row$target_cik,
    date_announced = row$date_announced,
    edgar_index = edgar_index,
    lag_min = TIMING$lag_min
  )
  
  match_results[[i]] <- tibble(
    row_index = i,
    matched_cik = if (result$matched) clean_cik(row$target_cik) else NA_character_,
    filing_date = result$filing_date,
    accession_number = result$accession_number,
    filing_url = result$filing_url,
    company_name = result$company_name,
    days_lag = result$days_lag,
    match_status = if (result$matched) "matched" else "no_valid_filing",
    n_eligible_filings = result$n_eligible_filings
  )
  
  progress$update(i)
}

progress$close()

# Combine results with original data
matches_df <- bind_rows(match_results)
deals_with_matches <- bind_cols(deals_clean, matches_df %>% select(-row_index))

log_message("\nMatching complete. Now deduplicating to one row per deal...", log_file)

# ==============================================================================
# CRITICAL: Deduplicate to ONE row per logical deal
# Strategy: "First valid CIK wins"
#   1. If any CIK candidate has a match, use that one
#   2. If multiple CIKs have matches, use the one with shortest lag (most recent filing)
#   3. If no CIK has a match, keep the first row (for tracking purposes)
# ==============================================================================

log_message("\nApplying 'first valid CIK wins' deduplication...", log_file)

deals_filing_matched <- deals_with_matches %>%
  group_by(logical_deal_id) %>%
  arrange(
    # Priority 1: matched status (matched first)
    desc(match_status == "matched"),
    # Priority 2: shortest lag (most recent filing) among matched
    days_lag
  ) %>%
  slice(1) %>%  # Take the best row for each deal
  ungroup()

# Verify deduplication
n_output_rows <- nrow(deals_filing_matched)
n_output_deals <- n_distinct(deals_filing_matched$logical_deal_id)

if (n_output_rows != n_output_deals) {
  log_message("  ✗ ERROR: Deduplication failed!", log_file, "ERROR")
  stop("Deduplication failed: output rows != unique deals")
}

if (n_output_rows != n_unique_deals) {
  log_message("  ✗ ERROR: Lost deals during deduplication!", log_file, "ERROR")
  stop(glue("Lost deals: input had {n_unique_deals}, output has {n_output_rows}"))
}

log_message(glue("  ✓ Deduplication successful: {n_output_rows} rows (one per deal)"), log_file)
log_message(glue("  → Removed {n_input_rows - n_output_rows} CIK candidate rows"), log_file)

# ==============================================================================
# Match statistics
# ==============================================================================

log_message("\nMatch statistics:", log_file)

match_summary <- deals_filing_matched %>%
  count(match_status) %>%
  mutate(pct = round(100 * n / sum(n), 1))

for (i in 1:nrow(match_summary)) {
  status <- match_summary$match_status[i]
  count <- match_summary$n[i]
  pct <- match_summary$pct[i]
  log_message(glue("  → {status}: {count} ({pct}%)"), log_file)
}

# Statistics for matched deals
matched_deals <- deals_filing_matched %>%
  filter(match_status == "matched")

if (nrow(matched_deals) > 0) {
  log_message("\nMatched deals statistics:", log_file)
  log_message(glue("  → Mean lag: {round(mean(matched_deals$days_lag), 1)} days"), log_file)
  log_message(glue("  → Median lag: {median(matched_deals$days_lag)} days"), log_file)
  log_message(glue("  → Min lag: {min(matched_deals$days_lag)} days"), log_file)
  log_message(glue("  → Max lag: {max(matched_deals$days_lag)} days"), log_file)
}

# ==============================================================================
# Validation checks
# ==============================================================================

log_message("\nValidation checks:", log_file)

validation_passed <- TRUE

# Check 1: One row per deal
check_one_row <- n_distinct(deals_filing_matched$logical_deal_id) == nrow(deals_filing_matched)
if (check_one_row) {
  log_message("  ✓ One row per logical deal", log_file)
} else {
  log_message("  ✗ Multiple rows per deal detected!", log_file, "ERROR")
  validation_passed <- FALSE
}

# Check 2: All original deals preserved
check_preserved <- n_distinct(deals_filing_matched$logical_deal_id) == n_unique_deals
if (check_preserved) {
  log_message(glue("  ✓ All {n_unique_deals} deals preserved"), log_file)
} else {
  log_message("  ✗ Some deals were lost!", log_file, "ERROR")
  validation_passed <- FALSE
}

# Check 3: Lag constraint for matched deals
if (nrow(matched_deals) > 0) {
  check_lag <- all(matched_deals$days_lag >= TIMING$lag_min)
  if (check_lag) {
    log_message(glue("  ✓ All matched deals have lag >= {TIMING$lag_min} days"), log_file)
  } else {
    log_message(glue("  ✗ Some matched deals have lag < {TIMING$lag_min} days"), log_file, "ERROR")
    validation_passed <- FALSE
  }
  
  # Check 4: Filing dates before announcement dates
  check_dates <- all(matched_deals$filing_date < matched_deals$date_announced)
  if (check_dates) {
    log_message("  ✓ All filing dates are before announcement dates", log_file)
  } else {
    log_message("  ✗ Some filing dates are after announcement dates!", log_file, "ERROR")
    validation_passed <- FALSE
  }
}

if (!validation_passed) {
  stop("Validation failed - check log for details")
}

# ==============================================================================
# Save results
# ==============================================================================

log_message("\nSaving matched deals...", log_file)
ensure_dir(dirname(PATHS$deals_filing_matched), log_file)
safe_save(deals_filing_matched, PATHS$deals_filing_matched, log_file)

# ==============================================================================
# Prepare filing list for next step
# ==============================================================================

log_message("\nPreparing unique filings list for extraction...", log_file)

# Get unique filings (multiple deals may reference same 10-K)
unique_filings <- deals_filing_matched %>%
  filter(match_status == "matched") %>%
  distinct(accession_number, .keep_all = TRUE) %>%
  select(accession_number, filing_url, filing_date, matched_cik)

n_unique_filings <- nrow(unique_filings)
n_matched_deals <- nrow(matched_deals)

log_message(glue("  → Matched deals: {n_matched_deals}"), log_file)
log_message(glue("  → Unique filings to extract: {n_unique_filings}"), log_file)

if (n_unique_filings < n_matched_deals) {
  n_shared <- n_matched_deals - n_unique_filings
  log_message(glue("  → Deals sharing a filing: {n_shared} (same company, multiple deals)"), log_file)
}

# Save unique filings list
filings_path <- file.path(dirname(PATHS$deals_filing_matched), "filings_to_download.rds")
saveRDS(unique_filings, filings_path)
log_message(glue("  → Saved: {filings_path}"), log_file)

# ==============================================================================
# Summary
# ==============================================================================

log_section("STEP 3 COMPLETE", log_file)
log_message(glue("Output: {PATHS$deals_filing_matched}"), log_file)
log_message(glue("Deals: {n_output_rows} (one row per deal)"), log_file)
log_message(glue("Matched: {nrow(matched_deals)} ({round(100 * nrow(matched_deals) / n_output_rows, 1)}%)"), log_file)
log_message(glue("Unique filings to extract: {n_unique_filings}"), log_file)
log_message("\nNext step: Run 04_ingest_download_10k_filings.R", log_file)
