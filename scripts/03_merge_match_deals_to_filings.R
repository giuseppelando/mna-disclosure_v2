# ==============================================================================
# Step 3: Match Deals to Filings
# Apply timing rules and match each deal to the most recent eligible 10-K
# First valid CIK wins
# ==============================================================================

library(tidyverse)
library(glue)

# Load configuration and utilities
source("C:/Users/giuse/Documents/GitHub/mna-disclosure/config/pipeline_config.R")
source("C:/Users/giuse/Documents/GitHub/mna-disclosure/src/99_utils/utils.R")

# Initialize logging
log_file <- init_logging(PATHS$log_dir, "03_merge_match_deals_to_filings")
log_section("STEP 3: MATCH DEALS TO FILINGS", log_file)

# ==============================================================================
# Load data
# ==============================================================================

log_message("Loading data...", log_file)
deals_clean <- readRDS(PATHS$deals_clean)
edgar_index <- readRDS(PATHS$edgar_index)

log_message(glue("  → Deals: {nrow(deals_clean)}"), log_file)
log_message(glue("  → EDGAR filings: {nrow(edgar_index)}"), log_file)

# ==============================================================================
# Matching function
# ==============================================================================

#' Match a single deal to a 10-K filing
#'
#' @param deal_row Single row from deals_clean
#' @param edgar_index EDGAR index data frame
#' @param lag_min Minimum lag in days
#' @return Named list with match results
match_deal_to_filing <- function(deal_row, edgar_index, lag_min) {
  date_announced <- deal_row$date_announced
  target_cik <- deal_row$target_cik
  
  # Split CIKs (handle multiple CIKs)
  cik_list <- str_split(target_cik, ",\\s*")[[1]] %>%
    str_trim() %>%
    clean_cik()
  
  # Try each CIK in order until we find a valid filing
  for (cik in cik_list) {
    # Filter filings for this CIK
    cik_filings <- edgar_index %>%
      filter(cik == !!cik)
    
    if (nrow(cik_filings) == 0) {
      next  # No filings for this CIK, try next
    }
    
    # Apply timing filter
    max_filing_date <- date_announced - lag_min
    
    eligible_filings <- cik_filings %>%
      filter(filing_date <= max_filing_date) %>%
      arrange(desc(filing_date))
    
    if (nrow(eligible_filings) > 0) {
      # Found a valid filing - take the most recent
      matched_filing <- eligible_filings[1, ]
      
      return(list(
        matched_cik = cik,
        filing_date = matched_filing$filing_date,
        accession_number = matched_filing$accession_number,
        filing_url = matched_filing$filing_url,
        company_name = matched_filing$company_name,
        days_lag = as.numeric(date_announced - matched_filing$filing_date),
        match_status = "matched",
        n_ciks_tried = which(cik_list == cik),
        n_eligible_filings = nrow(eligible_filings)
      ))
    }
  }
  
  # No valid filing found for any CIK
  return(list(
    matched_cik = NA_character_,
    filing_date = as.Date(NA),
    accession_number = NA_character_,
    filing_url = NA_character_,
    company_name = NA_character_,
    days_lag = NA_real_,
    match_status = "no_valid_filing",
    n_ciks_tried = length(cik_list),
    n_eligible_filings = 0
  ))
}

# ==============================================================================
# Match all deals
# ==============================================================================

log_message(glue("\nMatching deals to filings with lag_min = {TIMING$lag_min} days..."), log_file)
log_message("This may take a few minutes.\n", log_file)

# Create progress bar
progress <- create_progress(nrow(deals_clean), "Matching deals")

# Match each deal
matches <- vector("list", nrow(deals_clean))

for (i in 1:nrow(deals_clean)) {
  matches[[i]] <- match_deal_to_filing(
    deals_clean[i, ],
    edgar_index,
    TIMING$lag_min
  )
  progress$update(i)
}

progress$close()

# ==============================================================================
# Combine results
# ==============================================================================

log_message("\nCombining results...", log_file)

# Convert list to data frame
matches_df <- bind_rows(matches)

# Combine with original deals
deals_filing_matched <- bind_cols(
  deals_clean,
  matches_df
)

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
  
  # How many CIKs were tried
  cik_tries <- matched_deals %>%
    count(n_ciks_tried) %>%
    arrange(n_ciks_tried)
  
  log_message("\n  CIKs tried before match:", log_file)
  for (i in 1:nrow(cik_tries)) {
    n_tried <- cik_tries$n_ciks_tried[i]
    count <- cik_tries$n[i]
    log_message(glue("    → {n_tried} CIK(s): {count} deals"), log_file)
  }
}

# ==============================================================================
# Validation checks
# ==============================================================================

log_message("\nValidation checks:", log_file)

# Check: all matched deals have lag >= lag_min
if (nrow(matched_deals) > 0) {
  min_lag_check <- all(matched_deals$days_lag >= TIMING$lag_min)
  if (min_lag_check) {
    log_message(glue("  ✓ All matched deals have lag >= {TIMING$lag_min} days"), log_file)
  } else {
    log_message(glue("  ✗ Some matched deals have lag < {TIMING$lag_min} days"), log_file, "ERROR")
  }
  
  # Check: filing dates are before announcement dates
  date_order_check <- all(matched_deals$filing_date < matched_deals$date_announced)
  if (date_order_check) {
    log_message("  ✓ All filing dates are before announcement dates", log_file)
  } else {
    log_message("  ✗ Some filing dates are after announcement dates", log_file, "ERROR")
  }
}

# Check: total deals preserved
if (nrow(deals_filing_matched) == nrow(deals_clean)) {
  log_message(glue("  ✓ All {nrow(deals_clean)} deals preserved"), log_file)
} else {
  log_message("  ✗ Deal count mismatch", log_file, "ERROR")
}

# ==============================================================================
# Save results
# ==============================================================================

log_message("\nSaving matched deals...", log_file)
ensure_dir(dirname(PATHS$deals_filing_matched), log_file)
safe_save(deals_filing_matched, PATHS$deals_filing_matched, log_file)

# ==============================================================================
# Prepare download list
# ==============================================================================

log_message("\nPreparing download list...", log_file)

filings_to_download <- deals_filing_matched %>%
  filter(match_status == "matched") %>%
  select(deal_id, matched_cik, accession_number, filing_url, filing_date) %>%
  distinct()

log_message(glue("  → Unique filings to download: {nrow(filings_to_download)}"), log_file)

# Save download list
download_list_path <- file.path(
  dirname(PATHS$deals_filing_matched),
  "filings_to_download.rds"
)
saveRDS(filings_to_download, download_list_path)
log_message(glue("  → Saved download list: {download_list_path}"), log_file)

# ==============================================================================
# Summary
# ==============================================================================

log_section("STEP 3 COMPLETE", log_file)
log_message(glue("Output: {PATHS$deals_filing_matched}"), log_file)
log_message(glue("Matched deals: {nrow(matched_deals)} / {nrow(deals_clean)} ({round(100 * nrow(matched_deals) / nrow(deals_clean), 1)}%)"), log_file)
log_message(glue("Filings to download: {nrow(filings_to_download)}"), log_file)
log_message("\nNext step: Run 04_ingest_download_10k_filings.R", log_file)

