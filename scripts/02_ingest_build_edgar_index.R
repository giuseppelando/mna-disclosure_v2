# ==============================================================================
# Step 2: Build EDGAR Index
# Query EDGAR for all 10-K filings for unique CIKs in the sample
# ==============================================================================

library(tidyverse)
library(glue)

# Detect project root
if (!exists("PROJECT_ROOT")) {
  current_dir <- getwd()
  if (dir.exists(file.path(current_dir, "config")) && 
      dir.exists(file.path(current_dir, "src"))) {
    PROJECT_ROOT <- current_dir
  } else {
    PROJECT_ROOT <- dirname(current_dir)
  }
}

# Load configuration and utilities
source(file.path(PROJECT_ROOT, "config/pipeline_config.R"))
source(file.path(PROJECT_ROOT, "src/99_utils/utils.R"))
source(file.path(PROJECT_ROOT, "src/99_utils/edgar_utils.R"))

# Initialize logging
log_file <- init_logging(PATHS$log_dir, "02_ingest_build_edgar_index")
log_section("STEP 2: BUILD EDGAR INDEX", log_file)

# ==============================================================================
# Load cleaned deals
# ==============================================================================

log_message("Loading cleaned deals...", log_file)
deals_clean <- readRDS(PATHS$deals_clean)
log_message(glue("  → Loaded {nrow(deals_clean)} deals"), log_file)

# ==============================================================================
# Extract unique CIKs
# ==============================================================================

log_message("\nExtracting unique CIKs...", log_file)

# Split multiple CIKs and get unique list
all_ciks <- deals_clean %>%
  pull(target_cik) %>%
  str_split(",\\s*") %>%
  unlist() %>%
  str_trim() %>%
  unique() %>%
  clean_cik()  # Clean CIK format

log_message(glue("  → Total unique CIKs to query: {length(all_ciks)}"), log_file)

# ==============================================================================
# Check if index already exists (incremental update)
# ==============================================================================

existing_index <- NULL
if (file.exists(PATHS$edgar_index)) {
  log_message("\nFound existing EDGAR index...", log_file)
  existing_index <- readRDS(PATHS$edgar_index)
  
  existing_ciks <- unique(existing_index$cik)
  new_ciks <- setdiff(all_ciks, existing_ciks)
  
  log_message(glue("  → Existing CIKs in index: {length(existing_ciks)}"), log_file)
  log_message(glue("  → New CIKs to query: {length(new_ciks)}"), log_file)
  
  ciks_to_query <- new_ciks
} else {
  log_message("\nNo existing index found. Building from scratch...", log_file)
  ciks_to_query <- all_ciks
}

# ==============================================================================
# Query EDGAR for each CIK
# ==============================================================================

if (length(ciks_to_query) > 0) {
  log_message(glue("\nQuerying EDGAR for {length(ciks_to_query)} CIKs..."), log_file)
  log_message(glue("Rate limit: {EDGAR$rate_limit_per_second} requests/second"), log_file)
  log_message("This may take a while. Progress will be shown.\n", log_file)
  
  # Create rate limiter
  rate_limiter <- create_rate_limiter(EDGAR$rate_limit_per_second)
  
  # Initialize progress bar
  progress <- create_progress(length(ciks_to_query), "Querying EDGAR")
  
  # Initialize results list
  all_filings <- list()
  failed_ciks <- c()
  
  # Query each CIK
  for (i in seq_along(ciks_to_query)) {
    cik <- ciks_to_query[i]
    
    # Apply rate limit
    rate_limiter()
    
    # Query with retry logic
    filings <- retry_on_error(
      {query_edgar_filings(cik, EDGAR$user_agent, form_type = "10-K")},
      max_retries = EDGAR$max_retries,
      delay_seconds = EDGAR$retry_delay_seconds,
      log_file = log_file
    )
    
    if (!is.null(filings) && nrow(filings) > 0) {
      all_filings[[i]] <- filings
    } else {
      failed_ciks <- c(failed_ciks, cik)
    }
    
    # Update progress
    progress$update(i)
    
    # Periodic save (every 100 CIKs)
    if (i %% 100 == 0) {
      temp_results <- bind_rows(all_filings)
      saveRDS(temp_results, paste0(PATHS$edgar_index, ".temp"))
    }
  }
  
  progress$close()
  
  # Combine all results
  new_filings <- bind_rows(all_filings)
  
  log_message(glue("\nQuery complete:"), log_file)
  log_message(glue("  → Successful: {length(ciks_to_query) - length(failed_ciks)}"), log_file)
  log_message(glue("  → Failed: {length(failed_ciks)}"), log_file)
  log_message(glue("  → Total filings retrieved: {nrow(new_filings)}"), log_file)
  
  # Combine with existing index if present
  if (!is.null(existing_index)) {
    edgar_index <- bind_rows(existing_index, new_filings)
  } else {
    edgar_index <- new_filings
  }
  
} else {
  log_message("\nNo new CIKs to query. Using existing index.", log_file)
  edgar_index <- existing_index
}

# ==============================================================================
# Filter by sample period and add filing URLs
# ==============================================================================

log_message("\nFiltering and enriching index...", log_file)

edgar_index_final <- edgar_index %>%
  # Filter by sample period
  filter(
    filing_date >= as.Date(SAMPLE_PERIOD$start_date),
    filing_date <= as.Date(SAMPLE_PERIOD$end_date)
  ) %>%
  # Build filing URLs
  mutate(
    filing_url = build_filing_url(cik, accession_number, primary_document)
  ) %>%
  # Remove duplicates
  distinct(cik, filing_date, accession_number, .keep_all = TRUE) %>%
  # Sort
  arrange(cik, filing_date)

log_message(glue("  → Filings in sample period: {nrow(edgar_index_final)}"), log_file)
log_message(glue("  → Unique CIKs: {n_distinct(edgar_index_final$cik)}"), log_file)
log_message(glue("  → Date range: {min(edgar_index_final$filing_date)} to {max(edgar_index_final$filing_date)}"), log_file)

# ==============================================================================
# Coverage analysis
# ==============================================================================

log_message("\nCoverage analysis...", log_file)

coverage_stats <- edgar_index_final %>%
  group_by(cik) %>%
  summarise(
    n_filings = n(),
    first_filing = min(filing_date),
    last_filing = max(filing_date),
    .groups = "drop"
  )

log_message(glue("  → CIKs with filings: {nrow(coverage_stats)}"), log_file)
log_message(glue("  → Average filings per CIK: {round(mean(coverage_stats$n_filings), 1)}"), log_file)
log_message(glue("  → Median filings per CIK: {median(coverage_stats$n_filings)}"), log_file)

# CIKs with no filings
ciks_no_filings <- setdiff(all_ciks, edgar_index_final$cik)
if (length(ciks_no_filings) > 0) {
  log_message(glue("  ⚠ CIKs with no 10-K filings: {length(ciks_no_filings)}"), log_file, "WARNING")
}

# ==============================================================================
# Save index
# ==============================================================================

log_message("\nSaving EDGAR index...", log_file)
ensure_dir(dirname(PATHS$edgar_index), log_file)
safe_save(edgar_index_final, PATHS$edgar_index, log_file)

# Clean up temp file if exists
temp_file <- paste0(PATHS$edgar_index, ".temp")
if (file.exists(temp_file)) {
  file.remove(temp_file)
  log_message("  → Removed temporary file", log_file)
}

# ==============================================================================
# Summary
# ==============================================================================

log_section("STEP 2 COMPLETE", log_file)
log_message(glue("Output: {PATHS$edgar_index}"), log_file)
log_message(glue("Total 10-K filings indexed: {nrow(edgar_index_final)}"), log_file)
log_message(glue("Unique CIKs covered: {n_distinct(edgar_index_final$cik)}"), log_file)
log_message("\nNext step: Run 03_merge_match_deals_to_filings.R", log_file)
