# ==============================================================================
# 00_fix_existing_data.R
# One-time fix for existing data files that have duplicates
#
# RUN THIS ONCE to fix:
#   - deals_filing_matched.rds (multiple CIK candidates per deal)
#   - parsed_sections.rds (duplicate accession_numbers)
#
# After running this, you can proceed directly to 06_merge_final_dataset.R
# ==============================================================================

library(tidyverse)
library(glue)

cat("\n")
cat("==============================================================================\n")
cat("FIXING EXISTING DATA FILES\n")
cat("==============================================================================\n")

# Detect project root
current_dir <- getwd()
if (dir.exists(file.path(current_dir, "config")) && 
    dir.exists(file.path(current_dir, "src"))) {
  PROJECT_ROOT <- current_dir
} else {
  PROJECT_ROOT <- dirname(current_dir)
}

source(file.path(PROJECT_ROOT, "config/pipeline_config.R"))

# ==============================================================================
# Fix 1: deals_filing_matched.rds
# ==============================================================================

cat("\n--- Fixing deals_filing_matched.rds ---\n")

if (!file.exists(PATHS$deals_filing_matched)) {
  cat("  File not found, skipping.\n")
} else {
  deals <- readRDS(PATHS$deals_filing_matched)
  n_before <- nrow(deals)
  
  cat(glue("  Input rows: {n_before}\n"))
  
  # Check if logical_deal_id exists; if not, create it
  if (!"logical_deal_id" %in% names(deals)) {
    deals <- deals %>%
      mutate(logical_deal_id = paste(target_name, as.character(date_announced), sep = "___"))
    cat("  Created logical_deal_id column\n")
  }
  
  n_unique_deals <- n_distinct(deals$logical_deal_id)
  cat(glue("  Unique logical deals: {n_unique_deals}\n"))
  
  if (n_before > n_unique_deals) {
    cat(glue("  Found {n_before - n_unique_deals} duplicate CIK candidate rows\n"))
    
    # Handle days_lag vs days_before
    if ("days_before" %in% names(deals) && !"days_lag" %in% names(deals)) {
      deals <- deals %>% rename(days_lag = days_before)
    }
    
    # Deduplicate: keep best row per deal
    deals_fixed <- deals %>%
      group_by(logical_deal_id) %>%
      arrange(
        desc(match_status == "matched"),  # Matched first
        days_lag                          # Shortest lag (most recent filing)
      ) %>%
      slice(1) %>%
      ungroup()
    
    n_after <- nrow(deals_fixed)
    cat(glue("  After deduplication: {n_after} rows\n"))
    
    # Validate
    stopifnot(n_after == n_unique_deals)
    
    # Backup and save
    backup_path <- paste0(PATHS$deals_filing_matched, ".backup_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    file.copy(PATHS$deals_filing_matched, backup_path)
    cat(glue("  Backup saved: {backup_path}\n"))
    
    saveRDS(deals_fixed, PATHS$deals_filing_matched)
    cat(glue("  ✓ Fixed and saved: {PATHS$deals_filing_matched}\n"))
    
  } else {
    cat("  ✓ No duplicates found, file is OK\n")
  }
}

# ==============================================================================
# Fix 2: parsed_sections.rds
# ==============================================================================

cat("\n--- Fixing parsed_sections.rds ---\n")

if (!file.exists(PATHS$parsed_sections)) {
  cat("  File not found, skipping.\n")
} else {
  parsed <- readRDS(PATHS$parsed_sections)
  n_before <- nrow(parsed)
  n_unique <- n_distinct(parsed$accession_number)
  
  cat(glue("  Input rows: {n_before}\n"))
  cat(glue("  Unique accession_numbers: {n_unique}\n"))
  
  if (n_before > n_unique) {
    cat(glue("  Found {n_before - n_unique} duplicate rows\n"))
    
    # Deduplicate: keep latest extraction per accession
    parsed_fixed <- parsed %>%
      group_by(accession_number) %>%
      arrange(desc(extracted_at)) %>%
      slice(1) %>%
      ungroup()
    
    n_after <- nrow(parsed_fixed)
    cat(glue("  After deduplication: {n_after} rows\n"))
    
    # Validate
    stopifnot(n_after == n_unique)
    
    # Backup and save
    backup_path <- paste0(PATHS$parsed_sections, ".backup_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    file.copy(PATHS$parsed_sections, backup_path)
    cat(glue("  Backup saved: {backup_path}\n"))
    
    saveRDS(parsed_fixed, PATHS$parsed_sections)
    cat(glue("  ✓ Fixed and saved: {PATHS$parsed_sections}\n"))
    
  } else {
    cat("  ✓ No duplicates found, file is OK\n")
  }
}

# ==============================================================================
# Summary
# ==============================================================================

cat("\n==============================================================================\n")
cat("FIX COMPLETE\n")
cat("==============================================================================\n")
cat("\nYou can now run 06_merge_final_dataset.R to create the final dataset.\n")
cat("\nIf you need to re-run the full pipeline from scratch, use this order:\n")
cat("  1. 01_ingest_clean_deals.R\n")
cat("  2. 02_ingest_build_edgar_index.R\n")
cat("  3. 03_merge_match_deals_to_filings.R\n")
cat("  4. 04_ingest_download_10k_filings.R\n")
cat("  5. 05_nlp_parse_sections.R\n")
cat("  6. 06_merge_final_dataset.R\n")
cat("\n")
