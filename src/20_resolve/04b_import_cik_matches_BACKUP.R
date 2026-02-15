# =============================================================================
# src/20_resolve/04b_import_cik_matches.R
# =============================================================================
# Import manually reviewed CIK matches from Excel
#
# This script imports the Excel file with CIK matches (after manual review)
# and saves it as RDS for the filing identification step.
#
# Note: Keeps ALL CIK matches (including multiples per target). The filing
# identification step will determine which CIK has valid 10-K filings.
#
# Input:  data/interim/deals_with_cik_matches.xlsx (manually reviewed)
# Output: data/interim/deals_with_cik.rds
# =============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(glue)
})

cat("\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("IMPORT CIK MATCHES FROM EXCEL\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("\n")

# =============================================================================
# STEP 1: Load Excel file
# =============================================================================

input_file <- "data/interim/deals_with_cik_matches.xlsx"

if (!file.exists(input_file)) {
  stop(glue("Input file not found: {input_file}"))
}

cat(glue("Reading: {input_file}"), "\n")
df <- read_xlsx(input_file)

cat(glue("  Rows: {nrow(df)}"), "\n")
cat(glue("  Columns: {ncol(df)}"), "\n")

# =============================================================================
# STEP 2: Validate and clean
# =============================================================================

cat("\n")
cat("Validation:\n")

# Check required columns (target_cik is the CIK column)
required_cols <- c("deal_id", "target_name", "target_cik")
missing_cols <- setdiff(required_cols, names(df))
if (length(missing_cols) > 0) {
  stop(glue("Missing required columns: {paste(missing_cols, collapse = ', ')}"))
}
cat("  ✓ Required columns present\n")

# Check CIK coverage
n_total <- nrow(df)
n_with_cik <- sum(!is.na(df$target_cik))
n_without_cik <- n_total - n_with_cik

cat(glue("  Rows with CIK: {n_with_cik} ({round(100*n_with_cik/n_total,1)}%)"), "\n")
cat(glue("  Rows without CIK: {n_without_cik} ({round(100*n_without_cik/n_total,1)}%)"), "\n")

# Unique targets
n_unique_targets <- length(unique(df$target_name))
n_unique_deals <- length(unique(df$deal_id))
cat(glue("  Unique targets: {n_unique_targets}"), "\n")
cat(glue("  Unique deals: {n_unique_deals}"), "\n")

# Multiple CIKs per deal
cik_per_deal <- df %>%
  filter(!is.na(target_cik)) %>%
  group_by(deal_id) %>%
  summarise(n_cik = n_distinct(target_cik), .groups = "drop")

n_multi_cik <- sum(cik_per_deal$n_cik > 1)
cat(glue("  Deals with multiple CIKs: {n_multi_cik}"), "\n")

# =============================================================================
# STEP 3: Standardize CIK format
# =============================================================================

cat("\n")
cat("Standardizing CIK format...\n")

df <- df %>%
  mutate(
    # Ensure CIK is character and zero-padded to 10 digits
    target_cik = ifelse(
      is.na(target_cik),
      NA_character_,
      sprintf("%010.0f", as.numeric(target_cik))
    )
  )

cat("  ✓ CIK zero-padded to 10 digits\n")

# Show sample
cat("\n")
cat("Sample CIK values:\n")
sample_ciks <- df %>% 
  filter(!is.na(target_cik)) %>% 
  select(target_name, target_cik) %>% 
  head(5)
print(sample_ciks)

# =============================================================================
# STEP 4: Save as RDS
# =============================================================================

output_file <- "data/interim/deals_with_cik.rds"

saveRDS(df, output_file)

cat("\n")
cat(glue("✓ Saved: {output_file}"), "\n")
cat(glue("  Rows: {nrow(df)}"), "\n")
cat(glue("  Ready for filing identification"), "\n")

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("IMPORT COMPLETE\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat(glue("Total rows: {nrow(df)}"), "\n")
cat(glue("Rows with CIK: {n_with_cik} ({round(100*n_with_cik/n_total,1)}%)"), "\n")
cat(glue("Next step: source('src/20_resolve/05_filing_identify.R')"), "\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("\n")
