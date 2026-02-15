# =============================================================================
# src/20_resolve/04b_import_cik_matches.R (FIXED VERSION)
# =============================================================================
# Import manually reviewed CIK matches from Excel
# 
# FIXES:
# 1. Preserves number_of_days column as integer
# 2. Properly handles all column types from Excel
# 3. Creates withdrawn flag if not present
# =============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(glue)
})

cat("\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("IMPORT CIK MATCHES FROM EXCEL (FIXED)\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("\n")

# =============================================================================
# STEP 1: Load Excel file with explicit column types
# =============================================================================

input_file <- "data/interim/deals_with_cik_matches.xlsx"

if (!file.exists(input_file)) {
  stop(glue("Input file not found: {input_file}"))
}

cat(glue("Reading: {input_file}"), "\n")

# Read Excel - readxl may incorrectly parse some columns
df <- read_xlsx(input_file)

cat(glue("  Rows: {nrow(df)}"), "\n")
cat(glue("  Columns: {ncol(df)}"), "\n")

# =============================================================================
# STEP 2: Fix column types
# =============================================================================

cat("\n")
cat("Fixing column types...\n")

# SDC Missing Value Code
SDC_MISSING_VALUE <- -999L

# Find and fix number_of_days columns
days_cols <- names(df)[str_detect(names(df), "number_of_days|days_between")]

for (col in days_cols) {
  before_class <- paste(class(df[[col]]), collapse = "/")
  
  # If it got converted to Date, convert back
  if (inherits(df[[col]], c("Date", "POSIXt"))) {
    # This is the fix: if readxl converted to Date, it used Excel origin
    df[[col]] <- as.integer(as.Date(df[[col]]) - as.Date("1899-12-30"))
    cat(glue("  ✓ Fixed {col}: {before_class} -> integer\n"))
  } else if (is.numeric(df[[col]])) {
    # Already numeric, just ensure integer
    df[[col]] <- as.integer(round(df[[col]]))
    cat(glue("  ✓ {col}: preserved as integer\n"))
  }
  
  # Convert SDC missing to NA
  df[[col]][df[[col]] == SDC_MISSING_VALUE] <- NA_integer_
  
  # Show sample
  cat(glue("    Sample: {paste(head(df[[col]], 5), collapse = ', ')}\n"))
}

# Ensure withdrawn flag exists
if (!"withdrawn" %in% names(df)) {
  cat("\nCreating withdrawn flag...\n")
  
  if ("deal_status" %in% names(df)) {
    df <- df %>%
      mutate(
        withdrawn = case_when(
          str_to_lower(str_trim(deal_status)) == "withdrawn" ~ 1L,
          str_to_lower(str_trim(deal_status)) == "completed" ~ 0L,
          TRUE ~ NA_integer_
        )
      )
    cat(glue("  ✓ Created withdrawn from deal_status\n"))
  } else if ("deal_completed" %in% names(df)) {
    df <- df %>%
      mutate(
        withdrawn = case_when(
          deal_completed == 0 ~ 1L,
          deal_completed == 1 ~ 0L,
          TRUE ~ NA_integer_
        )
      )
    cat(glue("  ✓ Created withdrawn from deal_completed\n"))
  }
}

# =============================================================================
# STEP 3: Validate and clean
# =============================================================================

cat("\n")
cat("Validation:\n")

# Check required columns
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

# Withdrawn flag summary
if ("withdrawn" %in% names(df)) {
  n_withdrawn <- sum(df$withdrawn == 1, na.rm = TRUE)
  n_completed <- sum(df$withdrawn == 0, na.rm = TRUE)
  cat(glue("  Withdrawn: {n_withdrawn}"), "\n")
  cat(glue("  Completed: {n_completed}"), "\n")
}

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
# STEP 4: Standardize CIK format
# =============================================================================

cat("\n")
cat("Standardizing CIK format...\n")

df <- df %>%
  mutate(
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
# STEP 5: Save as RDS
# =============================================================================

output_file <- "data/interim/deals_with_cik.rds"
saveRDS(df, output_file)

cat("\n")
cat(glue("✓ Saved: {output_file}"), "\n")
cat(glue("  Rows: {nrow(df)}"), "\n")

# =============================================================================
# STEP 6: Verify saved file
# =============================================================================

cat("\n")
cat("Verifying saved file...\n")

df_check <- readRDS(output_file)

# Check days column
days_cols_check <- names(df_check)[str_detect(names(df_check), "number_of_days")]
for (col in days_cols_check) {
  cat(glue("  {col}: {paste(class(df_check[[col]]), collapse = '/')}\n"))
  cat(glue("    Sample: {paste(head(df_check[[col]], 5), collapse = ', ')}\n"))
}

if ("withdrawn" %in% names(df_check)) {
  cat(glue("  withdrawn: {class(df_check$withdrawn)}\n"))
  tbl <- table(df_check$withdrawn, useNA = "always")
  cat(glue("    Completed (0): {tbl['0']}, Withdrawn (1): {tbl['1']}\n"))
}

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("IMPORT COMPLETE (FIXED)\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat(glue("Total rows: {nrow(df)}"), "\n")
cat(glue("Rows with CIK: {n_with_cik} ({round(100*n_with_cik/n_total,1)}%)"), "\n")
cat(glue("Next step: source('src/20_resolve/05_filing_identify.R')"), "\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("\n")
