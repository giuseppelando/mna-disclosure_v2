# ==============================================================================
# MODULE 1: TEXT CLEANING FOR M&A DISCLOSURE ANALYSIS
# ==============================================================================
# Purpose: Clean EDGAR 10-K text (MD&A and Risk Factors) while preserving
#          financial information (numbers, percentages, currency symbols)
# Input: deals_with_10k_text_analysis.rds
# Output: data/interim/cleaned_text.rds + cleaning report
# Author: M&A Disclosure Project
# Date: 2025-02-03
# ==============================================================================

# SETUP ------------------------------------------------------------------------

# Load required libraries
suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(readr)
})

# Set paths (adjust if needed)
path_input <- "C:/Users/giuse/Documents/GitHub/mna-disclosure/data/processed/deals_with_10k_text_analysis.rds"
path_output_interim <- "C:/Users/giuse/Documents/GitHub/mna-disclosure/data/interim"
path_output_reports <- "C:/Users/giuse/Documents/GitHub/mna-disclosure/reports"

# Create output directories if they don't exist
dir.create(path_output_interim, showWarnings = FALSE, recursive = TRUE)
dir.create(path_output_reports, showWarnings = FALSE, recursive = TRUE)

cat("=== MODULE 1: TEXT CLEANING ===\n")
cat("Starting text cleaning pipeline...\n\n")


# BLOCK 0: DATA LOADING AND VALIDATION ----------------------------------------

cat("BLOCK 0: Loading and validating data...\n")

# Read raw data
deals_raw <- readRDS(path_input)

# Validate required columns
required_cols <- c("deal_id", "mda_text", "risk_factors_text")
missing_cols <- setdiff(required_cols, names(deals_raw))

if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

# Check for optional columns (for later use)
optional_cols <- c("target_primary_sic", "year_announced", 
                   "mda_word_count", "risk_word_count")
available_optional <- intersect(optional_cols, names(deals_raw))

cat(sprintf("  - Loaded %d deals\n", nrow(deals_raw)))
cat(sprintf("  - Required columns present: %s\n", 
            paste(required_cols, collapse = ", ")))
if (length(available_optional) > 0) {
  cat(sprintf("  - Optional columns available: %s\n", 
              paste(available_optional, collapse = ", ")))
}
cat("\n")


# BLOCK 1: EDGAR-AWARE CLEANING FUNCTIONS -------------------------------------

cat("BLOCK 1: Defining EDGAR-aware cleaning functions...\n")

#' Clean EDGAR Text
#' 
#' Removes HTML/XBRL artifacts and noise while preserving financial information
#' 
#' @param x Character vector of raw text
#' @return Character vector of cleaned text
#' @details
#' Preserves: numbers, percentages, currency symbols, dates, hyphens in compounds
#' Removes: HTML tags, entities, EDGAR artifacts, excessive whitespace
clean_edgar_text <- function(x) {
  
  # Handle NA and empty strings
  if (is.null(x) || length(x) == 0) {
    return(character(0))
  }
  
  # Replace NA with empty string (preserve as valid but empty)
  x[is.na(x)] <- ""
  
  # Step 1: Encoding normalization
  # Convert to UTF-8, replace invalid bytes with space
  x <- iconv(x, from = "", to = "UTF-8", sub = " ")
  
  # Step 2: Remove HTML tags (non-greedy)
  # Pattern matches <anything> but not greedy (shortest match)
  x <- str_replace_all(x, "<[^>]+>", " ")
  
  # Step 3: Decode HTML entities
  # Common entities first (specific replacements)
  x <- str_replace_all(x, "&nbsp;", " ")
  x <- str_replace_all(x, "&#160;", " ")
  x <- str_replace_all(x, "&amp;", "&")
  x <- str_replace_all(x, "&quot;", '"')
  x <- str_replace_all(x, "&apos;", "'")
  x <- str_replace_all(x, "&#8217;", "'")  # Right single quote
  x <- str_replace_all(x, "&#8220;", '"')  # Left double quote
  x <- str_replace_all(x, "&#8221;", '"')  # Right double quote
  x <- str_replace_all(x, "&#8211;", "-")  # En dash
  x <- str_replace_all(x, "&#8212;", "--") # Em dash
  x <- str_replace_all(x, "&lt;", "<")
  x <- str_replace_all(x, "&gt;", ">")
  
  # Remove remaining HTML entities (generic cleanup)
  x <- str_replace_all(x, "&[a-zA-Z]+;", " ")     # Named entities
  x <- str_replace_all(x, "&#\\d+;", " ")         # Numeric entities
  x <- str_replace_all(x, "&#x[0-9a-fA-F]+;", " ") # Hex entities
  
  # Step 4: Remove EDGAR structural artifacts
  # Remove lines of underscores/dashes (table borders)
  x <- str_replace_all(x, "_{3,}", " ")
  x <- str_replace_all(x, "-{5,}", " ")
  x <- str_replace_all(x, "={3,}", " ")
  
  # Remove EDGAR page markers (e.g., "Page 23")
  x <- str_replace_all(x, "\\bPage\\s+\\d+\\b", " ")
  
  # Remove table of contents artifacts (e.g., "..... 23")
  x <- str_replace_all(x, "\\.{3,}\\s*\\d+", " ")
  
  # Step 5: Lowercase conversion
  # Apply after entity decoding to preserve acronyms in pattern matching if needed
  x <- tolower(x)
  
  # Step 6: Normalize whitespace (CRITICAL for consistency)
  # Replace all whitespace variants with single space
  x <- str_replace_all(x, "[\\r\\n\\t]+", " ")  # Newlines and tabs to space
  x <- str_replace_all(x, "\\s{2,}", " ")       # Collapse multiple spaces
  x <- str_trim(x)                              # Trim leading/trailing space
  
  return(x)
}


#' Remove Conservative Boilerplate
#' 
#' Removes only highly standardized, repetitive legal text
#' 
#' @param x Character vector of cleaned text
#' @return Character vector with boilerplate removed
#' @details
#' Only removes text with very high confidence (strict patterns).
#' Does NOT remove substantive disclosure, even if formulaic.
remove_boilerplate_conservative <- function(x) {
  
  # Handle NA and empty strings
  if (is.null(x) || length(x) == 0) {
    return(character(0))
  }
  
  x[is.na(x)] <- ""
  
  # Pattern 1: Forward-Looking Statements disclaimer
  # Common pattern: starts with "forward-looking" and ends with regulatory cite
  # Use conservative boundary detection
  pattern_fls <- paste0(
    "(?i)",  # Case insensitive
    "\\bforward[- ]looking statements?\\b",  # Start marker
    ".{0,2000}?",  # Content (non-greedy, max 2000 chars)
    "(?:",  # Non-capturing group for end markers
      "\\bsecurities act\\b|",
      "\\bexchange act\\b|",
      "\\bprivate securities litigation reform act\\b|",
      "\\b1995\\b.*?\\bcaution\\b|",
      "section 21e.*?exchange act|",
      "actual results.*?differ materially",
    ")"
  )
  x <- str_replace_all(x, pattern_fls, " ")
  
  # Pattern 2: Website access to reports (standard SEC requirement)
  pattern_web <- paste0(
    "(?i)",
    "\\bwebsite access to\\b",
    ".{0,500}?",
    "\\bsec\\.gov\\b"
  )
  x <- str_replace_all(x, pattern_web, " ")
  
  # Pattern 3: "Available Information" boilerplate
  pattern_avail <- paste0(
    "(?i)",
    "\\bavailable information\\b",
    ".{0,400}?",
    "(?:free of charge|without charge|investor relations)"
  )
  x <- str_replace_all(x, pattern_avail, " ")
  
  # Pattern 4: EDGAR filing header artifacts (if any remain)
  # Example: "UNITED STATES SECURITIES AND EXCHANGE COMMISSION"
  x <- str_replace_all(x, 
    "(?i)united states securities and exchange commission.{0,200}?washington", 
    " ")
  
  # Re-normalize whitespace after removals
  x <- str_replace_all(x, "\\s{2,}", " ")
  x <- str_trim(x)
  
  return(x)
}


#' Count Words (Basic Tokenization)
#' 
#' Count words using simple whitespace tokenization
#' 
#' @param x Character vector of cleaned text
#' @return Numeric vector of word counts
#' @details
#' Uses whitespace separation. Assumes text is already cleaned and normalized.
#' Consistent with denominator needed for "per 1000 words" metrics.
count_words_basic <- function(x) {
  
  # Handle NA and empty strings
  if (is.null(x) || length(x) == 0) {
    return(numeric(0))
  }
  
  # Replace NA with empty string for counting
  x[is.na(x)] <- ""
  
  # Split on whitespace and count non-empty tokens
  word_counts <- sapply(x, function(text) {
    if (nchar(text) == 0) {
      return(0L)
    }
    tokens <- str_split(text, "\\s+")[[1]]
    tokens <- tokens[nchar(tokens) > 0]  # Remove empty tokens
    return(length(tokens))
  }, USE.NAMES = FALSE)
  
  return(as.integer(word_counts))
}

cat("  - Cleaning functions defined\n")
cat("  - Functions: clean_edgar_text(), remove_boilerplate_conservative(), count_words_basic()\n\n")


# BLOCK 3: APPLY CLEANING TO BOTH SECTIONS ------------------------------------

cat("BLOCK 3: Applying cleaning to MD&A and Risk Factors...\n")

# Create cleaned columns
deals_cleaned <- deals_raw %>%
  mutate(
    # Clean MD&A
    mda_clean = clean_edgar_text(mda_text),
    mda_clean = remove_boilerplate_conservative(mda_clean),
    
    # Clean Risk Factors
    risk_clean = clean_edgar_text(risk_factors_text),
    risk_clean = remove_boilerplate_conservative(risk_clean)
  )

cat("  - MD&A cleaning complete\n")
cat("  - Risk Factors cleaning complete\n\n")


# BLOCK 4: WORD COUNTS AND QUALITY CHECKS -------------------------------------

cat("BLOCK 4: Computing word counts and quality checks...\n")

# Compute clean word counts
deals_cleaned <- deals_cleaned %>%
  mutate(
    mda_word_count_clean = count_words_basic(mda_clean),
    risk_word_count_clean = count_words_basic(risk_clean)
  )

# Quality checks: create summary statistics
cleaning_summary <- list()

# 1. Basic counts
cleaning_summary$n_deals <- nrow(deals_cleaned)
cleaning_summary$n_mda_empty <- sum(deals_cleaned$mda_word_count_clean == 0)
cleaning_summary$n_risk_empty <- sum(deals_cleaned$risk_word_count_clean == 0)

# 2. Word count distributions (clean)
cleaning_summary$mda_wordcount_stats <- summary(deals_cleaned$mda_word_count_clean)
cleaning_summary$risk_wordcount_stats <- summary(deals_cleaned$risk_word_count_clean)

# 3. Reduction ratios (if raw counts available)
if ("mda_word_count" %in% names(deals_cleaned)) {
  deals_cleaned <- deals_cleaned %>%
    mutate(mda_reduction_ratio = mda_word_count_clean / 
             ifelse(mda_word_count > 0, mda_word_count, NA_real_))
  
  cleaning_summary$mda_reduction_stats <- summary(deals_cleaned$mda_reduction_ratio)
  
  # Flag extreme reductions (>70% reduction)
  cleaning_summary$n_mda_extreme_reduction <- sum(
    deals_cleaned$mda_reduction_ratio < 0.3, 
    na.rm = TRUE
  )
}

if ("risk_word_count" %in% names(deals_cleaned)) {
  deals_cleaned <- deals_cleaned %>%
    mutate(risk_reduction_ratio = risk_word_count_clean / 
             ifelse(risk_word_count > 0, risk_word_count, NA_real_))
  
  cleaning_summary$risk_reduction_stats <- summary(deals_cleaned$risk_reduction_ratio)
  
  cleaning_summary$n_risk_extreme_reduction <- sum(
    deals_cleaned$risk_reduction_ratio < 0.3, 
    na.rm = TRUE
  )
}

# 4. Check preservation of financial patterns (sample-based)
# Sample 100 random non-empty documents
set.seed(42)
sample_idx <- which(deals_cleaned$mda_word_count_clean > 100)
if (length(sample_idx) > 100) {
  sample_idx <- sample(sample_idx, 100)
}

if (length(sample_idx) > 0) {
  sample_texts <- deals_cleaned$mda_clean[sample_idx]
  
  # Check for numbers, percentages, currency
  cleaning_summary$sample_n <- length(sample_idx)
  cleaning_summary$pct_with_numbers <- mean(str_detect(sample_texts, "\\d")) * 100
  cleaning_summary$pct_with_percentages <- mean(str_detect(sample_texts, "\\d+%")) * 100
  cleaning_summary$pct_with_currency <- mean(str_detect(sample_texts, "\\$\\d")) * 100
  cleaning_summary$pct_with_decimals <- mean(str_detect(sample_texts, "\\d+\\.\\d+")) * 100
}

cat("  - Word counts computed\n")
cat("  - Quality checks completed\n\n")


# VALIDATION OUTPUT ------------------------------------------------------------

cat("=== CLEANING VALIDATION SUMMARY ===\n\n")

cat("Basic Counts:\n")
cat(sprintf("  Total deals: %d\n", cleaning_summary$n_deals))
cat(sprintf("  MD&A empty after cleaning: %d (%.1f%%)\n", 
            cleaning_summary$n_mda_empty,
            100 * cleaning_summary$n_mda_empty / cleaning_summary$n_deals))
cat(sprintf("  Risk Factors empty after cleaning: %d (%.1f%%)\n\n", 
            cleaning_summary$n_risk_empty,
            100 * cleaning_summary$n_risk_empty / cleaning_summary$n_deals))

cat("MD&A Word Count Distribution (clean):\n")
print(cleaning_summary$mda_wordcount_stats)
cat("\n")

cat("Risk Factors Word Count Distribution (clean):\n")
print(cleaning_summary$risk_wordcount_stats)
cat("\n")

if (!is.null(cleaning_summary$mda_reduction_stats)) {
  cat("MD&A Reduction Ratio (clean/raw):\n")
  print(cleaning_summary$mda_reduction_stats)
  cat(sprintf("  Deals with >70%% reduction: %d\n", 
              cleaning_summary$n_mda_extreme_reduction))
  cat("\n")
}

if (!is.null(cleaning_summary$risk_reduction_stats)) {
  cat("Risk Factors Reduction Ratio (clean/raw):\n")
  print(cleaning_summary$risk_reduction_stats)
  cat(sprintf("  Deals with >70%% reduction: %d\n", 
              cleaning_summary$n_risk_extreme_reduction))
  cat("\n")
}

if (!is.null(cleaning_summary$sample_n)) {
  cat(sprintf("Financial Pattern Preservation (n=%d sample):\n", 
              cleaning_summary$sample_n))
  cat(sprintf("  Documents with numbers: %.1f%%\n", 
              cleaning_summary$pct_with_numbers))
  cat(sprintf("  Documents with percentages: %.1f%%\n", 
              cleaning_summary$pct_with_percentages))
  cat(sprintf("  Documents with currency symbols: %.1f%%\n", 
              cleaning_summary$pct_with_currency))
  cat(sprintf("  Documents with decimals: %.1f%%\n", 
              cleaning_summary$pct_with_decimals))
  cat("\n")
}


# BLOCK 5: SAVE INTERIM OUTPUTS -----------------------------------------------

cat("BLOCK 5: Saving interim outputs...\n")

# Select columns for interim dataset
cols_to_keep <- c(
  "deal_id",
  "mda_clean", "risk_clean",
  "mda_word_count_clean", "risk_word_count_clean"
)

# Add optional columns if available (needed for later normalization)
if ("target_primary_sic" %in% names(deals_cleaned)) {
  cols_to_keep <- c(cols_to_keep, "target_primary_sic")
}
if ("year_announced" %in% names(deals_cleaned)) {
  cols_to_keep <- c(cols_to_keep, "year_announced")
}

# Keep reduction ratios if computed (for diagnostics)
if ("mda_reduction_ratio" %in% names(deals_cleaned)) {
  cols_to_keep <- c(cols_to_keep, "mda_reduction_ratio", "risk_reduction_ratio")
}

# Create interim dataset
deals_interim <- deals_cleaned %>%
  select(all_of(cols_to_keep))

# Save as RDS
output_file <- file.path(path_output_interim, "cleaned_text.rds")
saveRDS(deals_interim, output_file)
cat(sprintf("  - Saved: %s\n", output_file))

# Save cleaning summary as RDS (for later reference)
summary_file <- file.path(path_output_interim, "cleaning_summary.rds")
saveRDS(cleaning_summary, summary_file)
cat(sprintf("  - Saved: %s\n", summary_file))

# Create human-readable report
report_lines <- c(
  "# TEXT CLEANING REPORT",
  paste("Generated:", Sys.time()),
  "",
  "## Input",
  paste("- File:", path_input),
  paste("- Deals:", nrow(deals_raw)),
  "",
  "## Cleaning Operations",
  "1. HTML/XBRL artifact removal",
  "2. Entity decoding",
  "3. Conservative boilerplate removal",
  "4. Whitespace normalization",
  "5. Lowercase conversion",
  "",
  "## Output Summary",
  paste("- MD&A empty:", cleaning_summary$n_mda_empty, 
        sprintf("(%.1f%%)", 100 * cleaning_summary$n_mda_empty / cleaning_summary$n_deals)),
  paste("- Risk Factors empty:", cleaning_summary$n_risk_empty,
        sprintf("(%.1f%%)", 100 * cleaning_summary$n_risk_empty / cleaning_summary$n_deals)),
  "",
  "## Word Count Statistics (Clean)",
  "### MD&A:",
  paste("- Min:", cleaning_summary$mda_wordcount_stats[1]),
  paste("- Median:", cleaning_summary$mda_wordcount_stats[3]),
  paste("- Mean:", round(cleaning_summary$mda_wordcount_stats[4], 1)),
  paste("- Max:", cleaning_summary$mda_wordcount_stats[6]),
  "",
  "### Risk Factors:",
  paste("- Min:", cleaning_summary$risk_wordcount_stats[1]),
  paste("- Median:", cleaning_summary$risk_wordcount_stats[3]),
  paste("- Mean:", round(cleaning_summary$risk_wordcount_stats[4], 1)),
  paste("- Max:", cleaning_summary$risk_wordcount_stats[6]),
  ""
)

if (!is.null(cleaning_summary$mda_reduction_stats)) {
  report_lines <- c(report_lines,
    "## Reduction Ratios (clean/raw)",
    "### MD&A:",
    paste("- Median reduction ratio:", 
          round(cleaning_summary$mda_reduction_stats[3], 3)),
    paste("- Extreme reductions (>70%):", 
          cleaning_summary$n_mda_extreme_reduction),
    ""
  )
}

if (!is.null(cleaning_summary$sample_n)) {
  report_lines <- c(report_lines,
    "## Financial Pattern Preservation",
    paste("Sample size:", cleaning_summary$sample_n),
    paste("- With numbers:", sprintf("%.1f%%", cleaning_summary$pct_with_numbers)),
    paste("- With percentages:", sprintf("%.1f%%", cleaning_summary$pct_with_percentages)),
    paste("- With currency:", sprintf("%.1f%%", cleaning_summary$pct_with_currency)),
    paste("- With decimals:", sprintf("%.1f%%", cleaning_summary$pct_with_decimals)),
    ""
  )
}

report_lines <- c(report_lines,
  "## Validation Notes",
  "- Check that median reduction ratio is between 0.7-0.95 (not too aggressive)",
  "- Verify that >95% of sample documents retain financial patterns",
  "- Empty documents should be flagged but not dropped (preserve sample size)",
  "",
  "## Next Steps",
  "- Proceed to Module 2: Tokenization and DTM construction",
  "- Use mda_word_count_clean and risk_word_count_clean as denominators",
  ""
)

report_file <- file.path(path_output_reports, "01_cleaning_report.md")
writeLines(report_lines, report_file)
cat(sprintf("  - Saved: %s\n", report_file))

cat("\n=== MODULE 1 COMPLETE ===\n")
cat(sprintf("Cleaned text saved to: %s\n", output_file))
cat(sprintf("Report saved to: %s\n", report_file))
cat("\nReady for Module 2: Tokenization\n")
