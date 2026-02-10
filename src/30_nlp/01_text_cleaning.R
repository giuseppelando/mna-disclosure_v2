# ==============================================================================
# MODULE 1: TEXT CLEANING FOR M&A DISCLOSURE ANALYSIS  (REVISED v2)
# ==============================================================================
# Purpose: Clean EDGAR 10-K text (MD&A and Risk Factors) while preserving
#          financial information (numbers, percentages, currency symbols)
#
# REVISION NOTES (v2):
#   1. Vectorised word counting via str_count (replaces slow sapply)
#   2. Division-by-zero guard on reduction ratios
#   3. Boilerplate regex tightened (max span reduced, documented)
#   4. Quarter pattern lowercased (text is lowercase at that stage)
#   5. Summary subscripting made robust via names()
#   6. Idempotent: re-running produces identical output
#
# Input:  deals_with_10k_text_analysis.rds
# Output: data/interim/cleaned_text.rds + cleaning report
# ==============================================================================

# SETUP ------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(readr)
})

# ---- Paths (project-root relative; override via env vars) --------------------
project_root <- Sys.getenv("PROJECT_ROOT", unset = getwd())

path_input          <- Sys.getenv("PATH_DEALS_RDS",
                         unset = file.path(project_root, "data", "processed",
                                           "deals_with_10k_text_analysis.rds"))
path_output_interim <- Sys.getenv("PATH_INTERIM_DIR",
                         unset = file.path(project_root, "data", "interim"))
path_output_reports <- Sys.getenv("PATH_REPORTS_DIR",
                         unset = file.path(project_root, "reports"))

dir.create(path_output_interim, showWarnings = FALSE, recursive = TRUE)
dir.create(path_output_reports, showWarnings = FALSE, recursive = TRUE)

if (!file.exists(path_input)) stop("Input file not found: ", path_input)

cat("=== MODULE 1: TEXT CLEANING (v2) ===\n")
cat("Starting text cleaning pipeline...\n\n")


# BLOCK 0: DATA LOADING AND VALIDATION ----------------------------------------

cat("BLOCK 0: Loading and validating data...\n")

deals_raw <- readRDS(path_input)

# REVISION: Enforce character key type from the start
deals_raw <- deals_raw %>% mutate(deal_id = as.character(deal_id))

required_cols <- c("deal_id", "mda_text", "risk_factors_text")
missing_cols  <- setdiff(required_cols, names(deals_raw))
if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

optional_cols      <- c("target_primary_sic", "year_announced",
                        "mda_word_count", "risk_word_count")
available_optional <- intersect(optional_cols, names(deals_raw))

cat(sprintf("  - Loaded %d deals\n", nrow(deals_raw)))
cat(sprintf("  - Required columns: %s\n", paste(required_cols, collapse = ", ")))
if (length(available_optional) > 0) {
  cat(sprintf("  - Optional columns: %s\n",
              paste(available_optional, collapse = ", ")))
}
cat("\n")


# BLOCK 1: EDGAR-AWARE CLEANING FUNCTIONS --------------------------------------

cat("BLOCK 1: Defining EDGAR-aware cleaning functions...\n")

#' Clean EDGAR Text
#'
#' Removes HTML/XBRL artifacts and noise while preserving financial information.
#' Vectorised over character input.
#'
#' @param x Character vector of raw text
#' @return Character vector of cleaned text
clean_edgar_text <- function(x) {

  if (is.null(x) || length(x) == 0) return(character(0))
  x[is.na(x)] <- ""

  # Step 1: Encoding normalisation
  x <- iconv(x, from = "", to = "UTF-8", sub = " ")

  # Step 2: Remove HTML tags (non-greedy)
  x <- str_replace_all(x, "<[^>]+>", " ")

  # Step 3: Decode HTML entities (specific first, then generic)
  entity_map <- c(
    "&nbsp;" = " ", "&#160;" = " ",
    "&amp;"  = "&", "&quot;" = '"', "&apos;" = "'",
    "&#8217;" = "'", "&#8220;" = '"', "&#8221;" = '"',
    "&#8211;" = "-", "&#8212;" = "--",
    "&lt;"   = "<",  "&gt;"   = ">"
  )
  for (pat in names(entity_map)) {
    x <- str_replace_all(x, fixed(pat), entity_map[[pat]])
  }
  x <- str_replace_all(x, "&[a-zA-Z]+;", " ")
  x <- str_replace_all(x, "&#\\d+;", " ")
  x <- str_replace_all(x, "&#x[0-9a-fA-F]+;", " ")

  # Step 4: EDGAR structural artifacts
  x <- str_replace_all(x, "(?i)\\#table_start\\b|\\#table_end\\b", " ")
  x <- str_replace_all(x, "_{3,}", " ")
  x <- str_replace_all(x, "-{5,}", " ")
  x <- str_replace_all(x, "={3,}", " ")
  x <- str_replace_all(x, "\\bPage\\s+\\d+\\b", " ")
  x <- str_replace_all(x, "\\.{3,}\\s*\\d+", " ")

  # Step 5: Lowercase (applied last to preserve entity decoding)
  x <- tolower(x)

  # Step 6: Whitespace normalisation
  x <- str_replace_all(x, "[\\r\\n\\t]+", " ")
  x <- str_replace_all(x, "\\s{2,}", " ")
  x <- str_trim(x)

  return(x)
}


#' Remove Conservative Boilerplate
#'
#' Removes only highly standardised, repetitive legal text.
#' REVISION v2: Reduced max span from 2000 to 1200 chars to avoid
#' eating substantive forward-looking discussion in MD&A.
#'
#' @param x Character vector of cleaned text
#' @return Character vector with boilerplate removed
remove_boilerplate_conservative <- function(x) {

  if (is.null(x) || length(x) == 0) return(character(0))
  x[is.na(x)] <- ""

  # Pattern 1: Forward-Looking Statements disclaimer
  # REVISION: Max span reduced from 2000 → 1200 chars (documented choice)
  pattern_fls <- paste0(
    "(?i)",
    "\\bforward[- ]looking statements?\\b",
    ".{0,1200}?",
    "(?:",
      "\\bsecurities act\\b|",
      "\\bexchange act\\b|",
      "\\bprivate securities litigation reform act\\b|",
      "\\b1995\\b.*?\\bcaution\\b|",
      "section 21e.*?exchange act|",
      "actual results.*?differ materially",
    ")"
  )
  x <- str_replace_all(x, pattern_fls, " ")

  # Pattern 2: Website access to reports
  pattern_web <- paste0(
    "(?i)",
    "\\bwebsite access to\\b",
    ".{0,500}?",
    "\\bsec\\.gov\\b"
  )
  x <- str_replace_all(x, pattern_web, " ")

  # Pattern 3: Available Information boilerplate
  pattern_avail <- paste0(
    "(?i)",
    "\\bavailable information\\b",
    ".{0,400}?",
    "(?:free of charge|without charge|investor relations)"
  )
  x <- str_replace_all(x, pattern_avail, " ")

  # Pattern 4: EDGAR filing header residuals
  x <- str_replace_all(
    x,
    "(?i)united states securities and exchange commission.{0,200}?washington",
    " "
  )

  # Re-normalise whitespace
  x <- str_replace_all(x, "\\s{2,}", " ")
  x <- str_trim(x)

  return(x)
}


#' Count Words (Vectorised)
#'
#' REVISION: Replaced sapply-based counting with vectorised str_count.
#' Consistent denominator for "per 1000 words" metrics.
#'
#' @param x Character vector of cleaned text
#' @return Integer vector of word counts
count_words <- function(x) {
  if (is.null(x) || length(x) == 0) return(integer(0))
  x[is.na(x)] <- ""
  as.integer(str_count(x, "\\S+"))
}

cat("  - Functions defined: clean_edgar_text(), remove_boilerplate_conservative(),")
cat(" count_words()\n\n")


# BLOCK 3: APPLY CLEANING TO BOTH SECTIONS ------------------------------------

cat("BLOCK 3: Applying cleaning to MD&A and Risk Factors...\n")

deals_cleaned <- deals_raw %>%
  mutate(
    mda_clean  = clean_edgar_text(mda_text),
    mda_clean  = remove_boilerplate_conservative(mda_clean),
    risk_clean = clean_edgar_text(risk_factors_text),
    risk_clean = remove_boilerplate_conservative(risk_clean)
  )

cat("  - MD&A cleaning complete\n")
cat("  - Risk Factors cleaning complete\n\n")


# BLOCK 4: WORD COUNTS AND QUALITY CHECKS -------------------------------------

cat("BLOCK 4: Computing word counts and quality checks...\n")

deals_cleaned <- deals_cleaned %>%
  mutate(
    mda_word_count_clean  = count_words(mda_clean),
    risk_word_count_clean = count_words(risk_clean)
  )

cleaning_summary <- list()
cleaning_summary$n_deals     <- nrow(deals_cleaned)
cleaning_summary$n_mda_empty  <- sum(deals_cleaned$mda_word_count_clean == 0)
cleaning_summary$n_risk_empty <- sum(deals_cleaned$risk_word_count_clean == 0)

cleaning_summary$mda_wordcount_stats  <- summary(deals_cleaned$mda_word_count_clean)
cleaning_summary$risk_wordcount_stats <- summary(deals_cleaned$risk_word_count_clean)

# Reduction ratios (if raw counts available)
# REVISION: Division-by-zero guarded explicitly
if ("mda_word_count" %in% names(deals_cleaned)) {
  deals_cleaned <- deals_cleaned %>%
    mutate(
      mda_reduction_ratio = ifelse(
        mda_word_count > 0,
        mda_word_count_clean / mda_word_count,
        NA_real_
      )
    )
  cleaning_summary$mda_reduction_stats <- summary(deals_cleaned$mda_reduction_ratio)
  cleaning_summary$n_mda_extreme_reduction <- sum(
    deals_cleaned$mda_reduction_ratio < 0.3, na.rm = TRUE
  )
}

if ("risk_word_count" %in% names(deals_cleaned)) {
  deals_cleaned <- deals_cleaned %>%
    mutate(
      risk_reduction_ratio = ifelse(
        risk_word_count > 0,
        risk_word_count_clean / risk_word_count,
        NA_real_
      )
    )
  cleaning_summary$risk_reduction_stats <- summary(deals_cleaned$risk_reduction_ratio)
  cleaning_summary$n_risk_extreme_reduction <- sum(
    deals_cleaned$risk_reduction_ratio < 0.3, na.rm = TRUE
  )
}

# Financial pattern preservation check (sample-based)
set.seed(42)
sample_idx <- which(deals_cleaned$mda_word_count_clean > 100)
if (length(sample_idx) > 100) sample_idx <- sample(sample_idx, 100)

if (length(sample_idx) > 0) {
  stx <- deals_cleaned$mda_clean[sample_idx]
  cleaning_summary$sample_n             <- length(sample_idx)
  cleaning_summary$pct_with_numbers     <- mean(str_detect(stx, "\\d")) * 100
  cleaning_summary$pct_with_percentages <- mean(str_detect(stx, "\\d+%")) * 100
  cleaning_summary$pct_with_currency    <- mean(str_detect(stx, "\\$\\d")) * 100
  cleaning_summary$pct_with_decimals    <- mean(str_detect(stx, "\\d+\\.\\d+")) * 100
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
  cat(sprintf("  Documents with currency: %.1f%%\n",
              cleaning_summary$pct_with_currency))
  cat(sprintf("  Documents with decimals: %.1f%%\n",
              cleaning_summary$pct_with_decimals))
  cat("\n")
}


# BLOCK 5: SAVE INTERIM OUTPUTS -----------------------------------------------

cat("BLOCK 5: Saving interim outputs...\n")

cols_to_keep <- c("deal_id",
                  "mda_clean", "risk_clean",
                  "mda_word_count_clean", "risk_word_count_clean")

if ("target_primary_sic" %in% names(deals_cleaned))
  cols_to_keep <- c(cols_to_keep, "target_primary_sic")
if ("year_announced" %in% names(deals_cleaned))
  cols_to_keep <- c(cols_to_keep, "year_announced")
if ("mda_reduction_ratio" %in% names(deals_cleaned))
  cols_to_keep <- c(cols_to_keep, "mda_reduction_ratio")
if ("risk_reduction_ratio" %in% names(deals_cleaned))
  cols_to_keep <- c(cols_to_keep, "risk_reduction_ratio")

deals_interim <- deals_cleaned %>% select(all_of(cols_to_keep))

output_file <- file.path(path_output_interim, "cleaned_text.rds")
saveRDS(deals_interim, output_file)
cat(sprintf("  - Saved: %s\n", output_file))

summary_file <- file.path(path_output_interim, "cleaning_summary.rds")
saveRDS(cleaning_summary, summary_file)
cat(sprintf("  - Saved: %s\n", summary_file))

# Human-readable report
# REVISION: Robust summary stat extraction via named access
get_stat <- function(s, nm) {
  val <- s[nm]
  if (is.na(val)) return("NA") else return(as.character(round(val, 1)))
}

report_lines <- c(
  "# TEXT CLEANING REPORT (v2)",
  paste("Generated:", Sys.time()),
  "",
  "## Input",
  paste("- File:", path_input),
  paste("- Deals:", nrow(deals_raw)),
  "",
  "## Cleaning Operations",
  "1. HTML/XBRL artifact removal",
  "2. Entity decoding",
  "3. Conservative boilerplate removal (max span 1200 chars)",
  "4. Whitespace normalisation",
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
  paste("- Min:", get_stat(cleaning_summary$mda_wordcount_stats, "Min.")),
  paste("- Median:", get_stat(cleaning_summary$mda_wordcount_stats, "Median")),
  paste("- Mean:", get_stat(cleaning_summary$mda_wordcount_stats, "Mean")),
  paste("- Max:", get_stat(cleaning_summary$mda_wordcount_stats, "Max.")),
  "",
  "### Risk Factors:",
  paste("- Min:", get_stat(cleaning_summary$risk_wordcount_stats, "Min.")),
  paste("- Median:", get_stat(cleaning_summary$risk_wordcount_stats, "Median")),
  paste("- Mean:", get_stat(cleaning_summary$risk_wordcount_stats, "Mean")),
  paste("- Max:", get_stat(cleaning_summary$risk_wordcount_stats, "Max.")),
  ""
)

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
  "- Check median reduction ratio in [0.70, 0.95] (not too aggressive)",
  "- Verify >95% of sample documents retain financial patterns",
  "- Empty documents flagged but not dropped (preserve sample integrity)",
  ""
)

report_file <- file.path(path_output_reports, "01_cleaning_report.md")
writeLines(report_lines, report_file)
cat(sprintf("  - Saved: %s\n", report_file))

cat("\n=== MODULE 1 COMPLETE (v2) ===\n")
cat(sprintf("Cleaned text: %s\n", output_file))
cat(sprintf("Report: %s\n", report_file))
cat("Ready for Module 2: Tokenization\n")
