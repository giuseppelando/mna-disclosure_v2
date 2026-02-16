# ==============================================================================
# MODULE 1: TEXT CLEANING FOR M&A DISCLOSURE ANALYSIS  (REVISED v3)
# ==============================================================================
# Purpose: Clean EDGAR 10-K text (MD&A and Risk Factors) while preserving
#          financial information (numbers, percentages, currency symbols)
#
# REVISION NOTES (v3 — feedback-driven fixes):
#   1. DUAL WORD COUNTS: introduces wc_alpha (alphabetic tokens only) alongside
#      wc_total (all non-whitespace tokens). This resolves the denominator
#      mismatch identified in the feedback: dictionary counts operate on
#      alphabetic tokens (numbers removed at tokenization), so densities
#      must divide by wc_alpha, not wc_total.
#   2. wc_total is retained for numeric-density calculations (where the
#      denominator should include all tokens).
#   3. Boilerplate removal unchanged from v2 (conservative, documented).
#   4. Lowercasing unchanged (feedback acknowledged but cost/benefit
#      unfavourable for a unigram/bigram pipeline).
#
# Input:  deals_with_10k_text_analysis.rds
# Output: data/interim/cleaned_text.rds + cleaning report
#
# IMPORTANT: Downstream modules (02 v4, 03 v6) MUST use wc_alpha for
#            dictionary-based densities and wc_total for numeric densities.
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

cat("=== MODULE 1: TEXT CLEANING (v3) ===\n")
cat("Starting text cleaning pipeline...\n\n")


# BLOCK 0: DATA LOADING AND VALIDATION ----------------------------------------

cat("BLOCK 0: Loading and validating data...\n")

deals_raw <- readRDS(path_input)

# Enforce character key type from the start
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

#' Clean EDGAR Text (unchanged from v2)
clean_edgar_text <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  x[is.na(x)] <- ""

  x <- iconv(x, from = "", to = "UTF-8", sub = " ")
  x <- str_replace_all(x, "<[^>]+>", " ")

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

  x <- str_replace_all(x, "(?i)\\#table_start\\b|\\#table_end\\b", " ")
  x <- str_replace_all(x, "_{3,}", " ")
  x <- str_replace_all(x, "-{5,}", " ")
  x <- str_replace_all(x, "={3,}", " ")
  x <- str_replace_all(x, "\\bPage\\s+\\d+\\b", " ")
  x <- str_replace_all(x, "\\.{3,}\\s*\\d+", " ")

  x <- tolower(x)

  x <- str_replace_all(x, "[\\r\\n\\t]+", " ")
  x <- str_replace_all(x, "\\s{2,}", " ")
  x <- str_trim(x)

  return(x)
}


#' Remove Conservative Boilerplate (unchanged from v2)
remove_boilerplate_conservative <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  x[is.na(x)] <- ""

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

  pattern_web <- paste0(
    "(?i)",
    "\\bwebsite access to\\b",
    ".{0,500}?",
    "\\bsec\\.gov\\b"
  )
  x <- str_replace_all(x, pattern_web, " ")

  pattern_avail <- paste0(
    "(?i)",
    "\\bavailable information\\b",
    ".{0,400}?",
    "(?:free of charge|without charge|investor relations)"
  )
  x <- str_replace_all(x, pattern_avail, " ")

  x <- str_replace_all(
    x,
    "(?i)united states securities and exchange commission.{0,200}?washington",
    " "
  )

  x <- str_replace_all(x, "\\s{2,}", " ")
  x <- str_trim(x)

  return(x)
}


#' Count Total Words (all non-whitespace tokens — includes numbers)
#' Used as denominator for NUMERIC densities.
count_words_total <- function(x) {
  if (is.null(x) || length(x) == 0) return(integer(0))
  x[is.na(x)] <- ""
  as.integer(str_count(x, "\\S+"))
}


#' Count Alphabetic Words Only (tokens matching [a-z] after lowercasing)
#' Used as denominator for DICTIONARY-BASED densities (tone, fwd-looking, risk).
#'
#' Rationale (v3 fix): quanteda's tokens() with remove_numbers=TRUE produces
#' only alphabetic tokens. Dictionary hit counts are computed on this set.
#' Dividing dictionary hits by wc_total (which includes "$100", "2024", "15%")
#' creates a systematic denominator mismatch: documents with more numbers
#' appear mechanically less dictionary-dense, attenuating the signal and
#' introducing a spurious negative correlation with operational specificity.
count_words_alpha <- function(x) {
  if (is.null(x) || length(x) == 0) return(integer(0))
  x[is.na(x)] <- ""
  as.integer(str_count(x, "\\b[a-z][-a-z']*\\b"))
}


cat("  - Functions defined: clean_edgar_text(), remove_boilerplate_conservative(),\n")
cat("    count_words_total(), count_words_alpha()\n\n")


# BLOCK 3: APPLY CLEANING TO BOTH SECTIONS ------------------------------------

cat("BLOCK 3: Applying cleaning to MD&A and Risk Factors...\n")

# v3.1: Create a case-preserving variant of clean_edgar_text for readability.
# textstat_readability (Fog, FK) relies on sentence boundary detection which
# uses capitalisation after periods. Lowercased text breaks this, producing
# Fog values >2000 (entire doc treated as one sentence).
clean_edgar_text_cased <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  x[is.na(x)] <- ""
  # Same pipeline as clean_edgar_text() but WITHOUT tolower()
  x <- str_replace_all(x, "<[^>]+>", " ")
  entity_map <- c(
    "&amp;" = "&", "&lt;" = "<", "&gt;" = ">",
    "&nbsp;" = " ", "&mdash;" = " - ", "&ndash;" = " - ",
    "&rsquo;" = "'", "&lsquo;" = "'", "&rdquo;" = '"',
    "&ldquo;" = '"', "&bull;" = " ", "&middot;" = " "
  )
  for (pat in names(entity_map)) {
    x <- str_replace_all(x, fixed(pat), entity_map[[pat]])
  }
  x <- str_replace_all(x, "&[a-zA-Z]+;", " ")
  x <- str_replace_all(x, "&#\\d+;", " ")
  x <- str_replace_all(x, "&#x[0-9a-fA-F]+;", " ")
  x <- str_replace_all(x, "(?i)\\#table_start\\b|\\#table_end\\b", " ")
  x <- str_replace_all(x, "_{3,}", " ")
  x <- str_replace_all(x, "-{5,}", " ")
  x <- str_replace_all(x, "={3,}", " ")
  x <- str_replace_all(x, "\\bPage\\s+\\d+\\b", " ")
  x <- str_replace_all(x, "\\.{3,}\\s*\\d+", " ")
  # NO tolower() here — preserve case for sentence boundary detection
  x <- str_replace_all(x, "[\\r\\n\\t]+", " ")
  x <- str_replace_all(x, "\\s{2,}", " ")
  x <- str_trim(x)
  return(x)
}

deals_cleaned <- deals_raw %>%
  mutate(
    mda_clean  = clean_edgar_text(mda_text),
    mda_clean  = remove_boilerplate_conservative(mda_clean),
    risk_clean = clean_edgar_text(risk_factors_text),
    risk_clean = remove_boilerplate_conservative(risk_clean),
    # Case-preserved versions (for readability only — Module 07)
    mda_clean_cased  = clean_edgar_text_cased(mda_text),
    mda_clean_cased  = remove_boilerplate_conservative(mda_clean_cased),
    risk_clean_cased = clean_edgar_text_cased(risk_factors_text),
    risk_clean_cased = remove_boilerplate_conservative(risk_clean_cased)
  )

cat("  - MD&A cleaning complete\n")
cat("  - Risk Factors cleaning complete\n")
cat("  - Case-preserved versions created (for readability)\n\n")


# BLOCK 4: DUAL WORD COUNTS AND QUALITY CHECKS --------------------------------

cat("BLOCK 4: Computing DUAL word counts and quality checks...\n")

deals_cleaned <- deals_cleaned %>%
  mutate(
    # Total word count (all tokens, includes numbers — for numeric density)
    mda_wc_total  = count_words_total(mda_clean),
    risk_wc_total = count_words_total(risk_clean),

    # Alphabetic-only word count (for dictionary-based densities)
    mda_wc_alpha  = count_words_alpha(mda_clean),
    risk_wc_alpha = count_words_alpha(risk_clean),

    # Backward-compat alias (downstream scripts expecting this name)
    mda_word_count_clean  = mda_wc_total,
    risk_word_count_clean = risk_wc_total
  )

# Diagnostic: alpha/total ratio (expect ~0.70–0.90 for typical 10-K text)
alpha_ratio_mda <- with(deals_cleaned,
  ifelse(mda_wc_total > 0, mda_wc_alpha / mda_wc_total, NA_real_))
alpha_ratio_risk <- with(deals_cleaned,
  ifelse(risk_wc_total > 0, risk_wc_alpha / risk_wc_total, NA_real_))

cat(sprintf("  - MD&A  alpha/total ratio: mean=%.3f | median=%.3f\n",
            mean(alpha_ratio_mda, na.rm = TRUE),
            median(alpha_ratio_mda, na.rm = TRUE)))
cat(sprintf("  - Risk  alpha/total ratio: mean=%.3f | median=%.3f\n",
            mean(alpha_ratio_risk, na.rm = TRUE),
            median(alpha_ratio_risk, na.rm = TRUE)))

cleaning_summary <- list()
cleaning_summary$n_deals     <- nrow(deals_cleaned)
cleaning_summary$n_mda_empty  <- sum(deals_cleaned$mda_wc_total == 0)
cleaning_summary$n_risk_empty <- sum(deals_cleaned$risk_wc_total == 0)

cleaning_summary$mda_wc_total_stats  <- summary(deals_cleaned$mda_wc_total)
cleaning_summary$mda_wc_alpha_stats  <- summary(deals_cleaned$mda_wc_alpha)
cleaning_summary$risk_wc_total_stats <- summary(deals_cleaned$risk_wc_total)
cleaning_summary$risk_wc_alpha_stats <- summary(deals_cleaned$risk_wc_alpha)

cleaning_summary$alpha_ratio_mda  <- summary(alpha_ratio_mda)
cleaning_summary$alpha_ratio_risk <- summary(alpha_ratio_risk)

# Reduction ratios (if raw counts available)
if ("mda_word_count" %in% names(deals_cleaned)) {
  deals_cleaned <- deals_cleaned %>%
    mutate(
      mda_reduction_ratio = ifelse(
        mda_word_count > 0,
        mda_wc_total / mda_word_count,
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
        risk_wc_total / risk_word_count,
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
sample_idx <- which(deals_cleaned$mda_wc_total > 100)
if (length(sample_idx) > 100) sample_idx <- sample(sample_idx, 100)

if (length(sample_idx) > 0) {
  stx <- deals_cleaned$mda_clean[sample_idx]
  cleaning_summary$sample_n             <- length(sample_idx)
  cleaning_summary$pct_with_numbers     <- mean(str_detect(stx, "\\d")) * 100
  cleaning_summary$pct_with_percentages <- mean(str_detect(stx, "\\d+%")) * 100
  cleaning_summary$pct_with_currency    <- mean(str_detect(stx, "\\$\\d")) * 100
  cleaning_summary$pct_with_decimals    <- mean(str_detect(stx, "\\d+\\.\\d+")) * 100
}

cat("  - Dual word counts computed\n")
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

cat("MD&A Word Count — Total (all tokens):\n")
print(cleaning_summary$mda_wc_total_stats)
cat("\nMD&A Word Count — Alphabetic only:\n")
print(cleaning_summary$mda_wc_alpha_stats)

cat("\nRisk Factors Word Count — Total:\n")
print(cleaning_summary$risk_wc_total_stats)
cat("\nRisk Factors Word Count — Alphabetic only:\n")
print(cleaning_summary$risk_wc_alpha_stats)
cat("\n")

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
                  "mda_clean_cased", "risk_clean_cased",
                  "mda_wc_total", "risk_wc_total",
                  "mda_wc_alpha", "risk_wc_alpha",
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
get_stat <- function(s, nm) {
  val <- s[nm]
  if (is.na(val)) return("NA") else return(as.character(round(val, 1)))
}

report_lines <- c(
  "# TEXT CLEANING REPORT (v3 — dual word counts)",
  paste("Generated:", Sys.time()),
  "",
  "## Key change in v3",
  "Introduced dual word counts:",
  "- `wc_total`: all non-whitespace tokens (for numeric density denominators)",
  "- `wc_alpha`: alphabetic tokens only (for dictionary-based density denominators)",
  "",
  "This resolves the denominator mismatch where dictionary hits (computed on",
  "alphabetic tokens after `remove_numbers=TRUE`) were divided by total word count",
  "(including numbers), systematically attenuating dictionary-based indices.",
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
  "## Alpha/Total Ratio (diagnostic)",
  paste("- MD&A  mean:", round(mean(alpha_ratio_mda, na.rm = TRUE), 3)),
  paste("- Risk  mean:", round(mean(alpha_ratio_risk, na.rm = TRUE), 3)),
  "(Expected range: 0.70–0.90 for typical 10-K text)",
  ""
)

report_file <- file.path(path_output_reports, "01_cleaning_report.md")
writeLines(report_lines, report_file)
cat(sprintf("  - Saved: %s\n", report_file))

cat("\n=== MODULE 1 COMPLETE (v3) ===\n")
cat(sprintf("Cleaned text: %s\n", output_file))
cat(sprintf("Report: %s\n", report_file))
cat("Ready for Module 2: Tokenization (v4)\n")
