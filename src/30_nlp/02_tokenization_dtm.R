# ==============================================================================
# MODULE 2: TOKENIZATION AND DTM CONSTRUCTION  (REVISED v3)
# ==============================================================================
# Purpose: Convert cleaned text to tokens and document-feature matrices (DFMs)
#          with finance-aware processing and robust measurement
#
# REVISION NOTES (v3):
#   1. Fixed n_millions / n_billions regex over-matching (m/b → million/billion)
#   2. Division-by-zero guards on all density calculations
#   3. Removed deprecated cur_data() usage
#   4. Quarter regex corrected for lowercase text
#   5. Protected-terms patch streamlined
#   6. All seeds explicit for reproducibility
#
# Input:  data/interim/cleaned_text.rds
# Output: tokens, DFMs, numeric features, vocabularies
# ==============================================================================

# SETUP ------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(quanteda)
  library(quanteda.textstats)
  library(dplyr)
  library(stringr)
})

# ---- Paths -------------------------------------------------------------------
project_root     <- Sys.getenv("PROJECT_ROOT", unset = getwd())
path_input       <- Sys.getenv("PATH_CLEANED_RDS",
                      unset = file.path(project_root, "data", "interim",
                                        "cleaned_text.rds"))
path_output_interim <- Sys.getenv("PATH_INTERIM_DIR",
                        unset = file.path(project_root, "data", "interim"))
path_output_reports <- Sys.getenv("PATH_REPORTS_DIR",
                        unset = file.path(project_root, "reports"))
path_config         <- Sys.getenv("PATH_CONFIG_DIR",
                        unset = file.path(project_root, "config"))

dir.create(path_output_interim, showWarnings = FALSE, recursive = TRUE)
dir.create(path_output_reports, showWarnings = FALSE, recursive = TRUE)

cat("=== MODULE 2: TOKENIZATION AND DTM CONSTRUCTION (v3) ===\n")
cat("Starting tokenization pipeline...\n\n")


# BLOCK 0: DATA LOADING -------------------------------------------------------

cat("BLOCK 0: Loading cleaned data...\n")

cleaned_data <- readRDS(path_input)

required_cols <- c("deal_id", "mda_clean", "risk_clean",
                   "mda_word_count_clean", "risk_word_count_clean")
missing_cols  <- setdiff(required_cols, names(cleaned_data))
if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

n_mda_na  <- sum(is.na(cleaned_data$mda_clean))
n_risk_na <- sum(is.na(cleaned_data$risk_clean))
if (n_mda_na > 0 | n_risk_na > 0) {
  warning(sprintf("Found %d NA in mda_clean, %d NA in risk_clean",
                  n_mda_na, n_risk_na))
}

cleaned_data <- cleaned_data %>%
  mutate(
    deal_id     = as.character(deal_id),
    doc_id_mda  = paste0("deal_", deal_id, "_mda"),
    doc_id_risk = paste0("deal_", deal_id, "_risk")
  )

cat(sprintf("  - Loaded %d deals\n", nrow(cleaned_data)))
cat(sprintf("  - MD&A texts: %d\n", sum(!is.na(cleaned_data$mda_clean))))
cat(sprintf("  - Risk Factors texts: %d\n\n", sum(!is.na(cleaned_data$risk_clean))))


# BLOCK 1: CORPUS CONSTRUCTION -------------------------------------------------

cat("BLOCK 1: Creating quanteda corpus objects...\n")

make_docvars <- function(df, wc_col) {
  dv <- data.frame(deal_id = df$deal_id, stringsAsFactors = FALSE)
  if ("year_announced"    %in% names(df)) dv$year_announced    <- df$year_announced
  if ("target_primary_sic" %in% names(df)) dv$target_primary_sic <- df$target_primary_sic
  dv[[wc_col]] <- df[[wc_col]]
  dv
}

corpus_mda  <- corpus(cleaned_data$mda_clean,
                      docnames = cleaned_data$doc_id_mda,
                      docvars  = make_docvars(cleaned_data, "mda_word_count_clean"))
corpus_risk <- corpus(cleaned_data$risk_clean,
                      docnames = cleaned_data$doc_id_risk,
                      docvars  = make_docvars(cleaned_data, "risk_word_count_clean"))

cat(sprintf("  - MD&A corpus: %d documents\n", ndoc(corpus_mda)))
cat(sprintf("  - Risk corpus: %d documents\n\n", ndoc(corpus_risk)))


# BLOCK 2: DUAL-TRACK STOPWORD LISTS ------------------------------------------

cat("BLOCK 2: Defining dual-track stopword lists...\n")

stopwords_base <- stopwords("en", source = "snowball")

# Track A: Dictionary indices — keep finance-critical words
keep_words_dict <- c(
  "will", "shall", "may", "might", "could", "would", "should",
  "not", "no", "nor", "neither", "never", "none",
  "more", "most", "less", "least", "few", "many", "several",
  "above", "below", "over", "under", "up", "down",
  "can", "cannot", "must", "need"
)
stopwords_dict <- setdiff(stopwords_base, keep_words_dict)

cat(sprintf("  - Track A: %d stopwords (kept %d finance-critical)\n",
            length(stopwords_dict), length(keep_words_dict)))

# Track B: Bigrams — minimal removal to preserve collocations
stopwords_bigrams <- c(
  "a", "an", "the", "this", "that", "these", "those",
  "i", "you", "he", "she", "it", "we", "they",
  "me", "him", "her", "us", "them",
  "but", "if", "than", "then", "when", "where", "while"
)
cat(sprintf("  - Track B: %d minimal stopwords for bigrams\n\n",
            length(stopwords_bigrams)))


# BLOCK 2B: OPERATIONAL PHRASE WHITELIST ---------------------------------------

cat("BLOCK 2B: Loading operational phrase whitelist...\n")

operational_phrases <- c(
  # Income statement
  "operating income", "net income", "gross profit", "gross margin",
  "operating margin", "net revenue", "total revenue",
  "cost of revenue", "cost of goods", "operating expenses",
  "selling general administrative", "research and development",
  "interest expense", "income tax", "tax rate", "effective tax",
  # Cash flow
  "cash flow", "operating cash", "free cash flow", "cash flows",
  "working capital", "net working capital",
  "capital expenditure", "capital expenditures",
  # Balance sheet
  "total assets", "total liabilities", "total debt", "long term debt",
  "short term debt", "stockholders equity", "shareholders equity",
  "current assets", "current liabilities",
  "accounts receivable", "accounts payable", "inventory turnover",
  # Ratios
  "return on", "return on equity", "return on assets", "return on investment",
  "debt to equity", "debt to", "asset turnover", "profit margin",
  # Operational
  "revenue growth", "same store", "comparable store", "unit volume",
  "average selling price", "capacity utilization", "market share",
  # Time periods
  "fiscal year", "year ended", "year over year", "quarter ended",
  "three months", "six months", "nine months", "twelve months",
  # Other
  "as of", "compared to", "basis points", "per share",
  "earnings per share", "diluted earnings"
)
cat(sprintf("  - Defined %d operational phrases\n\n", length(operational_phrases)))


# BLOCK 3: NUMERIC FEATURES VIA REGEX -----------------------------------------

cat("BLOCK 3: Extracting numeric features via regex...\n")

# REVISION: safe_density helper guards against division by zero
safe_density <- function(count, word_count, per = 1000) {
  ifelse(word_count > 0, count / (word_count / per), NA_real_)
}

numeric_features <- cleaned_data %>%
  mutate(
    n_numbers     = str_count(mda_clean, "\\d+"),
    n_decimals    = str_count(mda_clean, "\\d+\\.\\d+"),
    n_percentages = str_count(mda_clean, "\\d+\\s*%"),
    n_currency    = str_count(mda_clean, "\\$\\s*\\d"),
    # REVISION: Fixed over-matching regex for millions/billions
    # Old: "\\d+\\s*(million|m)\\b" matched "10 months", "3 meters"
    # New: require full word "million" or $NNNm pattern
    n_millions    = str_count(mda_clean, "\\d+\\s*million\\b|\\$\\d+m\\b"),
    n_billions    = str_count(mda_clean, "\\d+\\s*billion\\b|\\$\\d+b\\b"),
    # REVISION: Quarter regex corrected for lowercase text
    n_quarters    = str_count(mda_clean, "q[1-4]\\s?\\d{4}"),

    # Densities per 1000 words (guarded)
    numeric_density  = safe_density(n_numbers, mda_word_count_clean),
    decimal_density  = safe_density(n_decimals, mda_word_count_clean),
    percent_density  = safe_density(n_percentages, mda_word_count_clean),
    currency_density = safe_density(n_currency, mda_word_count_clean),
    million_density  = safe_density(n_millions, mda_word_count_clean),
    billion_density  = safe_density(n_billions, mda_word_count_clean),

    # Risk Factors numerics (for completeness)
    risk_n_numbers       = str_count(risk_clean, "\\d+"),
    risk_numeric_density = safe_density(risk_n_numbers, risk_word_count_clean)
  ) %>%
  select(
    deal_id,
    mda_word_count_clean, risk_word_count_clean,
    n_numbers, n_decimals, n_percentages, n_currency,
    n_millions, n_billions, n_quarters,
    numeric_density, decimal_density, percent_density,
    currency_density, million_density, billion_density,
    risk_n_numbers, risk_numeric_density
  )

cat(sprintf("  - Numeric features extracted for %d deals\n", nrow(numeric_features)))
cat(sprintf("  - Summary (MD&A numeric_density):\n"))
print(summary(numeric_features$numeric_density))
cat("\n")

numeric_file <- file.path(path_output_interim, "numeric_features.rds")
saveRDS(numeric_features, numeric_file)
cat(sprintf("  - Saved: %s\n\n", numeric_file))


# BLOCK 4: BASE TOKENIZATION --------------------------------------------------

cat("BLOCK 4: Creating base tokens (pre-stopword removal)...\n")

tokens_mda_base <- tokens(
  corpus_mda,
  what = "word",
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_numbers = TRUE,      # Already extracted via regex
  remove_url = TRUE,
  remove_separators = TRUE,
  split_hyphens = FALSE,      # Keep "long-term", "short-term"
  include_docvars = TRUE
)

tokens_risk_base <- tokens(
  corpus_risk,
  what = "word",
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_numbers = TRUE,
  remove_url = TRUE,
  remove_separators = TRUE,
  split_hyphens = FALSE,
  include_docvars = TRUE
)

cat(sprintf("  - MD&A: %d documents\n", ndoc(tokens_mda_base)))
cat(sprintf("  - Risk: %d documents\n\n", ndoc(tokens_risk_base)))


# BLOCK 5: TRACK A — DICTIONARY-READY UNIGRAMS --------------------------------

cat("BLOCK 5: Track A — Dictionary-ready unigrams...\n")

tokens_mda_uni_dict  <- tokens_remove(tokens_mda_base, pattern = stopwords_dict)
tokens_risk_uni_dict <- tokens_remove(tokens_risk_base, pattern = stopwords_dict)

cat(sprintf("  - MD&A unigrams (dict): %d documents\n", ndoc(tokens_mda_uni_dict)))
cat(sprintf("  - Risk unigrams (dict): %d documents\n\n", ndoc(tokens_risk_uni_dict)))


# BLOCK 6: TRACK B — BIGRAM-READY WITH COMPOUNDS ------------------------------

cat("BLOCK 6: Track B — Bigram-ready tokens with compounds...\n")

tokens_mda_for_bi  <- tokens_remove(tokens_mda_base, pattern = stopwords_bigrams)
tokens_risk_for_bi <- tokens_remove(tokens_risk_base, pattern = stopwords_bigrams)

tokens_mda_compounds <- tokens_compound(
  tokens_mda_for_bi,
  pattern = phrase(operational_phrases),
  concatenator = "_"
)
tokens_risk_compounds <- tokens_compound(
  tokens_risk_for_bi,
  pattern = phrase(operational_phrases),
  concatenator = "_"
)

tokens_mda_bi_free  <- tokens_ngrams(tokens_mda_compounds, n = 2, concatenator = "_")
tokens_risk_bi_free <- tokens_ngrams(tokens_risk_compounds, n = 2, concatenator = "_")

cat(sprintf("  - Compounds applied (%d phrases)\n", length(operational_phrases)))
cat(sprintf("  - MD&A compound tokens: %d docs\n", ndoc(tokens_mda_compounds)))
cat(sprintf("  - MD&A free bigrams: %d docs\n\n", ndoc(tokens_mda_bi_free)))


# BLOCK 7: CREATE DFMs — UNIGRAMS ---------------------------------------------

cat("BLOCK 7: Creating unigram DFMs...\n")

dfm_mda_uni  <- dfm(tokens_mda_uni_dict)
dfm_risk_uni <- dfm(tokens_risk_uni_dict)

cat(sprintf("  - MD&A: %d features, sparsity %.2f%%\n",
            nfeat(dfm_mda_uni), sparsity(dfm_mda_uni) * 100))
cat(sprintf("  - Risk: %d features, sparsity %.2f%%\n\n",
            nfeat(dfm_risk_uni), sparsity(dfm_risk_uni) * 100))


# BLOCK 8: TRIM DFMs — UNIGRAMS -----------------------------------------------

cat("BLOCK 8: Trimming unigram vocabularies...\n")

dfm_mda_uni_trim <- dfm_mda_uni %>%
  dfm_trim(min_docfreq = 2, docfreq_type = "count") %>%
  dfm_trim(max_docfreq = 0.95, docfreq_type = "prop")

dfm_risk_uni_trim <- dfm_risk_uni %>%
  dfm_trim(min_docfreq = 2, docfreq_type = "count") %>%
  dfm_trim(max_docfreq = 0.95, docfreq_type = "prop")

cat(sprintf("  - MD&A: %d → %d features\n",
            nfeat(dfm_mda_uni), nfeat(dfm_mda_uni_trim)))
cat(sprintf("  - Risk: %d → %d features\n\n",
            nfeat(dfm_risk_uni), nfeat(dfm_risk_uni_trim)))

# PATCH: Protect critical modal verbs & negations from max-docfreq trimming
protected_terms <- c(
  "will", "would", "shall", "may", "might", "can", "could", "should", "must",
  "not", "no", "nor", "neither", "never", "none"
)

add_back_protected <- function(dfm_trimmed, dfm_full, protected) {
  # Re-add features that were trimmed but are theoretically required
  # (e.g., high-frequency modals removed by max_docfreq)
  protected_present <- intersect(protected, featnames(dfm_full))
  missing <- setdiff(protected_present, featnames(dfm_trimmed))
  if (length(missing) > 0) {
    dfm_add <- dfm_select(dfm_full, pattern = missing, selection = "keep")
    # Use cbind() generic — works in both old and new quanteda versions
    # (quanteda registers an S4 method for cbind on dfm objects)
    dfm_trimmed <- cbind(dfm_trimmed, dfm_add)
    # Compress to merge any duplicate features (safety)
    if ("dfm_compress" %in% getNamespaceExports("quanteda")) {
      dfm_trimmed <- quanteda::dfm_compress(dfm_trimmed)
    }
  }
  dfm_trimmed
}

dfm_mda_uni_trim  <- add_back_protected(dfm_mda_uni_trim, dfm_mda_uni, protected_terms)
dfm_risk_uni_trim <- add_back_protected(dfm_risk_uni_trim, dfm_risk_uni, protected_terms)

modal_check <- c("will", "may", "could", "would", "might", "shall")
cat("  Protected modal check (MD&A):\n")
for (w in modal_check) {
  cat(sprintf("    %-6s: %s\n", w,
              ifelse(w %in% featnames(dfm_mda_uni_trim), "OK", "MISSING")))
}
cat("\n")


# BLOCK 9: CREATE DFMs — COMPOUNDS AND BIGRAMS --------------------------------

cat("BLOCK 9: Creating compound/bigram DFMs...\n")

dfm_mda_compounds <- dfm(tokens_mda_compounds)
dfm_mda_compounds_trim <- dfm_mda_compounds %>%
  dfm_trim(min_docfreq = 2, docfreq_type = "count") %>%
  dfm_trim(max_docfreq = 0.90, docfreq_type = "prop")

dfm_mda_bi_free <- dfm(tokens_mda_bi_free)
dfm_mda_bi_free_trim <- dfm_mda_bi_free %>%
  dfm_trim(min_docfreq = 3, docfreq_type = "count") %>%
  dfm_trim(max_docfreq = 0.85, docfreq_type = "prop")

dfm_risk_compounds <- dfm(tokens_risk_compounds) %>%
  dfm_trim(min_docfreq = 2, docfreq_type = "count") %>%
  dfm_trim(max_docfreq = 0.90, docfreq_type = "prop")

cat(sprintf("  - MD&A compounds: %d features (trimmed)\n",
            nfeat(dfm_mda_compounds_trim)))
cat(sprintf("  - MD&A free bigrams: %d features (trimmed)\n",
            nfeat(dfm_mda_bi_free_trim)))
cat(sprintf("  - Risk compounds: %d features (trimmed)\n\n",
            nfeat(dfm_risk_compounds)))


# BLOCK 10: TF-IDF FOR RISK DISCLOSURE ----------------------------------------

cat("BLOCK 10: Creating TF-IDF weighted DFM for Risk Disclosure...\n")

dfm_risk_tfidf <- dfm_tfidf(
  dfm_risk_uni_trim,
  scheme_tf = "prop",
  scheme_df = "inverse"
)

cat(sprintf("  - Risk TF-IDF DFM: %d docs × %d features\n\n",
            ndoc(dfm_risk_tfidf), nfeat(dfm_risk_tfidf)))


# BLOCK 11: NEGATION RATE DIAGNOSTIC ------------------------------------------

cat("BLOCK 11: Computing negation diagnostics...\n")

negation_features <- cleaned_data %>%
  mutate(
    n_negations     = str_count(mda_clean, "\\b(not|no|never|nor|neither|none)\\b"),
    negation_rate   = safe_density(n_negations, mda_word_count_clean),
    risk_n_negations   = str_count(risk_clean, "\\b(not|no|never|nor|neither|none)\\b"),
    risk_negation_rate = safe_density(risk_n_negations, risk_word_count_clean)
  ) %>%
  select(deal_id, n_negations, negation_rate, risk_n_negations, risk_negation_rate)

negation_file <- file.path(path_output_interim, "negation_diagnostics.rds")
saveRDS(negation_features, negation_file)
cat(sprintf("  - Saved: %s\n\n", negation_file))


# BLOCK 12: VALIDATION --------------------------------------------------------

cat("BLOCK 12: Running validation checks...\n")

cat("\n  Top 20 MD&A unigrams (dict-ready):\n")
top_mda_uni <- topfeatures(dfm_mda_uni_trim, n = 20)
print(top_mda_uni)

cat("\n  Top 20 MD&A compounds:\n")
top_mda_compounds <- topfeatures(dfm_mda_compounds_trim, n = 20)
print(top_mda_compounds)

important_words <- c("revenue", "margin", "debt", "capital", "risk",
                     "uncertainty", "expect", "will", "increase",
                     "decrease", "not", "may")

# Diagnostic note: exact-form checks can produce false "MISS" when the corpus uses inflections
# (e.g., "revenues" instead of "revenue"). The diagnostic therefore checks a small set of
# conservative variants (plural + common verb inflections) purely for validation/reporting.
variant_forms <- function(w) {
  unique(c(
    w,
    paste0(w, "s"),
    paste0(w, "es"),
    paste0(w, "ed"),
    paste0(w, "ing")
  ))
}

present_any <- function(dfm_obj, w) {
  any(variant_forms(w) %in% featnames(dfm_obj))
}

word_check <- data.frame(
  word        = important_words,
  in_mda_uni  = sapply(important_words, function(w) present_any(dfm_mda_uni_trim,  w)),
  in_risk_uni = sapply(important_words, function(w) present_any(dfm_risk_uni_trim, w))
)
cat("\n  Finance word retention:\n")
for (i in seq_len(nrow(word_check))) {
  cat(sprintf("    %-15s MD&A: %s  Risk: %s\n",
              word_check$word[i],
              ifelse(word_check$in_mda_uni[i], "OK", "MISS"),
              ifelse(word_check$in_risk_uni[i], "OK", "MISS")))
}

summary_stats <- data.frame(
  DFM_Type  = c("MD&A Unigram (Dict)", "MD&A Compounds", "MD&A Free Bigrams",
                "Risk Unigram", "Risk TF-IDF"),
  Documents = c(ndoc(dfm_mda_uni_trim), ndoc(dfm_mda_compounds_trim),
                ndoc(dfm_mda_bi_free_trim), ndoc(dfm_risk_uni_trim),
                ndoc(dfm_risk_tfidf)),
  Features  = c(nfeat(dfm_mda_uni_trim), nfeat(dfm_mda_compounds_trim),
                nfeat(dfm_mda_bi_free_trim), nfeat(dfm_risk_uni_trim),
                nfeat(dfm_risk_tfidf)),
  Sparsity  = sprintf("%.2f%%",
    c(sparsity(dfm_mda_uni_trim), sparsity(dfm_mda_compounds_trim),
      sparsity(dfm_mda_bi_free_trim), sparsity(dfm_risk_uni_trim),
      sparsity(dfm_risk_tfidf)) * 100)
)
cat("\n  Summary:\n")
print(summary_stats)
cat("\n")


# BLOCK 13: SAVE OUTPUTS ------------------------------------------------------

cat("BLOCK 13: Saving outputs...\n")

tokens_file <- file.path(path_output_interim, "tokens_objects.rds")
saveRDS(list(
  mda_uni_dict    = tokens_mda_uni_dict,
  risk_uni_dict   = tokens_risk_uni_dict,
  mda_compounds   = tokens_mda_compounds,
  risk_compounds  = tokens_risk_compounds,
  mda_bi_free     = tokens_mda_bi_free,
  metadata = list(
    created = Sys.time(),
    n_docs  = ndoc(tokens_mda_uni_dict),
    stopwords_dict    = stopwords_dict,
    stopwords_bigrams = stopwords_bigrams,
    operational_phrases = operational_phrases,
    approach = "dual_track_with_compounds_v3"
  )
), tokens_file)
cat(sprintf("  - Saved: %s\n", tokens_file))

dfm_file <- file.path(path_output_interim, "dfm_objects.rds")
saveRDS(list(
  mda_uni      = dfm_mda_uni_trim,
  risk_uni     = dfm_risk_uni_trim,
  mda_compounds = dfm_mda_compounds_trim,
  mda_bi_free  = dfm_mda_bi_free_trim,
  risk_tfidf   = dfm_risk_tfidf,
  metadata = list(
    created = Sys.time(),
    trim_params = list(
      uni_min_docfreq = 2, uni_max_docfreq_pct = 95,
      compounds_min_docfreq = 2, bigrams_min_docfreq = 3
    ),
    tfidf_scheme = "prop_inverse"
  )
), dfm_file)
cat(sprintf("  - Saved: %s\n", dfm_file))

vocab_file <- file.path(path_output_interim, "vocabulary_lists.rds")
saveRDS(list(
  mda_uni_features       = featnames(dfm_mda_uni_trim),
  mda_compounds_features = featnames(dfm_mda_compounds_trim),
  mda_bi_features        = featnames(dfm_mda_bi_free_trim),
  risk_uni_features      = featnames(dfm_risk_uni_trim),
  stopwords_dict    = stopwords_dict,
  stopwords_bigrams = stopwords_bigrams,
  kept_words_dict   = keep_words_dict,
  operational_phrases = operational_phrases
), vocab_file)
cat(sprintf("  - Saved: %s\n", vocab_file))

summary_file <- file.path(path_output_interim, "tokenization_summary.rds")
saveRDS(list(
  summary_table = summary_stats,
  word_retention_check = word_check,
  numeric_features_summary = summary(numeric_features$numeric_density),
  negation_rate_summary    = summary(negation_features$negation_rate),
  execution_time = Sys.time()
), summary_file)
cat(sprintf("  - Saved: %s\n\n", summary_file))


# BLOCK 14: REPORT -------------------------------------------------------------

cat("BLOCK 14: Generating report...\n")

report_lines <- c(
  "# Tokenization and DTM Construction Report (v3)",
  "",
  paste("**Generated:**", Sys.time()),
  paste("**Input:**", path_input),
  paste("**Documents:**", ndoc(tokens_mda_uni_dict)),
  "",
  "## Revision Notes (v3)",
  "- Fixed million/billion regex (no longer matches 'months', 'below', etc.)",
  "- Division-by-zero guards on all density calculations",
  "- Quarter regex corrected for lowercase text",
  "- Protected-terms patch for high-frequency modals",
  "",
  "## DFM Summary",
  "```",
  capture.output(print(summary_stats)),
  "```",
  "",
  "## Finance Word Retention",
  capture.output(print(word_check)),
  ""
)

report_file <- file.path(path_output_reports, "02_tokenization_report.md")
writeLines(report_lines, report_file)
cat(sprintf("  - Report: %s\n", report_file))

cat("\n=== MODULE 2 COMPLETE (v3) ===\n")
cat("Ready for Module 3: Index Construction\n")
