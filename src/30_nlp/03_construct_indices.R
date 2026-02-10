# ==============================================================================
# MODULE 3: DISCLOSURE QUALITY INDEX CONSTRUCTION (REVISED v5)
# ==============================================================================
# Inputs:
#   - data/interim/dfm_objects.rds
#   - data/interim/numeric_features.rds
#   - data/interim/cleaned_text.rds
#   - config/analysis_config.rds
# Outputs:
#   - data/interim/disclosure_indices.rds
#   - reports/03_index_construction_report.md
#
# v5 changes (methodological defensibility + audit logging):
#   1) Forward-looking intensity = modal channel + regex channel (explicit).
#      Logs: number of active regex patterns; total regex matches; total modal
#      matches; their relative contribution; and per-1000w density.
#   2) Risk disclosure dictionary transparency:
#      Logs: dictionary source label (from config if provided), preprocessing
#      steps, raw sizes, final usable size, matched terms, unmatched terms,
#      and examples of both.
#   3) Normalisation with hierarchical fallback auditing:
#      Z-score at sic2×year if cell >= min_cell, else fallback to sic2, then year,
#      then global. Logs: counts at each fallback level; cell-size distributions.
#
# Notes:
#   - This module does not estimate any empirical models.
#   - Dictionaries are assumed pre-specified in config/analysis_config.rds.

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(quanteda)
  library(tidyr)
  library(Matrix)
})

cat("=== MODULE 3: INDEX CONSTRUCTION (v5) ===\n\n")

# ---- PATHS -------------------------------------------------------------------
project_root <- Sys.getenv("PROJECT_ROOT", unset = getwd())
path_interim <- file.path(project_root, "data", "interim")
path_reports <- file.path(project_root, "reports")
dir.create(path_reports, recursive = TRUE, showWarnings = FALSE)

path_dfm     <- file.path(path_interim, "dfm_objects.rds")
path_num     <- file.path(path_interim, "numeric_features.rds")
path_cleaned <- file.path(path_interim, "cleaned_text.rds")
path_config  <- file.path(project_root, "config", "analysis_config.rds")
path_out     <- file.path(path_interim, "disclosure_indices.rds")
path_report  <- file.path(path_reports, "03_index_construction_report.md")

stopifnot(file.exists(path_dfm), file.exists(path_num), file.exists(path_config), file.exists(path_cleaned))

# ---- HELPERS -----------------------------------------------------------------
`%||%` <- function(a, b) if (!is.null(a)) a else b

stop_if_missing <- function(x, msg) {
  if (is.null(x) || length(x) == 0) stop(msg, call. = FALSE)
  x
}

safe_density <- function(count, wc, per = 1000) {
  ifelse(!is.na(wc) & wc > 0, count / (wc / per), NA_real_)
}

zscore <- function(x) {
  s <- sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(0, length(x)))
  (x - mean(x, na.rm = TRUE)) / s
}

# Dictionary preprocessing: make token mapping auditable/reproducible.
# Preprocessing steps are logged explicitly below.
preprocess_dict <- function(x) {
  x <- tolower(x)
  x <- str_trim(x)
  x <- x[!is.na(x) & nzchar(x)]
  # keep only single-token items for DFM matching; multiword handled via regex channel where applicable
  x_single <- x[!str_detect(x, "\\s+")]
  unique(x_single)
}

# Build a safe combined regex from patterns (word boundaries applied where appropriate).
# Patterns are treated as regex as provided (no escaping), but we enforce case-insensitive
# matching by lowercasing text before counting.
combine_regex <- function(patterns) {
  patterns <- patterns[!is.na(patterns) & nzchar(patterns)]
  patterns <- unique(patterns)
  if (length(patterns) == 0) return(character(0))
  paste0("(", paste(patterns, collapse = "|"), ")")
}

# Hierarchical normalization with audited fallback level per observation.
normalize_hierarchical_audit <- function(df, value_col, min_cell = 10) {
  v <- df[[value_col]]
  out <- rep(NA_real_, nrow(df))
  lvl <- rep(NA_character_, nrow(df))

  apply_level <- function(group_cols, label) {
    g <- df %>%
      mutate(.value = v) %>%
      group_by(across(all_of(group_cols))) %>%
      mutate(.n = n()) %>%
      ungroup()

    idx <- which(is.na(out) & !is.na(v) & g$.n >= min_cell)
    if (length(idx) == 0) return(invisible(NULL))

    key <- interaction(g[idx, group_cols, drop = FALSE], drop = TRUE)
    out[idx] <<- ave(v[idx], key, FUN = function(x) zscore(x))
    lvl[idx] <<- label
    invisible(NULL)
  }

  if (all(c("sic2", "year_announced") %in% names(df))) apply_level(c("sic2", "year_announced"), "sic2_year")
  if ("sic2" %in% names(df)) apply_level("sic2", "sic2")
  if ("year_announced" %in% names(df)) apply_level("year_announced", "year")

  idx_rem <- which(is.na(out) & !is.na(v))
  if (length(idx_rem) > 0) {
    out[idx_rem] <- zscore(v[idx_rem])
    lvl[idx_rem] <- "global"
  }

  idx_na <- which(is.na(v))
  if (length(idx_na) > 0) lvl[idx_na] <- "missing"

  list(norm = out, level = lvl)
}

# ---- BLOCK 0: LOAD DATA ------------------------------------------------------
cat("BLOCK 0: Loading data...\n")

dfm_objects      <- readRDS(path_dfm)
numeric_features <- readRDS(path_num)
cleaned_data     <- readRDS(path_cleaned)
config           <- readRDS(path_config)

dfm_mda_uni       <- stop_if_missing(dfm_objects$mda_uni, "Missing mda_uni")
dfm_risk_uni      <- stop_if_missing(dfm_objects$risk_uni, "Missing risk_uni")
dfm_risk_tfidf    <- stop_if_missing(dfm_objects$risk_tfidf, "Missing risk_tfidf")
dfm_mda_compounds <- dfm_objects$mda_compounds %||% NULL

dv <- docvars(dfm_mda_uni)
stopifnot("deal_id" %in% names(dv))
dfm_deal_id <- as.character(dv$deal_id)

numeric_features <- numeric_features %>%
  mutate(deal_id = as.character(deal_id)) %>%
  right_join(tibble(deal_id = dfm_deal_id), by = "deal_id")

cleaned_data <- cleaned_data %>%
  mutate(deal_id = as.character(deal_id)) %>%
  right_join(tibble(deal_id = dfm_deal_id), by = "deal_id")

stopifnot(nrow(numeric_features) == ndoc(dfm_mda_uni), nrow(cleaned_data) == ndoc(dfm_mda_uni))

# Word counts (already computed in Module 1/2)
mda_wc  <- as.numeric(numeric_features$mda_word_count_clean %||% numeric_features$mda_word_count)
risk_wc <- as.numeric(numeric_features$risk_word_count_clean %||% numeric_features$risk_word_count)

# Dictionaries (assumed pre-specified in config)
lm_positive    <- config$lm_positive
lm_negative    <- config$lm_negative
lm_uncertainty <- config$lm_uncertainty
lm_litigious   <- config$lm_litigious

forward_looking_modal_terms <- config$forward_looking_modals %||% c("will","may","could","would","might","shall")
forward_looking_regex       <- config$forward_looking_regex %||% character(0)

# If regex list is missing/empty, use a conservative default list (can be overridden in config).
# This is intentionally short and designed to capture common forward-looking constructions
# beyond modals (e.g., "going forward", "in the future", "expects to", "plans to").
if (length(forward_looking_regex) == 0) {
  forward_looking_regex <- c(
    "\\bgoing\\s+forward\\b",
    "\\bin\\s+the\\s+future\\b",
    "\\bexpects?\\s+to\\b",
    "\\bexpected\\s+to\\b",
    "\\bplans?\\s+to\\b",
    "\\banticipat(e|ed|es|ing)\\b",
    "\\bforecast(s|ed|ing)?\\b",
    "\\boutlook\\b",
    "\\bwe\\s+believe\\b",
    "\\bintend(s|ed)?\\s+to\\b"
  )
  fl_regex_source <- "default(v5)"
} else {
  fl_regex_source <- "config"
}

operational_phrases  <- config$operational_phrases %||% character(0)

params <- config$params %||% list(weight_numeric = 0.5, weight_compounds = 0.5, min_cell_size = 10)
min_cell <- params$min_cell_size %||% 10

cat(sprintf("  LM: Pos=%d Neg=%d Unc=%d Lit=%d\n",
            length(lm_positive), length(lm_negative), length(lm_uncertainty), length(lm_litigious)))
cat(sprintf("  Forward-looking: modals=%d | regex=%d (%s)\n",
            length(forward_looking_modal_terms), length(forward_looking_regex), fl_regex_source))
cat(sprintf("  Operational phrases: %d | min_cell_size=%d\n\n",
            length(operational_phrases), min_cell))

# ---- BLOCK 1: OPERATIONAL SPECIFICITY ----------------------------------------
cat("BLOCK 1: Operational Specificity...\n")

numeric_density <- numeric_features$numeric_density
numeric_density <- as.numeric(numeric_density)

operational_compound_density <- rep(0, ndoc(dfm_mda_uni))
op_in_dfm <- character(0)

if (!is.null(dfm_mda_compounds) && nfeat(dfm_mda_compounds) > 0 && length(operational_phrases) > 0) {
  op_tokens <- str_replace_all(tolower(operational_phrases), "\\s+", "_")
  op_in_dfm <- intersect(op_tokens, featnames(dfm_mda_compounds))
  if (length(op_in_dfm) > 0) {
    dfm_op <- dfm_select(dfm_mda_compounds, pattern = op_in_dfm)
    operational_compound_density <- safe_density(rowSums(dfm_op), mda_wc)
  }
}

w_num <- params$weight_numeric %||% 0.5
w_cmp <- params$weight_compounds %||% 0.5
nd  <- ifelse(is.na(numeric_density), 0, numeric_density)
ocd <- ifelse(is.na(operational_compound_density), 0, operational_compound_density)

operational_specificity_raw <- w_num * nd + w_cmp * ocd

cat(sprintf("  Compounds matched in DFM: %d\n", length(op_in_dfm)))
cat(sprintf("  Composite (raw) mean: %.4f | sd: %.4f\n\n",
            mean(operational_specificity_raw, na.rm = TRUE),
            sd(operational_specificity_raw, na.rm = TRUE)))

# ---- BLOCK 2: FORWARD-LOOKING INTENSITY --------------------------------------
cat("BLOCK 2: Forward-Looking Intensity...\n")

# Modal channel (DFM-based; exact tokens)
modal_terms <- preprocess_dict(forward_looking_modal_terms)
modal_terms_in_dfm <- intersect(modal_terms, featnames(dfm_mda_uni))
modal_counts <- if (length(modal_terms_in_dfm) > 0) {
  as.numeric(rowSums(dfm_select(dfm_mda_uni, pattern = modal_terms_in_dfm)))
} else rep(0, ndoc(dfm_mda_uni))

# Regex channel (text-based; counts *occurrences* in cleaned MD&A text)
# IMPORTANT: this is intended to capture multi-word constructions and non-modal forward-looking phrases.
rx_active <- unique(forward_looking_regex[!is.na(forward_looking_regex) & nzchar(forward_looking_regex)])
rx_combined <- combine_regex(rx_active)

regex_counts <- rep(0, nrow(cleaned_data))
total_regex_matches <- 0L
if (length(rx_combined) == 1 && "mda_clean" %in% names(cleaned_data)) {
  txt <- tolower(cleaned_data$mda_clean)
  regex_counts <- str_count(txt, rx_combined)
  regex_counts[is.na(regex_counts)] <- 0
  total_regex_matches <- sum(regex_counts)
}

# Enforce nonzero regex activity when regex patterns are active.
if (length(rx_active) > 0 && total_regex_matches == 0) {
  stop(
    "Forward-looking regex channel active but produced zero matches. ",
    "Check forward_looking_regex patterns and/or mda_clean text preprocessing.",
    call. = FALSE
  )
}

# Final construct: forward-looking language = modals + regex patterns (exactly)
fl_total_counts <- modal_counts + regex_counts
fl_density <- safe_density(fl_total_counts, mda_wc)

# Logging required by spec
total_modal_matches <- sum(modal_counts)
cat(sprintf("  Active modal tokens (in DFM): %d/%d\n", length(modal_terms_in_dfm), length(modal_terms)))
cat(sprintf("  Active regex patterns: %d (%s)\n", length(rx_active), fl_regex_source))
cat(sprintf("  Total modal matches (corpus): %d\n", as.integer(total_modal_matches)))
cat(sprintf("  Total regex matches (corpus): %d\n", as.integer(total_regex_matches)))

den <- as.numeric(total_modal_matches + total_regex_matches)
modal_share <- ifelse(den > 0, total_modal_matches / den, NA_real_)
regex_share <- ifelse(den > 0, total_regex_matches / den, NA_real_)

cat(sprintf("  Contribution share: modal=%.3f | regex=%.3f\n",
            modal_share, regex_share))
cat(sprintf("  Forward-looking density mean: %.4f per 1000w | sd: %.4f\n\n",
            mean(fl_density, na.rm = TRUE), sd(fl_density, na.rm = TRUE)))

# ---- BLOCK 3: RISK DISCLOSURE (TRANSPARENT DICTIONARY ACCOUNTING) ------------
cat("BLOCK 3: Risk Disclosure...\n")

risk_source_label <- config$risk_dictionary_source %||% "LM(Uncertainty + Negative)"

raw_unc_n <- length(lm_uncertainty)
raw_neg_n <- length(lm_negative)
raw_combined_n <- length(c(lm_uncertainty, lm_negative))

risk_dict <- preprocess_dict(c(lm_uncertainty, lm_negative))
final_dict_n <- length(risk_dict)

cat(sprintf("  Dictionary source: %s\n", risk_source_label))
cat("  Preprocessing steps: lowercase -> trim -> drop NA/empty -> keep single-token -> unique\n")
cat(sprintf("  Raw sizes: Uncertainty=%d | Negative=%d | Combined=%d\n", raw_unc_n, raw_neg_n, raw_combined_n))
cat(sprintf("  Final usable dictionary size (single-token): %d\n", final_dict_n))

# DFM matching (unigram)
risk_terms_in_dfm <- intersect(risk_dict, featnames(dfm_risk_uni))
unmatched_terms   <- setdiff(risk_dict, risk_terms_in_dfm)

# Examples for auditability
ex_matched   <- head(sort(risk_terms_in_dfm), 10)
ex_unmatched <- head(sort(unmatched_terms), 10)

cat(sprintf("  Matched terms in risk_unigram DFM: %d/%d\n", length(risk_terms_in_dfm), final_dict_n))
cat(sprintf("  Unmatched terms (not in DFM feature set): %d/%d\n", length(unmatched_terms), final_dict_n))
cat(sprintf("  Examples matched: %s\n", paste(ex_matched, collapse = ", ")))
cat(sprintf("  Examples unmatched: %s\n", paste(ex_unmatched, collapse = ", ")))

risk_raw_counts <- if (length(risk_terms_in_dfm) > 0) {
  as.numeric(rowSums(dfm_select(dfm_risk_uni, pattern = risk_terms_in_dfm)))
} else rep(0, ndoc(dfm_risk_uni))
risk_disclosure_raw <- safe_density(risk_raw_counts, risk_wc)

docs_any_raw <- sum(risk_raw_counts > 0)

# TF-IDF share (dictionary TF-IDF mass / total TF-IDF mass)
risk_terms_in_tfidf <- intersect(risk_dict, featnames(dfm_risk_tfidf))
risk_tfidf_mass <- if (length(risk_terms_in_tfidf) > 0) {
  as.numeric(Matrix::rowSums(dfm_select(dfm_risk_tfidf, pattern = risk_terms_in_tfidf)))
} else rep(0, ndoc(dfm_risk_tfidf))
risk_tfidf_total <- as.numeric(Matrix::rowSums(dfm_risk_tfidf))

risk_tfidf_share <- ifelse(!is.na(risk_tfidf_total) & risk_tfidf_total > 0,
                           risk_tfidf_mass / risk_tfidf_total, 0)

docs_any_tfidf <- sum(risk_tfidf_mass > 0)

cat(sprintf("  Docs with any unigram match: %d/%d\n", docs_any_raw, ndoc(dfm_risk_uni)))
cat(sprintf("  TF-IDF matched terms in DFM: %d/%d\n", length(risk_terms_in_tfidf), final_dict_n))
cat(sprintf("  Docs with any TF-IDF mass: %d/%d | TF-IDF share mean: %.4f\n\n",
            docs_any_tfidf, ndoc(dfm_risk_tfidf), mean(risk_tfidf_share, na.rm = TRUE)))

# ---- BLOCK 4: MANAGERIAL TONE ------------------------------------------------
cat("BLOCK 4: Managerial Tone...\n")

pos_terms <- intersect(preprocess_dict(lm_positive), featnames(dfm_mda_uni))
neg_terms <- intersect(preprocess_dict(lm_negative), featnames(dfm_mda_uni))

pos_counts <- if (length(pos_terms) > 0) as.numeric(rowSums(dfm_select(dfm_mda_uni, pattern = pos_terms))) else rep(0, ndoc(dfm_mda_uni))
neg_counts <- if (length(neg_terms) > 0) as.numeric(rowSums(dfm_select(dfm_mda_uni, pattern = neg_terms))) else rep(0, ndoc(dfm_mda_uni))

tone_denom <- pos_counts + neg_counts
tone_lm <- ifelse(tone_denom >= 10, (pos_counts - neg_counts) / tone_denom, NA_real_)

cat(sprintf("  Matched LM terms: Pos=%d | Neg=%d\n", length(pos_terms), length(neg_terms)))
cat(sprintf("  ToneLM mean: %.4f | sd: %.4f | NA due to sparse (denom<10): %d\n\n",
            mean(tone_lm, na.rm = TRUE), sd(tone_lm, na.rm = TRUE), sum(tone_denom < 10)))

# ---- BLOCK 5: NORMALISATION WITH FALLBACK AUDIT ------------------------------
cat("BLOCK 5: Normalisation and fallback auditing...\n")

index_data <- tibble(
  deal_id = dfm_deal_id,
  year_announced = dv$year_announced %||% NA,
  target_primary_sic = dv$target_primary_sic %||% NA,
  sic2 = if (!is.null(dv$target_primary_sic)) substr(as.character(dv$target_primary_sic), 1, 2) else NA_character_,
  operational_specificity_raw = operational_specificity_raw,
  forward_looking_density = as.numeric(fl_density),
  risk_disclosure_raw = as.numeric(risk_disclosure_raw),
  risk_disclosure_tfidf = as.numeric(risk_tfidf_share),
  risk_disclosure_tfidf_mass = as.numeric(risk_tfidf_mass),
  risk_disclosure_tfidf_total = as.numeric(risk_tfidf_total),
  tone_lm = as.numeric(tone_lm)
) %>%
  mutate(
    sic2 = ifelse(is.na(sic2) | sic2 == "NA", substr(as.character(target_primary_sic), 1, 2), as.character(sic2))
  )

# Cell size distributions at each level
cell_sic2_year <- index_data %>% mutate(key = paste0(sic2, "_", year_announced)) %>%
  count(key, name = "n") %>% pull(n)
cell_sic2 <- index_data %>% count(sic2, name = "n") %>% pull(n)
cell_year <- index_data %>% count(year_announced, name = "n") %>% pull(n)

summ_cell <- function(x) {
  tibble(
    n_cells = length(x),
    median = as.integer(median(x)),
    p10 = as.integer(quantile(x, 0.10, na.rm = TRUE)),
    p25 = as.integer(quantile(x, 0.25, na.rm = TRUE)),
    p75 = as.integer(quantile(x, 0.75, na.rm = TRUE)),
    p90 = as.integer(quantile(x, 0.90, na.rm = TRUE))
  )
}

cat(sprintf("  min_cell_size=%d\n", min_cell))
cat("  Cell-size distributions:\n")
print(bind_rows(
  summ_cell(cell_sic2_year) %>% mutate(level = "sic2_year"),
  summ_cell(cell_sic2) %>% mutate(level = "sic2"),
  summ_cell(cell_year) %>% mutate(level = "year")
) %>% select(level, everything()))

# Apply normalization with audited fallback levels (for core indices)
norm_targets <- c(
  operational_specificity_raw = "operational_specificity_norm",
  forward_looking_density     = "forward_looking_norm",
  risk_disclosure_tfidf       = "risk_disclosure_tfidf_norm",
  tone_lm                     = "tone_lm_norm"
)

for (src in names(norm_targets)) {
  res <- normalize_hierarchical_audit(index_data, src, min_cell = min_cell)
  index_data[[norm_targets[[src]]]] <- res$norm
  index_data[[paste0(norm_targets[[src]], "_level")]] <- res$level
}

# Fallback usage summary (use operational_specificity as representative; report all)
audit_levels <- function(level_vec) {
  tibble(level = c("sic2_year","sic2","year","global","missing")) %>%
    mutate(n = sapply(level, function(l) sum(level_vec == l, na.rm = TRUE)))
}

cat("\n  Fallback usage (counts) by index:\n")
for (nm in norm_targets) {
  lvl_col <- paste0(nm, "_level")
  cat(sprintf("  - %s:\n", nm))
  print(audit_levels(index_data[[lvl_col]]))
}

# ---- BLOCK 6: SAVE OUTPUT ----------------------------------------------------
cat("\nBLOCK 6: Saving final dataset...\n")

final_df <- index_data %>%
  select(
    deal_id, target_primary_sic, sic2, year_announced,
    operational_specificity_raw, forward_looking_density, risk_disclosure_raw,
    risk_disclosure_tfidf, tone_lm,
    operational_specificity_norm, forward_looking_norm, risk_disclosure_tfidf_norm, tone_lm_norm,
    ends_with("_level")
  )

saveRDS(final_df, path_out)
cat(sprintf("  %d obs x %d vars -> %s\n\n", nrow(final_df), ncol(final_df), path_out))

# ---- BLOCK 7: WRITE REPORT ---------------------------------------------------
report_lines <- c(
  "# Module 3 - Index Construction Report (v5)", "",
  paste0("Generated: ", Sys.time()),
  paste0("Observations: ", nrow(final_df)), "",
  "## Forward-looking intensity construct",
  "- Definition: forward-looking language = modal-based detection + regex-pattern detection",
  paste0("- Modal tokens (configured): ", paste(modal_terms, collapse = ", ")),
  paste0("- Modal tokens active in DFM: ", paste(modal_terms_in_dfm, collapse = ", ")),
  paste0("- Regex patterns source: ", fl_regex_source),
  paste0("- Active regex patterns: ", length(rx_active)),
  paste0("- Total modal matches (corpus): ", as.integer(total_modal_matches)),
  paste0("- Total regex matches (corpus): ", as.integer(total_regex_matches)),
  paste0("- Contribution shares: modal=", round(modal_share, 3), " | regex=", round(regex_share, 3)), "",
  "## Risk dictionary transparency",
  paste0("- Source label: ", risk_source_label),
  "- Preprocessing: lowercase -> trim -> drop NA/empty -> keep single-token -> unique",
  paste0("- Raw sizes: Uncertainty=", raw_unc_n, " | Negative=", raw_neg_n, " | Combined=", raw_combined_n),
  paste0("- Final usable dictionary size: ", final_dict_n),
  paste0("- Matched in DFM (unigram): ", length(risk_terms_in_dfm), "/", final_dict_n),
  paste0("- Unmatched (not in DFM feature set): ", length(unmatched_terms), "/", final_dict_n),
  paste0("- Examples matched: ", paste(ex_matched, collapse = ", ")),
  paste0("- Examples unmatched: ", paste(ex_unmatched, collapse = ", ")), "",
  "## Normalisation audit (hierarchical fallback)",
  paste0("- min_cell_size: ", min_cell),
  "- Levels: sic2×year -> sic2 -> year -> global",
  ""
)

writeLines(report_lines, path_report)
cat(sprintf("Report: %s\n", path_report))
cat("=== MODULE 3 COMPLETE (v5) ===\n")
