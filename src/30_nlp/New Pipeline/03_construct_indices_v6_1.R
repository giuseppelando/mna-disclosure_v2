# ==============================================================================
# MODULE 3: DISCLOSURE QUALITY INDEX CONSTRUCTION (REVISED v6)
# ==============================================================================
# Inputs:
#   - data/interim/dfm_objects.rds       (Module 2 v4)
#   - data/interim/numeric_features.rds  (Module 2 v4)
#   - data/interim/cleaned_text.rds      (Module 1 v3)
#   - config/analysis_config.rds
# Outputs:
#   - data/interim/disclosure_indices.rds
#   - reports/03_index_construction_report.md
#
# v6 CHANGES (feedback-driven — 5 high-impact fixes + 3 construct redesigns):
#
# FIX 1 — Coherent denominators:
#   Dictionary-based densities (tone, FL, risk) use wc_alpha (alphabetic tokens
#   only). Numeric densities use wc_total. This eliminates the systematic
#   attenuation caused by dividing dictionary hits by inflated denominators.
#
# FIX 2 — No max_docfreq trimming on dictionary DFMs:
#   Handled upstream in Module 2 v4. Dictionary DFMs now retain all LM terms.
#
# FIX 3 — Multiword dictionary terms:
#   preprocess_dict() now ERRORS on multiword entries instead of silently
#   splitting them into unigrams (which caused massive false positives).
#
# FIX 4 — Tone formula corrected:
#   Now uses (Pos - Neg) / (Pos + Neg + 1), always defined (no NA from sparse
#   denominator). The +1 smoothing follows standard practice and eliminates
#   the endogenous missingness from the denom>=10 threshold.
#
# FIX 5 — Forward-looking: commitment vs hedging separated:
#   Strong modals (will/shall/must) → commitment channel
#   Weak modals (may/might/could) → hedging channel (feeds uncertainty)
#   Sentence-based FL measure is the new primary construct.
#
# NEW CONSTRUCT A — Forward-looking (sentence-based):
#   Primary FL measure = share of MD&A sentences containing prospective
#   markers. Secondary = share of FL sentences containing numbers (precision).
#
# NEW CONSTRUCT B — Risk transparency (cosine similarity to peers):
#   1 − cosine_similarity(doc_tfidf, industry_year_centroid_tfidf)
#   Higher = more idiosyncratic/less boilerplate risk disclosure.
#
# NEW CONSTRUCT C — Tone corrected (always-defined formula):
#   (Pos - Neg) / (Pos + Neg + 1). Also exports Pos_density and Neg_density
#   separately for robustness.
#
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(quanteda)
  library(tidyr)
  library(Matrix)
})

cat("=== MODULE 3: INDEX CONSTRUCTION (v6) ===\n\n")

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

stopifnot(file.exists(path_dfm), file.exists(path_num),
          file.exists(path_config), file.exists(path_cleaned))

# ---- HELPERS -----------------------------------------------------------------
`%||%` <- function(a, b) if (!is.null(a)) a else b

stop_if_missing <- function(x, msg) {
  if (is.null(x) || length(x) == 0) stop(msg, call. = FALSE)
  x
}

# v6: TWO density functions — one for numeric, one for dictionary
safe_density_total <- function(count, wc_total, per = 1000) {
  ifelse(!is.na(wc_total) & wc_total > 0, count / (wc_total / per), NA_real_)
}

safe_density_alpha <- function(count, wc_alpha, per = 1000) {
  ifelse(!is.na(wc_alpha) & wc_alpha > 0, count / (wc_alpha / per), NA_real_)
}

zscore <- function(x) {
  s <- sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(0, length(x)))
  (x - mean(x, na.rm = TRUE)) / s
}

# FIX 3: preprocess_dict now REJECTS multiword terms
preprocess_dict <- function(x, label = "unnamed") {
  x <- tolower(x)
  x <- str_trim(x)
  x <- x[!is.na(x) & nzchar(x)]
  multiword <- x[str_detect(x, "\\s+")]
  if (length(multiword) > 0) {
    stop(sprintf(
      "Dictionary '%s' contains %d multiword terms that cannot be matched ",
      label, length(multiword)),
      "against a unigram DFM. Multiword terms must be handled via regex or ",
      "compound tokens.\nExamples: ",
      paste(head(multiword, 5), collapse = ", "),
      call. = FALSE)
  }
  unique(x)
}

combine_regex <- function(patterns) {
  patterns <- patterns[!is.na(patterns) & nzchar(patterns)]
  patterns <- unique(patterns)
  if (length(patterns) == 0) return(character(0))
  paste0("(", paste(patterns, collapse = "|"), ")")
}

# Hierarchical normalization (unchanged logic)
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

  if (all(c("sic2", "year_announced") %in% names(df)))
    apply_level(c("sic2", "year_announced"), "sic2_year")
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

stopifnot(nrow(numeric_features) == ndoc(dfm_mda_uni),
          nrow(cleaned_data) == ndoc(dfm_mda_uni))

# v6: DUAL word counts
mda_wc_total  <- as.numeric(numeric_features$mda_wc_total %||% numeric_features$mda_word_count_clean)
mda_wc_alpha  <- as.numeric(numeric_features$mda_wc_alpha %||% as.integer(mda_wc_total * 0.80))
risk_wc_total <- as.numeric(numeric_features$risk_wc_total %||% numeric_features$risk_word_count_clean)
risk_wc_alpha <- as.numeric(numeric_features$risk_wc_alpha %||% as.integer(risk_wc_total * 0.80))

# Dictionaries
lm_positive    <- config$lm_positive
lm_negative    <- config$lm_negative
lm_uncertainty <- config$lm_uncertainty
lm_litigious   <- config$lm_litigious

operational_phrases <- config$operational_phrases %||% character(0)

params <- config$params %||% list(weight_numeric = 0.5, weight_compounds = 0.5, min_cell_size = 10)
min_cell <- params$min_cell_size %||% 10

cat(sprintf("  LM: Pos=%d Neg=%d Unc=%d Lit=%d\n",
            length(lm_positive), length(lm_negative),
            length(lm_uncertainty), length(lm_litigious)))
cat(sprintf("  min_cell_size=%d\n\n", min_cell))

# ---- BLOCK 1: OPERATIONAL SPECIFICITY (unchanged logic, corrected denom) ------
cat("BLOCK 1: Operational Specificity...\n")

# Numeric density uses wc_total (correct: numbers are the thing being counted)
numeric_density <- as.numeric(numeric_features$numeric_density)

operational_compound_density <- rep(0, ndoc(dfm_mda_uni))
op_in_dfm <- character(0)

if (!is.null(dfm_mda_compounds) && nfeat(dfm_mda_compounds) > 0 &&
    length(operational_phrases) > 0) {
  op_tokens <- str_replace_all(tolower(operational_phrases), "\\s+", "_")
  op_in_dfm <- intersect(op_tokens, featnames(dfm_mda_compounds))
  if (length(op_in_dfm) > 0) {
    dfm_op <- dfm_select(dfm_mda_compounds, pattern = op_in_dfm)
    # Compound density uses wc_alpha (compounds are alphabetic tokens)
    operational_compound_density <- safe_density_alpha(rowSums(dfm_op), mda_wc_alpha)
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


# ---- BLOCK 2: FORWARD-LOOKING INTENSITY (REDESIGNED — sentence-based) --------
cat("BLOCK 2: Forward-Looking Intensity (v6.1 — sentence-based)...\n")

# v6.1: FL action verbs configurable via config; abbreviation-safe splitting.
#
# The "will + action verb" pattern uses a closed list intentionally: open-ended
# matching ("will + any verb") would capture legal boilerplate ("will not be
# liable", "will indemnify"). The list is conservative by design.

fl_action_verbs <- config$fl_action_verbs %||% c(
  "continue", "increase", "decrease", "remain", "result", "generate",
  "achieve", "deliver", "drive", "invest", "expand", "grow", "improve",
  "reduce", "pursue", "focus", "seek", "launch"
)

fl_sentence_markers <- c(
  # Explicit prospective phrases (high precision)
  "\\bexpect(s|ed|ing)?\\s+(to|that)\\b",
  "\\banticipat(e|ed|es|ing)\\b",
  "\\bforecast(s|ed|ing)?\\b",
  "\\bproject(s|ed|ing)?\\s+(to|that|a|an|the|revenue|growth|income|earnings)\\b",
  "\\boutlook\\b",
  "\\bguidance\\b",
  "\\bplan(s|ned)?\\s+to\\b",
  "\\bintend(s|ed)?\\s+to\\b",
  "\\btarget(s|ed|ing)?\\b.{0,20}\\b(growth|revenue|margin|rate|ratio)\\b",
  "\\bgoing\\s+forward\\b",
  "\\bin\\s+the\\s+future\\b",
  "\\bnext\\s+(quarter|year|fiscal|twelve|six|three)\\b",
  "\\bover\\s+the\\s+next\\b",
  "\\bfor\\s+(fiscal\\s+)?\\d{4}\\b",
  # Temporal future (will/shall + configurable action verb list)
  paste0("\\bwill\\s+(", paste(fl_action_verbs, collapse = "|"), ")\\b"),
  paste0("\\bshall\\s+(", paste(fl_action_verbs[1:4], collapse = "|"), ")\\b")
)
fl_sentence_regex <- combine_regex(fl_sentence_markers)

cat(sprintf("  FL sentence markers: %d patterns (%d action verbs from %s)\n",
            length(fl_sentence_markers), length(fl_action_verbs),
            ifelse(is.null(config$fl_action_verbs), "default", "config")))

# Abbreviation-safe sentence splitting
# 10-K text contains abbreviations (e.g., "U.S.", "i.e.", "e.g.", "vs.",
# "Inc.", "Corp.", "No.", "approx.") that create false sentence breaks.
# We protect common ones before splitting, then restore after.
protect_abbreviations <- function(txt) {
  # Protect known abbreviations by replacing their periods with a placeholder
  abbrevs <- c("u\\.s\\.", "i\\.e\\.", "e\\.g\\.", "vs\\.", "inc\\.", "corp\\.",
               "ltd\\.", "no\\.", "approx\\.", "dept\\.", "avg\\.", "est\\.",
               "assn\\.", "govt\\.", "natl\\.", "intl\\.", "mgmt\\.",
               "\\bdr\\.", "\\bmr\\.", "\\bms\\.", "\\bmrs\\.")
  for (ab in abbrevs) {
    txt <- str_replace_all(txt, regex(ab, ignore_case = TRUE),
                           function(m) str_replace_all(m, "\\.", "\u2024"))
  }
  # Protect decimal numbers (e.g., "3.14", "$2.5 million")
  txt <- str_replace_all(txt, "(\\d)\\.(\\d)", "\\1\u2024\\2")
  txt
}

restore_abbreviations <- function(sents) {
  str_replace_all(sents, "\u2024", ".")
}

# Sentence splitting + classification
fl_results <- lapply(seq_len(nrow(cleaned_data)), function(i) {
  txt <- cleaned_data$mda_clean[i]
  if (is.na(txt) || nchar(txt) < 50) {
    return(list(n_sentences = 0L, n_fl = 0L, n_fl_with_num = 0L))
  }

  # Protect abbreviations, split, restore
  txt_safe <- protect_abbreviations(txt)
  sents <- unlist(str_split(txt_safe, "(?<=[.!?;])\\s+"))
  sents <- restore_abbreviations(sents)
  sents <- sents[nchar(sents) > 15]  # filter fragments (raised from 10)
  n_sentences <- length(sents)

  if (n_sentences == 0) {
    return(list(n_sentences = 0L, n_fl = 0L, n_fl_with_num = 0L))
  }

  # Classify
  is_fl <- str_detect(sents, fl_sentence_regex)
  n_fl <- sum(is_fl)

  # Among FL sentences, how many contain numbers? (precision proxy)
  n_fl_with_num <- sum(is_fl & str_detect(sents, "\\d"))

  list(n_sentences = n_sentences, n_fl = n_fl, n_fl_with_num = n_fl_with_num)
})

fl_df <- bind_rows(lapply(fl_results, as_tibble))

fl_sentence_share <- ifelse(fl_df$n_sentences > 0,
                            fl_df$n_fl / fl_df$n_sentences, NA_real_)
fl_precision_share <- ifelse(fl_df$n_fl > 0,
                             fl_df$n_fl_with_num / fl_df$n_fl, NA_real_)

cat(sprintf("  FL sentence share: mean=%.4f | sd=%.4f\n",
            mean(fl_sentence_share, na.rm = TRUE),
            sd(fl_sentence_share, na.rm = TRUE)))
cat(sprintf("  FL precision share (FL sents with numbers): mean=%.4f\n",
            mean(fl_precision_share, na.rm = TRUE)))

# Also compute legacy token-based FL for backward compatibility / robustness
# But now with commitment/hedging separation (FIX 5)
strong_modals <- c("will", "shall", "must")
weak_modals   <- c("may", "might", "could", "would", "should")

strong_in_dfm <- intersect(strong_modals, featnames(dfm_mda_uni))
weak_in_dfm   <- intersect(weak_modals, featnames(dfm_mda_uni))

strong_counts <- if (length(strong_in_dfm) > 0) {
  as.numeric(rowSums(dfm_select(dfm_mda_uni, pattern = strong_in_dfm)))
} else rep(0, ndoc(dfm_mda_uni))

weak_counts <- if (length(weak_in_dfm) > 0) {
  as.numeric(rowSums(dfm_select(dfm_mda_uni, pattern = weak_in_dfm)))
} else rep(0, ndoc(dfm_mda_uni))

# Commitment density (strong modals per 1000 alpha words)
commitment_density <- safe_density_alpha(strong_counts, mda_wc_alpha)
# Hedging density (weak modals — conceptually closer to uncertainty)
hedging_density    <- safe_density_alpha(weak_counts, mda_wc_alpha)

cat(sprintf("  Commitment (strong modal) density: mean=%.4f\n",
            mean(commitment_density, na.rm = TRUE)))
cat(sprintf("  Hedging (weak modal) density: mean=%.4f\n\n",
            mean(hedging_density, na.rm = TRUE)))


# ---- BLOCK 3: RISK TRANSPARENCY (REDESIGNED — cosine similarity to peers) ----
cat("BLOCK 3: Risk Transparency (v6.1 — boilerplate vs idiosyncratic)...\n")

# risk_transparency = 1 - cosine_sim(doc_tfidf, peer_centroid_tfidf)
# Higher = more idiosyncratic/less boilerplate disclosure.
#
# v6.1: Hierarchical fallback for small cells.
# If a sic2×year cell has < min_cosine_cell docs, fall back to sic2-only
# centroid; if sic2 also too small, fall back to year-only; then global.
# This mirrors the normalisation fallback logic and prevents noisy centroids.

min_cosine_cell <- config$params$min_cosine_cell %||% 5

risk_tfidf_mat <- as(dfm_risk_tfidf, "dgCMatrix")

risk_dv <- docvars(dfm_risk_uni)
risk_sic2 <- if ("target_primary_sic" %in% names(risk_dv)) {
  substr(as.character(risk_dv$target_primary_sic), 1, 2)
} else if ("sic2" %in% names(risk_dv)) {
  as.character(risk_dv$sic2)
} else {
  rep("00", ndoc(dfm_risk_uni))
}
risk_year <- if ("year_announced" %in% names(risk_dv)) {
  as.character(risk_dv$year_announced)
} else {
  rep("0000", ndoc(dfm_risk_uni))
}

# Hierarchical grouping: sic2×year → sic2 → year → global
risk_group_levels <- list(
  sic2_year = paste0(risk_sic2, "_", risk_year),
  sic2      = risk_sic2,
  year      = risk_year,
  global    = rep("all", length(risk_sic2))
)

# Core cosine function (single grouping vector)
cosine_sim_to_centroid <- function(tfidf_mat, groups, min_cell) {
  n <- nrow(tfidf_mat)
  sim <- rep(NA_real_, n)
  lvl <- rep(NA_character_, n)
  unique_groups <- unique(groups)

  for (g in unique_groups) {
    idx <- which(groups == g)
    if (length(idx) < min_cell) next

    group_mat <- tfidf_mat[idx, , drop = FALSE]
    centroid  <- Matrix::colMeans(group_mat)
    centroid_norm <- sqrt(sum(centroid^2))
    if (centroid_norm == 0) next

    for (i in idx) {
      if (!is.na(sim[i])) next  # already computed at finer level
      doc_vec <- tfidf_mat[i, ]
      doc_norm <- sqrt(sum(doc_vec^2))
      if (doc_norm == 0) next
      sim[i] <- as.numeric(sum(doc_vec * centroid)) / (doc_norm * centroid_norm)
    }
  }
  sim
}

# Apply hierarchical fallback
risk_cosine_sim <- rep(NA_real_, nrow(risk_tfidf_mat))
risk_cosine_level <- rep(NA_character_, nrow(risk_tfidf_mat))

for (level_name in names(risk_group_levels)) {
  remaining <- which(is.na(risk_cosine_sim))
  if (length(remaining) == 0) break

  groups_vec <- risk_group_levels[[level_name]]
  sim_attempt <- cosine_sim_to_centroid(risk_tfidf_mat, groups_vec, min_cosine_cell)

  filled <- which(!is.na(sim_attempt) & is.na(risk_cosine_sim))
  if (length(filled) > 0) {
    risk_cosine_sim[filled] <- sim_attempt[filled]
    risk_cosine_level[filled] <- level_name
  }
}

risk_transparency <- 1 - risk_cosine_sim

cat(sprintf("  min_cosine_cell: %d\n", min_cosine_cell))
cat(sprintf("  Cosine fallback usage: sic2_year=%d | sic2=%d | year=%d | global=%d | NA=%d\n",
            sum(risk_cosine_level == "sic2_year", na.rm = TRUE),
            sum(risk_cosine_level == "sic2", na.rm = TRUE),
            sum(risk_cosine_level == "year", na.rm = TRUE),
            sum(risk_cosine_level == "global", na.rm = TRUE),
            sum(is.na(risk_cosine_level))))
risk_transparency <- 1 - risk_cosine_sim  # higher = less boilerplate

cat(sprintf("  Cosine sim to centroid: mean=%.4f | sd=%.4f\n",
            mean(risk_cosine_sim, na.rm = TRUE),
            sd(risk_cosine_sim, na.rm = TRUE)))
cat(sprintf("  Risk transparency (1-sim): mean=%.4f | sd=%.4f\n",
            mean(risk_transparency, na.rm = TRUE),
            sd(risk_transparency, na.rm = TRUE)))
cat(sprintf("  NAs (singleton groups): %d\n",
            sum(is.na(risk_transparency))))

# Construct B: Legacy LM-based risk disclosure (retained for robustness)
# FIX 1 applied: use wc_alpha as denominator
risk_source_label <- config$risk_dictionary_source %||% "LM(Uncertainty + Negative)"
risk_dict <- preprocess_dict(c(lm_uncertainty, lm_negative), label = "risk_lm")

risk_terms_in_dfm <- intersect(risk_dict, featnames(dfm_risk_uni))
risk_raw_counts <- if (length(risk_terms_in_dfm) > 0) {
  as.numeric(rowSums(dfm_select(dfm_risk_uni, pattern = risk_terms_in_dfm)))
} else rep(0, ndoc(dfm_risk_uni))

# FIX 1: use wc_alpha, not wc_total
risk_disclosure_raw <- safe_density_alpha(risk_raw_counts, risk_wc_alpha)

# Also compute risk-section numeric density (specificity/structure proxy)
risk_numeric_density <- as.numeric(numeric_features$risk_numeric_density)

cat(sprintf("  LM risk dict matched: %d/%d terms\n",
            length(risk_terms_in_dfm), length(risk_dict)))
cat(sprintf("  LM risk density (per 1000 alpha): mean=%.4f\n",
            mean(risk_disclosure_raw, na.rm = TRUE)))
cat(sprintf("  Risk numeric density: mean=%.4f\n\n",
            mean(risk_numeric_density, na.rm = TRUE)))


# ---- BLOCK 4: MANAGERIAL TONE (CORRECTED FORMULA) ---------------------------
cat("BLOCK 4: Managerial Tone (v6 — corrected formula)...\n")

# FIX 3: preprocess_dict rejects multiword
pos_terms <- intersect(preprocess_dict(lm_positive, "LM_positive"), featnames(dfm_mda_uni))
neg_terms <- intersect(preprocess_dict(lm_negative, "LM_negative"), featnames(dfm_mda_uni))

pos_counts <- if (length(pos_terms) > 0) {
  as.numeric(rowSums(dfm_select(dfm_mda_uni, pattern = pos_terms)))
} else rep(0, ndoc(dfm_mda_uni))

neg_counts <- if (length(neg_terms) > 0) {
  as.numeric(rowSums(dfm_select(dfm_mda_uni, pattern = neg_terms)))
} else rep(0, ndoc(dfm_mda_uni))

# FIX 4: Always-defined tone formula with +1 smoothing
# (Pos - Neg) / (Pos + Neg + 1)
# No NA from sparse denominator; no threshold-induced missingness
tone_lm <- (pos_counts - neg_counts) / (pos_counts + neg_counts + 1)

# Also export separate densities for robustness (FIX 1: wc_alpha)
pos_density <- safe_density_alpha(pos_counts, mda_wc_alpha)
neg_density <- safe_density_alpha(neg_counts, mda_wc_alpha)

cat(sprintf("  Matched LM terms: Pos=%d | Neg=%d\n", length(pos_terms), length(neg_terms)))
cat(sprintf("  ToneLM mean: %.4f | sd: %.4f | NAs: %d (should be 0)\n",
            mean(tone_lm, na.rm = TRUE), sd(tone_lm, na.rm = TRUE),
            sum(is.na(tone_lm))))
cat(sprintf("  Pos density mean: %.4f | Neg density mean: %.4f\n\n",
            mean(pos_density, na.rm = TRUE), mean(neg_density, na.rm = TRUE)))


# ---- BLOCK 5: NORMALISATION WITH FALLBACK AUDIT ------------------------------
cat("BLOCK 5: Normalisation and fallback auditing...\n")

index_data <- tibble(
  deal_id = dfm_deal_id,
  year_announced = dv$year_announced %||% NA,
  target_primary_sic = dv$target_primary_sic %||% NA,
  sic2 = if (!is.null(dv$target_primary_sic)) {
    substr(as.character(dv$target_primary_sic), 1, 2)
  } else NA_character_,

  # Core indices (raw)
  operational_specificity_raw = operational_specificity_raw,

  # v6: sentence-based FL (primary)
  fl_sentence_share = as.numeric(fl_sentence_share),
  fl_precision_share = as.numeric(fl_precision_share),
  # v6: modal decomposition (robustness)
  commitment_density = as.numeric(commitment_density),
  hedging_density = as.numeric(hedging_density),

  # v6: risk transparency (primary = cosine; legacy = LM density)
  risk_transparency = as.numeric(risk_transparency),
  risk_disclosure_raw = as.numeric(risk_disclosure_raw),
  risk_numeric_density = as.numeric(risk_numeric_density),

  # v6: corrected tone
  tone_lm = as.numeric(tone_lm),
  pos_density = as.numeric(pos_density),
  neg_density = as.numeric(neg_density)
) %>%
  mutate(
    sic2 = ifelse(is.na(sic2) | sic2 == "NA",
                  substr(as.character(target_primary_sic), 1, 2),
                  as.character(sic2))
  )

# v6: Normalise RAW indices only. Regressions will use raw + FE (standard
# approach). Normalised versions available for descriptive analysis but NOT
# for double-demeaning in regressions (feedback point on efficiency loss).

norm_targets <- c(
  operational_specificity_raw = "operational_specificity_norm",
  fl_sentence_share           = "forward_looking_norm",
  risk_transparency           = "risk_transparency_norm",
  risk_disclosure_raw         = "risk_disclosure_raw_norm",
  tone_lm                     = "tone_lm_norm"
)

for (src in names(norm_targets)) {
  res <- normalize_hierarchical_audit(index_data, src, min_cell = min_cell)
  index_data[[norm_targets[[src]]]] <- res$norm
  index_data[[paste0(norm_targets[[src]], "_level")]] <- res$level
}

# Fallback usage summary
audit_levels <- function(level_vec) {
  tibble(level = c("sic2_year", "sic2", "year", "global", "missing")) %>%
    mutate(n = sapply(level, function(l) sum(level_vec == l, na.rm = TRUE)))
}

cat(sprintf("\n  min_cell_size=%d\n", min_cell))
cat("  Fallback usage (counts) by index:\n")
for (nm in norm_targets) {
  lvl_col <- paste0(nm, "_level")
  if (lvl_col %in% names(index_data)) {
    cat(sprintf("  - %s:\n", nm))
    print(audit_levels(index_data[[lvl_col]]))
  }
}


# ---- BLOCK 6: SAVE OUTPUT ----------------------------------------------------
cat("\nBLOCK 6: Saving final dataset...\n")

final_df <- index_data %>%
  select(
    deal_id, target_primary_sic, sic2, year_announced,
    # Raw indices
    operational_specificity_raw,
    fl_sentence_share, fl_precision_share,
    commitment_density, hedging_density,
    risk_transparency, risk_disclosure_raw, risk_numeric_density,
    tone_lm, pos_density, neg_density,
    # Normalised indices
    operational_specificity_norm, forward_looking_norm,
    risk_transparency_norm, risk_disclosure_raw_norm, tone_lm_norm,
    # Fallback levels
    ends_with("_level")
  )

saveRDS(final_df, path_out)
cat(sprintf("  %d obs x %d vars -> %s\n\n", nrow(final_df), ncol(final_df), path_out))


# ---- BLOCK 7: WRITE REPORT ---------------------------------------------------
report_lines <- c(
  "# Module 3 — Index Construction Report (v6)", "",
  paste0("Generated: ", Sys.time()),
  paste0("Observations: ", nrow(final_df)), "",

  "## v6 Changes (feedback-driven)", "",
  "### High-impact fixes applied:",
  "1. **Coherent denominators**: dictionary densities use wc_alpha; numeric densities use wc_total",
  "2. **No max_docfreq trimming** on dictionary DFMs (handled in Module 2 v4)",
  "3. **Multiword rejection**: preprocess_dict() errors on multiword entries",
  "4. **Tone formula**: (Pos-Neg)/(Pos+Neg+1), always defined, no endogenous NA",
  "5. **Modal separation**: strong (commitment) vs weak (hedging) tracked separately",
  "",
  "### New constructs:",
  "A. **Forward-looking (sentence-based)**: share of MD&A sentences with prospective markers",
  "B. **Risk transparency (cosine similarity)**: 1 - cosine_sim(doc, industry-year centroid)",
  "C. **Corrected tone + separate Pos/Neg densities**",
  "",

  "## Forward-looking intensity (v6)",
  sprintf("- Sentence markers: %d patterns", length(fl_sentence_markers)),
  sprintf("- FL sentence share: mean=%.4f | sd=%.4f",
          mean(fl_sentence_share, na.rm = TRUE),
          sd(fl_sentence_share, na.rm = TRUE)),
  sprintf("- FL precision (FL sents with numbers): mean=%.4f",
          mean(fl_precision_share, na.rm = TRUE)),
  sprintf("- Commitment density (strong modals): mean=%.4f",
          mean(commitment_density, na.rm = TRUE)),
  sprintf("- Hedging density (weak modals): mean=%.4f",
          mean(hedging_density, na.rm = TRUE)),
  "",

  "## Risk transparency (v6)",
  sprintf("- Cosine sim to industry-year centroid: mean=%.4f",
          mean(risk_cosine_sim, na.rm = TRUE)),
  sprintf("- Risk transparency (1-sim): mean=%.4f | sd=%.4f",
          mean(risk_transparency, na.rm = TRUE),
          sd(risk_transparency, na.rm = TRUE)),
  sprintf("- Legacy LM risk density (per 1000 alpha): mean=%.4f",
          mean(risk_disclosure_raw, na.rm = TRUE)),
  "",

  "## Tone (v6 — corrected)",
  sprintf("- Formula: (Pos-Neg)/(Pos+Neg+1)"),
  sprintf("- ToneLM: mean=%.4f | sd=%.4f | NAs=%d",
          mean(tone_lm, na.rm = TRUE), sd(tone_lm, na.rm = TRUE),
          sum(is.na(tone_lm))),
  "",

  "## Normalisation",
  sprintf("- min_cell_size: %d", min_cell),
  "- Levels: sic2×year → sic2 → year → global",
  "- NOTE: normalised indices are for descriptive use. Regressions should",
  "  use raw indices + FE (avoids double de-meaning efficiency loss).",
  ""
)

writeLines(report_lines, path_report)
cat(sprintf("Report: %s\n", path_report))
cat("=== MODULE 3 COMPLETE (v6) ===\n")
