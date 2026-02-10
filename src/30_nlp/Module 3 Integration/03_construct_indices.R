# ==============================================================================
# MODULE 3: DISCLOSURE QUALITY INDEX CONSTRUCTION
# ==============================================================================
# Constructs four disclosure indices:
# 1) Operational Specificity (MD&A): numeric densities + operational compounds
# 2) Forward-Looking Intensity (MD&A): project dictionary density
# 3) Risk Disclosure (Risk Factors): LM Uncertainty + Negative, TF-IDF weighted
# 4) Managerial Tone (MD&A): LM Positive/Negative, (Pos-Neg)/(Pos+Neg+1)
#
# Inputs:
# - data/interim/dfm_objects.rds (Module 2)
# - data/interim/numeric_features.rds (Module 2)
# - data/interim/cleaned_text.rds (Module 1; for denominators/metadata)
# - config/analysis_config.rds (LM dictionaries + parameters; optional)
# Output:
# - data/interim/disclosure_indices.rds
# - reports/03_index_construction_report.md
# ==============================================================================

suppressPackageStartupMessages({
  library(quanteda)
  library(dplyr)
  library(stringr)
  library(tidyr)
})

# ---- Paths (project-root relative; override via environment variables) --------
project_root <- Sys.getenv("PROJECT_ROOT", unset = ".")
path_dfm <- Sys.getenv("PATH_DFM_RDS", unset = file.path(project_root, "data", "interim", "dfm_objects.rds"))
path_numeric <- Sys.getenv("PATH_NUMERIC_RDS", unset = file.path(project_root, "data", "interim", "numeric_features.rds"))
path_cleaned <- Sys.getenv("PATH_CLEANED_RDS", unset = file.path(project_root, "data", "interim", "cleaned_text.rds"))
path_config <- Sys.getenv("PATH_ANALYSIS_CONFIG", unset = file.path(project_root, "config", "analysis_config.rds"))
path_output <- Sys.getenv("PATH_INDICES_RDS", unset = file.path(project_root, "data", "interim", "disclosure_indices.rds"))
path_report <- Sys.getenv("PATH_INDICES_REPORT", unset = file.path(project_root, "reports", "03_index_construction_report.md"))

dir.create(dirname(path_output), showWarnings = FALSE, recursive = TRUE)
dir.create(dirname(path_report), showWarnings = FALSE, recursive = TRUE)

cat("=== MODULE 3: DISCLOSURE QUALITY INDEX CONSTRUCTION ===\n")

# ---- Helpers -----------------------------------------------------------------
stop_if_missing <- function(path) {
  if (!file.exists(path)) stop("File not found: ", path)
}

safe_z <- function(x) {
  s <- stats::sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(NA_real_, length(x)))
  (x - mean(x, na.rm = TRUE)) / s
}

sic2_from_sic <- function(sic) {
  # Handles numeric SIC (drops leading zeros) by padding to 4 digits.
  sic_chr <- as.character(sic)
  sic_chr <- ifelse(grepl("^\\d+$", sic_chr), sprintf("%04d", as.integer(sic_chr)), sic_chr)
  substr(sic_chr, 1, 2)
}

# ---- Load inputs -------------------------------------------------------------
stop_if_missing(path_dfm)
stop_if_missing(path_numeric)
stop_if_missing(path_cleaned)

dfm_objects <- readRDS(path_dfm)
if (!is.list(dfm_objects)) stop("dfm_objects.rds must be a list")

numeric_features <- readRDS(path_numeric)
if (!is.data.frame(numeric_features) || !"deal_id" %in% names(numeric_features)) {
  stop("numeric_features.rds must be a data.frame with deal_id")
}

cleaned_data <- readRDS(path_cleaned)
required_clean_cols <- c("deal_id", "mda_word_count_clean", "risk_word_count_clean")
missing_clean_cols <- setdiff(required_clean_cols, names(cleaned_data))
if (length(missing_clean_cols) > 0) {
  stop("cleaned_text.rds missing required columns: ", paste(missing_clean_cols, collapse = ", "))
}

# Optional metadata for normalization
if (!all(c("target_primary_sic", "year_announced") %in% names(cleaned_data))) {
  stop("cleaned_text.rds must include target_primary_sic and year_announced for industry×year normalization")
}

# Load config if available
config <- NULL
if (file.exists(path_config)) config <- readRDS(path_config)

params <- list(
  min_cell_size = 15L,
  op_spec_w_numeric = 0.6,
  op_spec_w_operational = 0.4
)
if (!is.null(config) && is.list(config) && !is.null(config$params)) {
  params <- modifyList(params, config$params)
}

# ---- Retrieve DFMs by expected names ----------------------------------------
get_dfm <- function(name) {
  if (!name %in% names(dfm_objects)) stop("Missing DFM in dfm_objects: ", name)
  obj <- dfm_objects[[name]]
  if (!inherits(obj, "dfm")) stop("Object is not a dfm: ", name)
  obj
}

# Module 2 (revised) saves these keys; if you changed Module 2 naming, update here.
dfm_mda_uni <- get_dfm("mda_uni")
dfm_mda_compounds <- get_dfm("mda_compounds")
dfm_risk_uni <- get_dfm("risk_uni")
dfm_risk_tfidf <- get_dfm("risk_tfidf")

# ---- Merge base deal-level frame --------------------------------------------
index_data <- cleaned_data %>%
  select(deal_id, target_primary_sic, year_announced, mda_word_count_clean, risk_word_count_clean) %>%
  left_join(numeric_features, by = "deal_id")

# ---- Dictionaries ------------------------------------------------------------
# Forward-looking: use config if present, else conservative built-in list.
forward_terms <- NULL
if (!is.null(config) && is.list(config) && !is.null(config$project_dicts$forward_looking_terms)) {
  forward_terms <- config$project_dicts$forward_looking_terms
} else {
  forward_terms <- c(
    "will","shall","may","might","could","would","should","can","must",
    "expect","expects","expected","anticipate","anticipates","anticipated",
    "believe","believes","believed","intend","intends","intended",
    "plan","plans","planned","forecast","forecasts","forecasted",
    "project","projects","projected","estimate","estimates","estimated",
    "outlook","guidance","future","next","upcoming","potential","likely","unlikely"
  )
}

# LM dictionaries: require config for authoritative lists.
if (is.null(config) || is.null(config$lm) || !all(c("positive","negative","uncertainty") %in% names(config$lm))) {
  stop("analysis_config.rds missing LM dictionaries. Run create_lm_dictionaries.R first.")
}

lm_pos <- config$lm$positive
lm_neg <- config$lm$negative
lm_unc <- config$lm$uncertainty

# Risk disclosure dictionary (per plan): Uncertainty + Negative
risk_terms <- union(lm_unc, lm_neg)

# ---- 1) Operational Specificity ---------------------------------------------
# Numeric density is extracted in Module 2 as numeric_density (per 1000 words).
if (!"numeric_density" %in% names(index_data)) {
  stop("numeric_features must include numeric_density")
}

# Operational compound density from compounded DFM (Module 2).
# Use a whitelist of operational phrases if provided; else use all compounded phrases present.
op_whitelist <- NULL
if (!is.null(config) && !is.null(config$project_dicts$operational_phrases)) {
  op_whitelist <- paste0(gsub(" ", "_", config$project_dicts$operational_phrases))
}

op_features <- featnames(dfm_mda_compounds)
if (!is.null(op_whitelist)) {
  op_features <- intersect(op_features, op_whitelist)
}

op_counts <- rep(0, ndoc(dfm_mda_compounds))
if (length(op_features) > 0) {
  op_counts <- rowSums(dfm_mda_compounds[, op_features, drop = FALSE])
}

op_density <- op_counts / (docvars(dfm_mda_compounds)$mda_word_count_clean / 1000)
# If docvars missing clean word counts (should not happen with revised Module 2), fall back to index_data.
if (all(is.na(op_density))) {
  op_density <- op_counts / (index_data$mda_word_count_clean / 1000)
}

index_data$operational_specificity_raw <- params$op_spec_w_numeric * index_data$numeric_density +
  params$op_spec_w_operational * op_density

# Keep components for transparency
index_data$op_spec_numeric_density <- index_data$numeric_density
index_data$op_spec_operational_density <- op_density

# ---- 2) Forward-Looking Intensity -------------------------------------------
# Compute forward-looking density from MD&A unigram DFM (dictionary-ready).
# Note: dfm is already lowercased in Module 2.
fwd_features <- intersect(featnames(dfm_mda_uni), forward_terms)

fwd_counts <- rep(0, ndoc(dfm_mda_uni))
if (length(fwd_features) > 0) {
  fwd_counts <- rowSums(dfm_mda_uni[, fwd_features, drop = FALSE])
}

fwd_density <- fwd_counts / (docvars(dfm_mda_uni)$mda_word_count_clean / 1000)
if (all(is.na(fwd_density))) {
  fwd_density <- fwd_counts / (index_data$mda_word_count_clean / 1000)
}

index_data$forward_looking_density <- fwd_density

# ---- 3) Risk Disclosure (TF-IDF) --------------------------------------------
# Use TF-IDF weighted DFM and average TF-IDF across risk terms.
risk_features_tfidf <- intersect(featnames(dfm_risk_tfidf), risk_terms)

risk_tfidf_mean <- rep(0, ndoc(dfm_risk_tfidf))
if (length(risk_features_tfidf) > 0) {
  m <- dfm_risk_tfidf[, risk_features_tfidf, drop = FALSE]
  risk_tfidf_mean <- rowMeans(as.matrix(m), na.rm = TRUE)
}

index_data$risk_disclosure_tfidf <- risk_tfidf_mean

# Also provide simple frequency density (baseline)
risk_features_uni <- intersect(featnames(dfm_risk_uni), risk_terms)

risk_counts <- rep(0, ndoc(dfm_risk_uni))
if (length(risk_features_uni) > 0) {
  risk_counts <- rowSums(dfm_risk_uni[, risk_features_uni, drop = FALSE])
}

risk_density_simple <- risk_counts / (docvars(dfm_risk_uni)$risk_word_count_clean / 1000)
if (all(is.na(risk_density_simple))) {
  risk_density_simple <- risk_counts / (index_data$risk_word_count_clean / 1000)
}

index_data$risk_disclosure_raw <- risk_density_simple

# ---- 4) Managerial Tone (LM) ------------------------------------------------
# Counts from MD&A unigram DFM
pos_features <- intersect(featnames(dfm_mda_uni), lm_pos)
neg_features <- intersect(featnames(dfm_mda_uni), lm_neg)

pos_count <- if (length(pos_features) > 0) rowSums(dfm_mda_uni[, pos_features, drop = FALSE]) else rep(0, ndoc(dfm_mda_uni))
neg_count <- if (length(neg_features) > 0) rowSums(dfm_mda_uni[, neg_features, drop = FALSE]) else rep(0, ndoc(dfm_mda_uni))

index_data$pos_share <- pos_count / index_data$mda_word_count_clean
index_data$neg_share <- neg_count / index_data$mda_word_count_clean
index_data$tone_lm <- (pos_count - neg_count) / (pos_count + neg_count + 1)

# ---- Industry × year normalization ------------------------------------------
index_data <- index_data %>%
  mutate(
    sic2 = sic2_from_sic(target_primary_sic),
    ind_year = paste0(sic2, "_", year_announced)
  )

cell_counts <- index_data %>% count(ind_year, name = "n_obs")
index_data <- index_data %>% left_join(cell_counts, by = "ind_year") %>%
  mutate(small_cell = n_obs < params$min_cell_size)

indices_to_normalize <- c(
  "operational_specificity_raw",
  "forward_looking_density",
  "risk_disclosure_raw",
  "risk_disclosure_tfidf",
  "tone_lm",
  "pos_share",
  "neg_share"
)

for (nm in indices_to_normalize) {
  grp <- index_data %>%
    group_by(ind_year) %>%
    mutate(.z = safe_z(.data[[nm]])) %>%
    ungroup() %>%
    pull(.z)

  grand <- safe_z(index_data[[nm]])

  index_data[[paste0(nm, "_norm")]] <- ifelse(index_data$small_cell | is.na(grp), grand, grp)
}

# ---- Final dataset -----------------------------------------------------------
final <- index_data %>%
  select(
    deal_id, target_primary_sic, year_announced, sic2, ind_year, n_obs, small_cell,
    operational_specificity_raw, operational_specificity_raw_norm,
    op_spec_numeric_density, op_spec_operational_density,
    forward_looking_density, forward_looking_density_norm,
    risk_disclosure_raw, risk_disclosure_raw_norm,
    risk_disclosure_tfidf, risk_disclosure_tfidf_norm,
    tone_lm, tone_lm_norm,
    pos_share, pos_share_norm,
    neg_share, neg_share_norm
  )

saveRDS(final, path_output)

# ---- Minimal report ----------------------------------------------------------
report <- c(
  "# Index Construction Report",
  paste("Generated:", Sys.time()),
  "",
  "## Inputs",
  paste("- dfm_objects:", path_dfm),
  paste("- numeric_features:", path_numeric),
  paste("- cleaned_text:", path_cleaned),
  paste("- analysis_config:", ifelse(file.exists(path_config), path_config, "(missing)")),
  "",
  "## Sample",
  paste("- Deals:", nrow(final)),
  paste("- Industry×year cells:", dplyr::n_distinct(final$ind_year)),
  paste("- Small cells (n<", params$min_cell_size, "):", sum(final$small_cell, na.rm = TRUE)),
  "",
  "## Indices (raw) summaries",
  "```",
  capture.output(summary(final$operational_specificity_raw)),
  capture.output(summary(final$forward_looking_density)),
  capture.output(summary(final$risk_disclosure_tfidf)),
  capture.output(summary(final$tone_lm)),
  "```"
)
writeLines(report, path_report)

cat("Saved indices: ", normalizePath(path_output, winslash = "/", mustWork = FALSE), "\n", sep = "")
cat("Saved report : ", normalizePath(path_report, winslash = "/", mustWork = FALSE), "\n", sep = "")
