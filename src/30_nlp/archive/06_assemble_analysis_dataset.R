# =============================================================================
# MODULE 6: FINAL DATASET ASSEMBLY (v4)
# Purpose: Merge base M&A deal dataset with NLP disclosure indices and export
#          analysis-ready datasets + merge audits.
#
# Key design choices (defensible):
# - Join key is detected (prefers deal_id if present).
# - Key types are aligned to character (robust to leading zeros / mixed types).
# - Index dataset is reduced to "deliverables" to avoid column collisions.
# - Missingness checks are strict for core indices expected to be complete.
# - tone_lm_norm is allowed to have small missingness due to sparse denominator
#   rule (documented in Module 3 logs). We export sample flags accordingly.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

cat("=== MODULE 6: FINAL DATASET ASSEMBLY (v4) ===\n\n")

# ----------------------------
# Paths (edit if needed)
# ----------------------------
path_base <- "data/processed/deals_with_10k_text_analysis.rds"
path_idx  <- "data/interim/disclosure_indices.rds"

out_dir_final  <- "data/final"
out_dir_tables <- "output/tables"

dir.create(out_dir_final,  recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir_tables, recursive = TRUE, showWarnings = FALSE)

# ----------------------------
# BLOCK 1: Load data
# ----------------------------
cat("BLOCK 1: Loading base deals and indices...\n")
base <- readRDS(path_base)
idx  <- readRDS(path_idx)

cat(sprintf("  - Base deals rows: %d  | cols: %d\n", nrow(base), ncol(base)))
cat(sprintf("  - Indices rows   : %d  | cols: %d\n\n", nrow(idx),  ncol(idx)))

stopifnot(nrow(base) > 0, nrow(idx) > 0)

# ----------------------------
# BLOCK 2: Identify merge key
# ----------------------------
cat("BLOCK 2: Identifying merge key...\n")
candidate_keys <- c("deal_id", "DealID", "dealid", "id")
merge_key <- candidate_keys[candidate_keys %in% intersect(names(base), names(idx))][1]

if (is.na(merge_key) || length(merge_key) == 0) {
  stop("No common merge key found between base and indices. Expected one of: ",
       paste(candidate_keys, collapse = ", "))
}
cat(sprintf("  - Using merge key: %s\n\n", merge_key))

# ----------------------------
# BLOCK 2B: Key type + uniqueness audit
# ----------------------------
cat("BLOCK 2B: Auditing key types and uniqueness...\n")

key_audit <- tibble(
  dataset = c("base", "idx"),
  key     = merge_key,
  type    = c(class(base[[merge_key]])[1], class(idx[[merge_key]])[1]),
  n_rows  = c(nrow(base), nrow(idx)),
  n_unique_key = c(n_distinct(base[[merge_key]]), n_distinct(idx[[merge_key]])),
  n_missing_key = c(sum(is.na(base[[merge_key]])), sum(is.na(idx[[merge_key]])))
)

write_csv(key_audit, file.path(out_dir_tables, "module6_key_type_alignment.csv"))

cat("  - Saved key type audit: output/tables/module6_key_type_alignment.csv\n")

if (key_audit$n_missing_key[1] > 0 || key_audit$n_missing_key[2] > 0) {
  stop("Merge key has missing values in base and/or idx. Fix upstream before merging.")
}

if (key_audit$n_unique_key[1] != nrow(base)) stop("Base key is not unique (expected 1 row per key).")
if (key_audit$n_unique_key[2] != nrow(idx))  stop("Indices key is not unique (expected 1 row per key).")

cat("  - Indices key uniqueness: OK (1 row per key)\n")
cat("  - Base key uniqueness: OK (1 row per key)\n\n")

# ----------------------------
# BLOCK 3: Align key types (to character)
# ----------------------------
cat("BLOCK 3: Aligning key types...\n")
base[[merge_key]] <- as.character(base[[merge_key]])
idx[[merge_key]]  <- as.character(idx[[merge_key]])
cat(sprintf("  - base[%s] -> character\n", merge_key))
cat(sprintf("  - idx [%s] -> character\n\n", merge_key))

# ----------------------------
# BLOCK 3B: Select index deliverables
# - Keep: *_norm, *_level, and raw constructs used in Module 5
# - Drop: normalization grouping metadata that may overlap with base
# ----------------------------
cat("BLOCK 3B: Selecting index deliverables (avoid key collisions)...\n")

deliverable_patterns <- c(
  "_norm$", "_level$",
  "^risk_disclosure_tfidf$", "^tone_lm$",
  "^forward_looking_density$", "^forward_looking_intensity$", "^forward_looking$",
  "^operational_specificity$", "^operational_specificity_raw$", "^operational_specificity_composite$"
)

keep_cols <- names(idx)[
  names(idx) == merge_key |
    Reduce(`|`, lapply(deliverable_patterns, function(p) str_detect(names(idx), p)))
]

idx_sel <- idx %>% select(all_of(unique(keep_cols)))
cat(sprintf("  - idx columns kept: %d/%d\n\n", ncol(idx_sel), ncol(idx)))

# ----------------------------
# BLOCK 4: Merge
# ----------------------------
cat("BLOCK 4: Merging datasets...\n")
merged <- base %>% left_join(idx_sel, by = merge_key)

# ----------------------------
# BLOCK 5: Post-merge checks
# ----------------------------
cat("\nBLOCK 5: Post-merge checks...\n")

if (nrow(merged) != nrow(base)) stop("Row count not preserved after merge.")
cat("  - Row count preserved: OK\n")

# Define core indices expected for empirical models
core_norm <- c(
  "operational_specificity_norm",
  "forward_looking_norm",
  "risk_disclosure_tfidf_norm",
  "tone_lm_norm"
)

core_present <- core_norm[core_norm %in% names(merged)]
missing_core <- setdiff(core_norm, core_present)

if (length(missing_core) > 0) {
  stop("Missing core normalized indices in merged dataset: ",
       paste(missing_core, collapse = ", "))
}

missingness_tbl <- tibble(
  variable = core_present,
  n = nrow(merged),
  n_na = sapply(core_present, function(v) sum(is.na(merged[[v]]))),
  na_share = n_na / n
)

write_csv(missingness_tbl, file.path(out_dir_tables, "module6_index_missingness.csv"))
cat("  - Saved missingness table: output/tables/module6_index_missingness.csv\n")

# Strict missingness rules:
# - The first three indices should be complete.
# - tone_lm_norm may have small missingness due to sparse denom rule (Module 3).
strict_vars <- c("operational_specificity_norm", "forward_looking_norm", "risk_disclosure_tfidf_norm")
strict_tbl <- missingness_tbl %>% filter(variable %in% strict_vars)

if (any(strict_tbl$na_share > 0)) {
  stop("Strict core indices have missing values (unexpected). See module6_index_missingness.csv.")
}

tone_tbl <- missingness_tbl %>% filter(variable == "tone_lm_norm")
tone_na_share <- if (nrow(tone_tbl) == 1) tone_tbl$na_share else 0

# Allow up to 1% missingness for tone
if (tone_na_share > 0.01) {
  stop("tone_lm_norm missingness > 1% (unexpectedly high). See module6_index_missingness.csv.")
}

cat(sprintf("  - Strict core indices missingness: OK (0%%)\n"))
cat(sprintf("  - tone_lm_norm missingness: %.3f%% (allowed <= 1%%)\n", 100 * tone_na_share))

# ----------------------------
# BLOCK 5B: Outcome variable construction (action plan Step 6.1)
# ----------------------------
cat("\nBLOCK 5B: Constructing outcome variables...\n")

# Premium aliases (short names for regression code)
if ("premium_paid_1_day_prior_to_announcement" %in% names(merged)) {
  merged$premium_1d <- as.numeric(merged$premium_paid_1_day_prior_to_announcement)
  cat("  - premium_1d created\n")
}
if ("premium_paid_1_week_prior_to_announcement" %in% names(merged)) {
  merged$premium_1w <- as.numeric(merged$premium_paid_1_week_prior_to_announcement)
  cat("  - premium_1w created\n")
}
if ("premium_paid_4_weeks_prior_to_announcement" %in% names(merged)) {
  merged$premium_4w <- as.numeric(merged$premium_paid_4_weeks_prior_to_announcement)
  cat("  - premium_4w created\n")
}

# Completion dummy (Completed = 1, Withdrawn = 0, else NA)
if ("deal_status" %in% names(merged)) {
  merged$completion <- case_when(
    tolower(merged$deal_status) == "completed"  ~ 1L,
    tolower(merged$deal_status) == "withdrawn"  ~ 0L,
    TRUE ~ NA_integer_
  )
  cat(sprintf("  - completion: Completed=%d | Withdrawn=%d | NA=%d\n",
              sum(merged$completion == 1L, na.rm = TRUE),
              sum(merged$completion == 0L, na.rm = TRUE),
              sum(is.na(merged$completion))))
}

# Time to close (days between announcement and effective date)
# Prefer existing 'time_to_close' if present; else derive from
# 'number_of_days_between_date_announced_and_date_effective'.
if (!"time_to_close" %in% names(merged) &&
    "number_of_days_between_date_announced_and_date_effective" %in% names(merged)) {
  merged$time_to_close <- as.numeric(
    merged$number_of_days_between_date_announced_and_date_effective
  )
  cat("  - time_to_close derived from number_of_days_...\n")
} else if ("time_to_close" %in% names(merged)) {
  merged$time_to_close <- as.numeric(merged$time_to_close)
  cat("  - time_to_close already present (coerced to numeric)\n")
}

# ----------------------------
# BLOCK 5C: Fixed-effect identifiers (action plan Step 6.4)
# ----------------------------
cat("\nBLOCK 5C: Constructing FE identifiers...\n")

if ("target_primary_sic" %in% names(merged)) {
  merged$sic2 <- substr(as.character(merged$target_primary_sic), 1, 2)
} else if (!"sic2" %in% names(merged)) {
  warning("Cannot construct sic2: target_primary_sic not found.")
}

if ("sic2" %in% names(merged) && "year_announced" %in% names(merged)) {
  merged$ind_year_fe <- paste0(merged$sic2, "_", merged$year_announced)
  cat(sprintf("  - ind_year_fe: %d unique cells\n",
              n_distinct(merged$ind_year_fe)))
}

# ----------------------------
# BLOCK 5D: Analysis sample flags (action plan Step 6.3)
# ----------------------------
cat("\nBLOCK 5D: Creating analysis sample flags...\n")

merged <- merged %>%
  mutate(
    # Core NLP indices available
    sample_core = if_else(
      !is.na(operational_specificity_norm) &
        !is.na(forward_looking_norm) &
        !is.na(risk_disclosure_tfidf_norm),
      1L, 0L),
    sample_tone = if_else(sample_core == 1L & !is.na(tone_lm_norm), 1L, 0L),

    # Premium analysis: need premium and valid share price
    sample_premium = if_else(
      sample_core == 1L &
        !is.na(premium_1d) &
        !is.na(target_share_price_1_day_prior_to_announcement_usd) &
        target_share_price_1_day_prior_to_announcement_usd > 0,
      1L, 0L),

    # Completion analysis: need terminal outcome
    sample_completion = if_else(
      sample_core == 1L & !is.na(completion),
      1L, 0L),

    # Duration analysis: conditional on completion
    sample_duration = if_else(
      sample_core == 1L &
        !is.na(completion) & completion == 1L &
        !is.na(time_to_close) & time_to_close > 0,
      1L, 0L)
  )

cat(sprintf("  - sample_core:       %d / %d\n", sum(merged$sample_core), nrow(merged)))
cat(sprintf("  - sample_tone:       %d / %d\n", sum(merged$sample_tone), nrow(merged)))
cat(sprintf("  - sample_premium:    %d / %d\n", sum(merged$sample_premium), nrow(merged)))
cat(sprintf("  - sample_completion: %d / %d\n", sum(merged$sample_completion), nrow(merged)))
cat(sprintf("  - sample_duration:   %d / %d\n", sum(merged$sample_duration), nrow(merged)))

# ----------------------------
# BLOCK 5E: Outlier flags (action plan Step 6.5)
# ----------------------------
cat("\nBLOCK 5E: Flagging outliers (|z| > 5 SD)...\n")

index_vars_for_outliers <- c(
  "operational_specificity_norm", "forward_looking_norm",
  "risk_disclosure_tfidf_norm", "tone_lm_norm"
)

for (idx_var in index_vars_for_outliers) {
  if (!idx_var %in% names(merged)) next
  flag_col <- paste0(idx_var, "_outlier")
  vals <- merged[[idx_var]]
  mu <- mean(vals, na.rm = TRUE)
  sigma <- sd(vals, na.rm = TRUE)
  merged[[flag_col]] <- if_else(
    !is.na(vals) & abs(vals - mu) > 5 * sigma, 1L, 0L
  )
  n_out <- sum(merged[[flag_col]] == 1L, na.rm = TRUE)
  cat(sprintf("  - %s: %d outliers flagged\n", idx_var, n_out))
}

# ----------------------------
# Merge audit
# ----------------------------
merge_audit <- tibble(
  n_rows = nrow(merged),
  sample_core = sum(merged$sample_core == 1L),
  sample_tone = sum(merged$sample_tone == 1L),
  sample_premium = sum(merged$sample_premium == 1L),
  sample_completion = sum(merged$sample_completion == 1L),
  sample_duration = sum(merged$sample_duration == 1L),
  tone_missing_in_core = sum(merged$sample_core == 1L & is.na(merged$tone_lm_norm))
)
write_csv(merge_audit, file.path(out_dir_tables, "module6_merge_audit.csv"))
cat("\n  - Saved merge audit: output/tables/module6_merge_audit.csv\n")

# ----------------------------
# BLOCK 6: Save final datasets
# ----------------------------
cat("\nBLOCK 6: Saving final dataset...\n")

# Full RDS (includes raw text for traceability)
saveRDS(merged, file.path(out_dir_final, "analysis_dataset.rds"))
cat(sprintf("  - Saved (RDS, full): %s\n", file.path(out_dir_final, "analysis_dataset.rds")))

# CSV export: EXCLUDE raw text columns (mda_text, risk_factors_text)
# These inflate the CSV to ~350 MB and are not needed for regression.
text_cols <- c("mda_text", "risk_factors_text")
merged_csv <- merged %>% select(-any_of(text_cols))
write_csv(merged_csv, file.path(out_dir_final, "analysis_dataset.csv"))
cat(sprintf("  - Saved (CSV, no text): %s (%d cols)\n",
            file.path(out_dir_final, "analysis_dataset.csv"), ncol(merged_csv)))

# ----------------------------
# BLOCK 7: Data dictionary (action plan deliverable)
# ----------------------------
cat("\nBLOCK 7: Generating data dictionary...\n")

make_source <- function(col_name) {
  case_when(
    col_name %in% c("premium_1d","premium_1w","premium_4w","completion",
                    "sic2","ind_year_fe",
                    "sample_core","sample_tone","sample_premium",
                    "sample_completion","sample_duration") ~ "Module 6 (derived)",
    str_detect(col_name, "_outlier$") ~ "Module 6 (outlier flag)",
    str_detect(col_name, "_norm$|_norm_level$|_raw$|_tfidf$|^tone_lm$|forward_looking_density") ~ "Module 3 (NLP index)",
    TRUE ~ "Base dataset (SDC / EDGAR)"
  )
}

make_construction <- function(col_name) {
  case_when(
    col_name == "premium_1d" ~ "= premium_paid_1_day_prior_to_announcement (numeric)",
    col_name == "premium_1w" ~ "= premium_paid_1_week_prior_to_announcement (numeric)",
    col_name == "premium_4w" ~ "= premium_paid_4_weeks_prior_to_announcement (numeric)",
    col_name == "completion" ~ "1 if deal_status=Completed, 0 if Withdrawn, else NA",
    col_name == "sic2" ~ "substr(target_primary_sic, 1, 2)",
    col_name == "ind_year_fe" ~ "paste0(sic2, '_', year_announced)",
    col_name == "sample_premium" ~ "sample_core=1 & premium_1d non-NA & share_price>0",
    col_name == "sample_completion" ~ "sample_core=1 & completion non-NA",
    col_name == "sample_duration" ~ "sample_core=1 & completion=1 & time_to_close>0",
    col_name == "sample_core" ~ "op_spec, fwd_looking, risk_disclosure all non-NA",
    col_name == "sample_tone" ~ "sample_core=1 & tone_lm_norm non-NA",
    str_detect(col_name, "_outlier$") ~ "1 if |value - mean| > 5*SD, else 0",
    str_detect(col_name, "_norm$") ~ "Z-scored (hierarchical: sic2xyear > sic2 > year > global)",
    str_detect(col_name, "_norm_level$") ~ "Normalisation level used (sic2_year/sic2/year/global/missing)",
    col_name == "operational_specificity_raw" ~ "0.5*numeric_density + 0.5*compound_density (per 1000w)",
    col_name == "forward_looking_density" ~ "(modal_matches + regex_matches) per 1000 MD&A words",
    col_name == "risk_disclosure_tfidf" ~ "LM(uncertainty+negative) TF-IDF mass / total TF-IDF mass",
    col_name == "tone_lm" ~ "(pos-neg)/(pos+neg) if denom>=10, else NA",
    TRUE ~ "From source (see base dataset documentation)"
  )
}

data_dict <- tibble(
  column_name  = names(merged),
  type         = sapply(merged, function(x) class(x)[1]),
  source       = make_source(names(merged)),
  construction = make_construction(names(merged)),
  n_non_na     = sapply(merged, function(x) sum(!is.na(x))),
  n_na         = sapply(merged, function(x) sum(is.na(x)))
)

write_csv(data_dict, file.path(out_dir_tables, "data_dictionary_final.csv"))
cat(sprintf("  - Saved: %s (%d columns documented)\n",
            file.path(out_dir_tables, "data_dictionary_final.csv"), nrow(data_dict)))

# ----------------------------
# BLOCK 8: Final dataset report (action plan deliverable)
# ----------------------------
cat("\nBLOCK 8: Writing final dataset report...\n")

out_dir_reports <- "reports"
dir.create(out_dir_reports, recursive = TRUE, showWarnings = FALSE)

report <- c(
  "# Module 6 \u2014 Final Dataset Report (v5)",
  "",
  paste0("Generated: ", Sys.time()),
  "",
  "## Dataset dimensions",
  sprintf("- Rows: %d", nrow(merged)),
  sprintf("- Columns (RDS): %d", ncol(merged)),
  sprintf("- Columns (CSV, no text): %d", ncol(merged_csv)),
  "",
  "## Sample sizes",
  sprintf("- sample_core (all 3 strict indices non-NA): %d", sum(merged$sample_core)),
  sprintf("- sample_tone (core + tone non-NA): %d", sum(merged$sample_tone)),
  sprintf("- sample_premium (core + premium_1d + valid price): %d", sum(merged$sample_premium)),
  sprintf("- sample_completion (core + terminal outcome): %d", sum(merged$sample_completion)),
  sprintf("- sample_duration (completed + time_to_close > 0): %d", sum(merged$sample_duration)),
  "",
  "## Outcome variables"
)

if ("premium_1d" %in% names(merged)) {
  report <- c(report,
    sprintf("- premium_1d: n=%d | mean=%.2f | median=%.2f | sd=%.2f",
            sum(!is.na(merged$premium_1d)),
            mean(merged$premium_1d, na.rm=TRUE),
            median(merged$premium_1d, na.rm=TRUE),
            sd(merged$premium_1d, na.rm=TRUE)))
}
if ("completion" %in% names(merged)) {
  report <- c(report,
    sprintf("- completion: Completed=%d | Withdrawn=%d | NA=%d",
            sum(merged$completion == 1L, na.rm=TRUE),
            sum(merged$completion == 0L, na.rm=TRUE),
            sum(is.na(merged$completion))))
}
if ("time_to_close" %in% names(merged)) {
  ttc <- merged$time_to_close[merged$completion == 1L & !is.na(merged$time_to_close)]
  report <- c(report,
    sprintf("- time_to_close (completed only): n=%d | mean=%.1f | median=%.1f",
            length(ttc), mean(ttc, na.rm=TRUE), median(ttc, na.rm=TRUE)))
}

report <- c(report, "", "## Normalisation fallback shares")
level_cols <- names(merged)[str_detect(names(merged), "_norm_level$")]
for (lc in level_cols) {
  tbl <- table(merged[[lc]], useNA = "ifany")
  shares <- paste(names(tbl), "=", tbl, collapse = " | ")
  report <- c(report, sprintf("- %s: %s", lc, shares))
}

report <- c(report, "", "## Outlier counts (|z| > 5 SD)")
outlier_cols <- names(merged)[str_detect(names(merged), "_outlier$")]
for (oc in outlier_cols) {
  report <- c(report, sprintf("- %s: %d", oc, sum(merged[[oc]] == 1L, na.rm=TRUE)))
}

report <- c(report, "",
  "## Index missingness (core normalised)",
  capture.output(print(missingness_tbl)),
  "",
  "## Notes",
  "- Raw text columns (mda_text, risk_factors_text) excluded from CSV but retained in RDS.",
  "- Outlier flags are informational; no observations dropped.",
  "- tone_lm_norm has small missingness due to sparse-denominator rule (pos+neg < 10)."
)

writeLines(report, file.path(out_dir_reports, "06_final_dataset_report.md"))
cat(sprintf("  - Saved: %s\n", file.path(out_dir_reports, "06_final_dataset_report.md")))

cat("\n=== MODULE 6 COMPLETE (v5) ===\n")
