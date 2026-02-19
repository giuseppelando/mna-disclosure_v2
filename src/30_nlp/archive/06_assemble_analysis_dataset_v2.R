# =============================================================================
# MODULE 6: FINAL DATASET ASSEMBLY (v6 — REVISED)
# =============================================================================
# Purpose: Merge base M&A deal dataset with NLP disclosure indices and export
#          analysis-ready datasets + merge audits.
#
# v6 CHANGES:
#   1. PREMIUM: Use premium_1d as primary; keep premium_4w for robustness only
#   2. TIME_TO_CLOSE: Always use SDC's number_of_days_between_* (fixes sign bug)
#   3. COLUMN CLEANUP: Remove redundant flags, duplicate columns
#   4. Streamlined output with clear variable selection
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

cat("=== MODULE 6: FINAL DATASET ASSEMBLY (v6 — REVISED) ===\n\n")

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
# BLOCK 5B: Outcome variable construction (REVISED v6)
# ----------------------------
cat("\nBLOCK 5B: Constructing outcome variables...\n")

# -----------------------------------------------------------------------------
# PREMIUM DECISION (v6):
# - Use premium_1d as PRIMARY outcome (standard in M&A literature)
# - Keep premium_4w for ROBUSTNESS checks only (captures run-up effects)
# - DROP premium_1w (adds no value between 1d and 4w)
# -----------------------------------------------------------------------------
if ("premium_paid_1_day_prior_to_announcement" %in% names(merged)) {
  merged$premium <- as.numeric(merged$premium_paid_1_day_prior_to_announcement)
  cat("  - premium (=premium_1d) created as PRIMARY outcome\n")
}
if ("premium_paid_4_weeks_prior_to_announcement" %in% names(merged)) {
  merged$premium_4w <- as.numeric(merged$premium_paid_4_weeks_prior_to_announcement)
  cat("  - premium_4w created for robustness\n")
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

# -----------------------------------------------------------------------------
# TIME_TO_CLOSE FIX (v6):
# ALWAYS use the SDC-provided column to avoid date subtraction errors.
# NOTE: SDC uses -999 as sentinel value for withdrawn deals (no effective date).
#       We convert -999 to NA since duration is undefined for non-completed deals.
# -----------------------------------------------------------------------------
if ("number_of_days_between_date_announced_and_date_effective" %in% names(merged)) {
  raw_ttc <- as.numeric(merged$number_of_days_between_date_announced_and_date_effective)
  
  # Count sentinel values before conversion
  n_sentinel <- sum(raw_ttc == -999, na.rm = TRUE)
  
  # Convert -999 sentinel to NA (withdrawn deals have no effective date)
  merged$time_to_close <- if_else(raw_ttc == -999, NA_real_, raw_ttc)
  
  # Summary stats (valid observations only)
  valid_ttc <- merged$time_to_close[!is.na(merged$time_to_close) & merged$time_to_close > 0]
  
  cat(sprintf("  - time_to_close: n_valid=%d | n_sentinel(-999)=%d | mean=%.1f | median=%.1f\n",
              length(valid_ttc),
              n_sentinel,
              mean(valid_ttc, na.rm = TRUE),
              median(valid_ttc, na.rm = TRUE)))
} else {
  warning("SDC duration column not found; time_to_close may be incorrect.")
}

# ----------------------------
# BLOCK 5C: Fixed-effect identifiers
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
# BLOCK 5D: Analysis sample flags
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
        !is.na(premium) &
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
# BLOCK 5E: Outlier flags (|z| > 5 SD)
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

# =============================================================================
# BLOCK 6: COLUMN CLEANUP (NEW in v6)
# =============================================================================
cat("\nBLOCK 6: Selecting final columns (removing redundant)...\n")

# -----------------------------------------------------------------------------
# COLUMNS TO DROP:
# 1. Redundant ID columns: logical_deal_id (deal_id is the key)
# 2. Redundant outcome columns: keep premium (1d) and premium_4w; drop premium_1w
# 3. Redundant flags: flag_has_*, has_*, deal_completed, deal_outcome_terminal
# 4. Original long-named premium columns (aliased to premium/premium_4w)
# 5. Original long-named duration column (replaced by corrected time_to_close)
# -----------------------------------------------------------------------------

cols_to_drop <- c(

  # Redundant ID
  "logical_deal_id",
  

  # Original long-named premium columns (aliased)
  "premium_paid_1_day_prior_to_announcement",
  "premium_paid_1_week_prior_to_announcement",
  "premium_paid_4_weeks_prior_to_announcement",
  
  # Redundant flags (QC artifacts, derivable from data)
  "flag_has_deal_id",
  "flag_has_target_name",
  "flag_has_announce_date",
  "flag_terminal_outcome",
  "flag_has_premium_inputs",
  "has_premium",
  "has_premium_1d",
  "has_premium_4w",
  "has_completion_outcome",
  "has_ticker",
  "has_cusip",
  
  # Redundant outcome indicators (replaced by completion)
  "deal_completed",
  "deal_outcome_terminal"
)

# Only drop columns that exist
cols_to_drop_present <- cols_to_drop[cols_to_drop %in% names(merged)]

cat(sprintf("  - Dropping %d redundant columns:\n", length(cols_to_drop_present)))
for (col in cols_to_drop_present) {
  cat(sprintf("      • %s\n", col))
}

merged_clean <- merged %>% select(-any_of(cols_to_drop_present))

cat(sprintf("  - Columns before cleanup: %d\n", ncol(merged)))
cat(sprintf("  - Columns after cleanup:  %d\n", ncol(merged_clean)))

# ----------------------------
# BLOCK 7: Save final datasets
# ----------------------------
cat("\nBLOCK 7: Saving final dataset...\n")

# Full RDS (includes raw text for traceability)
saveRDS(merged_clean, file.path(out_dir_final, "analysis_dataset.rds"))
cat(sprintf("  - Saved (RDS, full): %s\n", file.path(out_dir_final, "analysis_dataset.rds")))

# CSV export: EXCLUDE raw text columns (mda_text, risk_factors_text)
# These inflate the CSV to ~350 MB and are not needed for regression.
text_cols <- c("mda_text", "risk_factors_text")
merged_csv <- merged_clean %>% select(-any_of(text_cols))
write_csv(merged_csv, file.path(out_dir_final, "analysis_dataset.csv"))
cat(sprintf("  - Saved (CSV, no text): %s (%d cols)\n",
            file.path(out_dir_final, "analysis_dataset.csv"), ncol(merged_csv)))

# ----------------------------
# BLOCK 8: Data dictionary
# ----------------------------
cat("\nBLOCK 8: Generating data dictionary...\n")

make_source <- function(col_name) {
  case_when(
    col_name %in% c("premium", "premium_4w", "completion", "time_to_close",
                    "sic2", "ind_year_fe",
                    "sample_core", "sample_tone", "sample_premium",
                    "sample_completion", "sample_duration") ~ "Module 6 (derived)",
    str_detect(col_name, "_outlier$") ~ "Module 6 (outlier flag)",
    str_detect(col_name, "_norm$|_norm_level$|_raw$|_tfidf$|^tone_lm$|forward_looking_density") ~ "Module 3 (NLP index)",
    TRUE ~ "Base dataset (SDC / EDGAR)"
  )
}

make_construction <- function(col_name) {
  case_when(
    col_name == "premium" ~ "= premium_paid_1_day_prior_to_announcement (PRIMARY)",
    col_name == "premium_4w" ~ "= premium_paid_4_weeks_prior_to_announcement (robustness)",
    col_name == "completion" ~ "1 if deal_status=Completed, 0 if Withdrawn, else NA",
    col_name == "time_to_close" ~ "= number_of_days_between_date_announced_and_date_effective",
    col_name == "sic2" ~ "substr(target_primary_sic, 1, 2)",
    col_name == "ind_year_fe" ~ "paste0(sic2, '_', year_announced)",
    col_name == "sample_premium" ~ "sample_core=1 & premium non-NA & share_price>0",
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
  column_name  = names(merged_clean),
  type         = sapply(merged_clean, function(x) class(x)[1]),
  source       = make_source(names(merged_clean)),
  construction = make_construction(names(merged_clean)),
  n_non_na     = sapply(merged_clean, function(x) sum(!is.na(x))),
  n_na         = sapply(merged_clean, function(x) sum(is.na(x)))
)

write_csv(data_dict, file.path(out_dir_tables, "data_dictionary_final.csv"))
cat(sprintf("  - Saved: %s (%d columns documented)\n",
            file.path(out_dir_tables, "data_dictionary_final.csv"), nrow(data_dict)))

# ----------------------------
# BLOCK 9: Final dataset report
# ----------------------------
cat("\nBLOCK 9: Writing final dataset report...\n")

out_dir_reports <- "reports"
dir.create(out_dir_reports, recursive = TRUE, showWarnings = FALSE)

report <- c(
  "# Module 6 — Final Dataset Report (v6 REVISED)",
  "",
  paste0("Generated: ", Sys.time()),
  "",
  "## v6 Changes",
  "- **Premium**: `premium` (=premium_1d) is PRIMARY outcome; `premium_4w` for robustness",
  "- **time_to_close**: Now uses SDC's `number_of_days_between_*` (fixes negative values)",
  "- **Column cleanup**: Removed 17 redundant columns (flags, duplicates)",
  "",
  "## Dataset dimensions",
  sprintf("- Rows: %d", nrow(merged_clean)),
  sprintf("- Columns (RDS): %d", ncol(merged_clean)),
  sprintf("- Columns (CSV, no text): %d", ncol(merged_csv)),
  "",
  "## Sample sizes",
  sprintf("- sample_core (all 3 strict indices non-NA): %d", sum(merged_clean$sample_core)),
  sprintf("- sample_tone (core + tone non-NA): %d", sum(merged_clean$sample_tone)),
  sprintf("- sample_premium (core + premium + valid price): %d", sum(merged_clean$sample_premium)),

  sprintf("- sample_completion (core + terminal outcome): %d", sum(merged_clean$sample_completion)),
  sprintf("- sample_duration (completed + time_to_close > 0): %d", sum(merged_clean$sample_duration)),
  "",
  "## Outcome variables"
)

if ("premium" %in% names(merged_clean)) {
  report <- c(report,
              sprintf("- premium (1d): n=%d | mean=%.2f | median=%.2f | sd=%.2f",
                      sum(!is.na(merged_clean$premium)),
                      mean(merged_clean$premium, na.rm = TRUE),
                      median(merged_clean$premium, na.rm = TRUE),
                      sd(merged_clean$premium, na.rm = TRUE)))
}
if ("premium_4w" %in% names(merged_clean)) {
  report <- c(report,
              sprintf("- premium_4w: n=%d | mean=%.2f | median=%.2f | sd=%.2f",
                      sum(!is.na(merged_clean$premium_4w)),
                      mean(merged_clean$premium_4w, na.rm = TRUE),
                      median(merged_clean$premium_4w, na.rm = TRUE),
                      sd(merged_clean$premium_4w, na.rm = TRUE)))
}
if ("completion" %in% names(merged_clean)) {
  report <- c(report,
              sprintf("- completion: Completed=%d | Withdrawn=%d | NA=%d",
                      sum(merged_clean$completion == 1L, na.rm = TRUE),
                      sum(merged_clean$completion == 0L, na.rm = TRUE),
                      sum(is.na(merged_clean$completion))))
}
if ("time_to_close" %in% names(merged_clean)) {
  ttc <- merged_clean$time_to_close[merged_clean$completion == 1L & 
                                      !is.na(merged_clean$time_to_close) & 
                                      merged_clean$time_to_close > 0]
  report <- c(report,
              sprintf("- time_to_close (completed, >0): n=%d | mean=%.1f days | median=%.1f days",
                      length(ttc), mean(ttc, na.rm = TRUE), median(ttc, na.rm = TRUE)))
}

report <- c(report, "", "## Normalisation fallback shares")
level_cols <- names(merged_clean)[str_detect(names(merged_clean), "_norm_level$")]
for (lc in level_cols) {
  tbl <- table(merged_clean[[lc]], useNA = "ifany")
  shares <- paste(names(tbl), "=", tbl, collapse = " | ")
  report <- c(report, sprintf("- %s: %s", lc, shares))
}

report <- c(report, "", "## Outlier counts (|z| > 5 SD)")
outlier_cols <- names(merged_clean)[str_detect(names(merged_clean), "_outlier$")]
for (oc in outlier_cols) {
  report <- c(report, sprintf("- %s: %d", oc, sum(merged_clean[[oc]] == 1L, na.rm = TRUE)))
}

report <- c(report, "",
            "## Index missingness (core normalised)",
            capture.output(print(missingness_tbl)),
            "",
            "## Columns removed in v6",
            paste0("- ", paste(cols_to_drop_present, collapse = ", ")),
            "",
            "## Notes",
            "- Raw text columns (mda_text, risk_factors_text) excluded from CSV but retained in RDS.",
            "- Outlier flags are informational; no observations dropped.",
            "- tone_lm_norm has small missingness due to sparse-denominator rule (pos+neg < 10).",
            "- Premium uses 1-day prior price (standard in M&A literature); 4-week for robustness."
)

writeLines(report, file.path(out_dir_reports, "06_final_dataset_report.md"))
cat(sprintf("  - Saved: %s\n", file.path(out_dir_reports, "06_final_dataset_report.md")))

cat("\n=== MODULE 6 COMPLETE (v6 REVISED) ===\n")
cat(sprintf("Final dataset: %d rows × %d columns\n", nrow(merged_clean), ncol(merged_clean)))
cat("Ready for econometric analysis.\n")
