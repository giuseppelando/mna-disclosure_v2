# =============================================================================
# MODULE 6: FINAL DATASET ASSEMBLY (v6)
# =============================================================================
# Purpose: Merge base M&A deal dataset with NLP disclosure indices (v6) and
#          export analysis-ready datasets + merge audits.
#
# v6 CHANGES (aligned with Module 3 v6):
#   1. Updated index column names for v6 constructs:
#      - fl_sentence_share (primary FL), risk_transparency (primary risk)
#      - tone_lm (corrected formula, always defined — no sparse NA)
#      - commitment_density, hedging_density (modal decomposition)
#   2. tone_lm_norm is now always complete (no denom>=10 missingness).
#      The strict check for tone missingness is relaxed accordingly.
#   3. sample_core includes the v6 primary indices.
#   4. Data dictionary updated with v6 variable definitions.
#   5. Raw indices exported for regression (NOT normalised — avoids
#      double de-meaning with FE, per feedback).
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

cat("=== MODULE 6: FINAL DATASET ASSEMBLY (v6) ===\n\n")

# ---- Paths ----
path_base <- "data/processed/deals_with_10k_text_analysis.rds"
path_idx  <- "data/interim/disclosure_indices.rds"

out_dir_final  <- "data/final"
out_dir_tables <- "output/tables"
out_dir_reports <- "reports"

dir.create(out_dir_final,  recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir_tables, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir_reports, recursive = TRUE, showWarnings = FALSE)

# ---- BLOCK 1: Load ----
cat("BLOCK 1: Loading data...\n")
base <- readRDS(path_base)
idx  <- readRDS(path_idx)
cat(sprintf("  - Base: %d × %d | Indices: %d × %d\n\n", nrow(base), ncol(base), nrow(idx), ncol(idx)))

# ---- BLOCK 2: Merge key ----
cat("BLOCK 2: Merge key...\n")
candidate_keys <- c("deal_id", "DealID", "dealid", "id")
merge_key <- candidate_keys[candidate_keys %in% intersect(names(base), names(idx))][1]
if (is.na(merge_key)) stop("No common merge key found.")
cat(sprintf("  - Key: %s\n\n", merge_key))

# Key audit
base[[merge_key]] <- as.character(base[[merge_key]])
idx[[merge_key]]  <- as.character(idx[[merge_key]])

stopifnot(n_distinct(base[[merge_key]]) == nrow(base))
stopifnot(n_distinct(idx[[merge_key]])  == nrow(idx))

# ---- BLOCK 3: Select index deliverables ----
cat("BLOCK 3: Selecting index deliverables...\n")

# v6: explicit list of columns to keep from indices
idx_keep_cols <- c(
  merge_key,
  # Raw indices (for regression — use raw + FE, not normalised)
  "operational_specificity_raw",
  "fl_sentence_share", "fl_precision_share",
  "commitment_density", "hedging_density",
  "risk_transparency", "risk_disclosure_raw", "risk_numeric_density",
  "tone_lm", "pos_density", "neg_density",
  # Normalised (for descriptive analysis only)
  "operational_specificity_norm", "forward_looking_norm",
  "risk_transparency_norm", "risk_disclosure_raw_norm", "tone_lm_norm",
  # Fallback level metadata
  names(idx)[str_detect(names(idx), "_level$")]
)
idx_keep_cols <- intersect(idx_keep_cols, names(idx))

idx_sel <- idx %>% select(all_of(unique(idx_keep_cols)))
cat(sprintf("  - Kept %d/%d columns from indices\n\n", ncol(idx_sel), ncol(idx)))

# ---- BLOCK 4: Merge ----
cat("BLOCK 4: Merging...\n")
merged <- base %>% left_join(idx_sel, by = merge_key)
stopifnot(nrow(merged) == nrow(base))
cat("  - Row count preserved: OK\n")

# ---- BLOCK 5: Post-merge checks ----
cat("\nBLOCK 5: Post-merge checks...\n")

# v6 core indices (all should be present and non-NA for matched deals)
core_vars <- c("operational_specificity_raw", "fl_sentence_share",
               "risk_transparency", "tone_lm")
core_present <- core_vars[core_vars %in% names(merged)]

missingness_tbl <- tibble(
  variable = core_present,
  n = nrow(merged),
  n_na = sapply(core_present, function(v) sum(is.na(merged[[v]]))),
  na_share = sapply(core_present, function(v) mean(is.na(merged[[v]])))
)
write_csv(missingness_tbl, file.path(out_dir_tables, "module6_index_missingness.csv"))
cat("  - Index missingness:\n")
for (i in seq_len(nrow(missingness_tbl))) {
  cat(sprintf("    %-30s: %d NAs (%.2f%%)\n",
              missingness_tbl$variable[i], missingness_tbl$n_na[i],
              100 * missingness_tbl$na_share[i]))
}

# v6: tone_lm is always defined (no sparse-denom NA), so no special tolerance needed
# risk_transparency may have NAs for singleton industry-year groups

# ---- BLOCK 5B: Outcome variables ----
cat("\nBLOCK 5B: Constructing outcome variables...\n")

if ("premium_paid_1_day_prior_to_announcement" %in% names(merged)) {
  merged$premium_1d <- as.numeric(merged$premium_paid_1_day_prior_to_announcement)
  cat("  - premium_1d\n")
}
if ("premium_paid_1_week_prior_to_announcement" %in% names(merged)) {
  merged$premium_1w <- as.numeric(merged$premium_paid_1_week_prior_to_announcement)
  cat("  - premium_1w\n")
}
if ("premium_paid_4_weeks_prior_to_announcement" %in% names(merged)) {
  merged$premium_4w <- as.numeric(merged$premium_paid_4_weeks_prior_to_announcement)
  cat("  - premium_4w\n")
}

if ("deal_status" %in% names(merged)) {
  merged$completion <- case_when(
    tolower(merged$deal_status) == "completed"  ~ 1L,
    tolower(merged$deal_status) == "withdrawn"  ~ 0L,
    TRUE ~ NA_integer_)
  cat(sprintf("  - completion: C=%d W=%d NA=%d\n",
              sum(merged$completion == 1L, na.rm = TRUE),
              sum(merged$completion == 0L, na.rm = TRUE),
              sum(is.na(merged$completion))))
}

# -----------------------------------------------------------------------------
# TIME TO CLOSE (v6.1 — aligned with working v2 logic)
# SDC uses number_of_days_between_date_announced_and_date_effective.
# IMPORTANT: SDC uses -999 as sentinel for withdrawn deals (no effective date).
#            We convert -999 to NA since duration is undefined for non-completed.
# -----------------------------------------------------------------------------
cat("  - time_to_close construction:\n")

ttc_col <- "number_of_days_between_date_announced_and_date_effective"

if (ttc_col %in% names(merged)) {
  raw_ttc <- as.numeric(merged[[ttc_col]])
  n_sentinel <- sum(raw_ttc == -999, na.rm = TRUE)

  # Convert -999 sentinel to NA
  merged$time_to_close <- if_else(raw_ttc == -999, NA_real_, raw_ttc)

  valid_ttc <- merged$time_to_close[!is.na(merged$time_to_close) & merged$time_to_close > 0]
  cat(sprintf("    Found SDC column '%s'\n", ttc_col))
  cat(sprintf("    n_valid=%d | n_sentinel(-999)=%d | mean=%.1f | median=%.1f days\n",
              length(valid_ttc), n_sentinel,
              mean(valid_ttc, na.rm = TRUE),
              median(valid_ttc, na.rm = TRUE)))

} else if ("time_to_close" %in% names(merged)) {
  merged$time_to_close <- as.numeric(merged$time_to_close)
  cat("    Using pre-existing time_to_close column\n")

} else {
  # Auto-discovery fallback
  duration_candidates <- grep(
    "days|duration|time.to|date_effective|date_completed|effective_date|completion_date",
    names(merged), ignore.case = TRUE, value = TRUE
  )
  cat(sprintf("    SDC column '%s' NOT FOUND\n", ttc_col))

  if (length(duration_candidates) > 0) {
    cat(sprintf("    Candidate columns: %s\n", paste(duration_candidates, collapse = ", ")))
  }

  # Try computing from date pairs
  date_ann_col <- grep("^date.*(announced|announcement)", names(merged),
                       ignore.case = TRUE, value = TRUE)[1]
  date_eff_candidates <- grep("^date.*(effective|complet|unconditional)|effective.*date|complet.*date",
                              names(merged), ignore.case = TRUE, value = TRUE)

  if (!is.na(date_ann_col) && length(date_eff_candidates) > 0) {
    date_eff_col <- date_eff_candidates[1]
    cat(sprintf("    Computing from: '%s' and '%s'\n", date_ann_col, date_eff_col))

    d_ann <- tryCatch(as.Date(merged[[date_ann_col]]), error = function(e) {
      as.Date(merged[[date_ann_col]], format = "%m/%d/%Y")
    })
    d_eff <- tryCatch(as.Date(merged[[date_eff_col]]), error = function(e) {
      as.Date(merged[[date_eff_col]], format = "%m/%d/%Y")
    })

    merged$time_to_close <- as.numeric(difftime(d_eff, d_ann, units = "days"))
    valid_ttc <- !is.na(merged$time_to_close) & merged$time_to_close > 0
    cat(sprintf("    Computed: valid=%d | negative/zero=%d | NA=%d\n",
                sum(valid_ttc),
                sum(!is.na(merged$time_to_close) & merged$time_to_close <= 0),
                sum(is.na(merged$time_to_close))))
  } else {
    cat("    WARNING: Cannot construct time_to_close.\n")
    cat(sprintf("    All columns with 'date': %s\n",
                paste(grep("date", names(merged), ignore.case = TRUE, value = TRUE),
                      collapse = ", ")))
    merged$time_to_close <- NA_real_
  }
}

# ---- BLOCK 5C: FE identifiers ----
cat("\nBLOCK 5C: FE identifiers...\n")
if ("target_primary_sic" %in% names(merged)) {
  merged$sic2 <- substr(as.character(merged$target_primary_sic), 1, 2)
}
if ("sic2" %in% names(merged) && "year_announced" %in% names(merged)) {
  merged$ind_year_fe <- paste0(merged$sic2, "_", merged$year_announced)
  cat(sprintf("  - ind_year_fe: %d unique cells\n", n_distinct(merged$ind_year_fe)))
}

# ---- BLOCK 5D: Sample flags ----
cat("\nBLOCK 5D: Sample flags...\n")

merged <- merged %>%
  mutate(
    sample_core = if_else(
      !is.na(operational_specificity_raw) &
        !is.na(fl_sentence_share) &
        !is.na(tone_lm),
      1L, 0L),

    # Risk transparency may have NAs (singleton groups)
    sample_risk = if_else(sample_core == 1L & !is.na(risk_transparency), 1L, 0L),

    sample_premium = if_else(
      sample_core == 1L & !is.na(premium_1d) &
        !is.na(target_share_price_1_day_prior_to_announcement_usd) &
        target_share_price_1_day_prior_to_announcement_usd > 0,
      1L, 0L),

    sample_completion = if_else(sample_core == 1L & !is.na(completion), 1L, 0L),

    sample_duration = if_else(
      sample_core == 1L & !is.na(completion) & completion == 1L &
        !is.na(time_to_close) & time_to_close > 0,
      1L, 0L)
  )

cat(sprintf("  - sample_core:       %d / %d\n", sum(merged$sample_core), nrow(merged)))
cat(sprintf("  - sample_risk:       %d / %d\n", sum(merged$sample_risk), nrow(merged)))
cat(sprintf("  - sample_premium:    %d / %d\n", sum(merged$sample_premium), nrow(merged)))
cat(sprintf("  - sample_completion: %d / %d\n", sum(merged$sample_completion), nrow(merged)))
cat(sprintf("  - sample_duration:   %d / %d\n", sum(merged$sample_duration), nrow(merged)))

# ---- BLOCK 5E: Outlier flags ----
cat("\nBLOCK 5E: Outlier flags (|z| > 5 SD on raw)...\n")

outlier_vars <- c("operational_specificity_raw", "fl_sentence_share",
                  "risk_transparency", "tone_lm")
outlier_vars <- outlier_vars[outlier_vars %in% names(merged)]

for (v in outlier_vars) {
  flag_col <- paste0(v, "_outlier")
  vals <- merged[[v]]
  mu <- mean(vals, na.rm = TRUE); sigma <- sd(vals, na.rm = TRUE)
  merged[[flag_col]] <- if_else(!is.na(vals) & abs(vals - mu) > 5 * sigma, 1L, 0L)
  cat(sprintf("  - %s: %d outliers\n", v, sum(merged[[flag_col]], na.rm = TRUE)))
}

# ---- BLOCK 5F: Processing costs integration (v6.1) ----
# Module 07 produces processing_costs.rds (Fog, FK, log_length).
# Processing costs are a core construct in the research design, so they are
# joined by default — not as an optional step.
cat("\nBLOCK 5F: Integrating processing costs (Module 07)...\n")

path_proc_costs <- "data/interim/processing_costs.rds"
if (file.exists(path_proc_costs)) {
  proc_costs <- readRDS(path_proc_costs)
  proc_costs$deal_id <- as.character(proc_costs$deal_id)

  # Avoid column collisions (drop any overlapping non-key columns)
  proc_keep <- setdiff(names(proc_costs), setdiff(names(merged), "deal_id"))
  proc_costs <- proc_costs %>% select(deal_id, all_of(proc_keep))

  merged <- merged %>% left_join(proc_costs, by = "deal_id")
  cat(sprintf("  - Joined %d processing cost variables\n", length(proc_keep)))
  cat(sprintf("  - mda_fog NAs: %d | mda_log_length NAs: %d\n",
              sum(is.na(merged$mda_fog)), sum(is.na(merged$mda_log_length))))
} else {
  cat("  - processing_costs.rds not found. Run Module 07 first.\n")
  cat("  - Continuing without processing costs (they can be joined later).\n")
}

# ---- BLOCK 6: Save ----
cat("\nBLOCK 6: Saving...\n")

saveRDS(merged, file.path(out_dir_final, "analysis_dataset.rds"))
cat(sprintf("  - RDS: %s\n", file.path(out_dir_final, "analysis_dataset.rds")))

text_cols <- c("mda_text", "risk_factors_text", "mda_clean", "risk_clean")
merged_csv <- merged %>% select(-any_of(text_cols))
write_csv(merged_csv, file.path(out_dir_final, "analysis_dataset.csv"))
cat(sprintf("  - CSV: %s (%d cols)\n", file.path(out_dir_final, "analysis_dataset.csv"), ncol(merged_csv)))

# ---- BLOCK 7: Data dictionary ----
cat("\nBLOCK 7: Data dictionary...\n")

make_construction_v6 <- function(col_name) {
  case_when(
    col_name == "operational_specificity_raw" ~ "0.5*numeric_density(wc_total) + 0.5*compound_density(wc_alpha)",
    col_name == "fl_sentence_share" ~ "# FL sentences / # total sentences (MD&A)",
    col_name == "fl_precision_share" ~ "# FL sentences with numbers / # FL sentences",
    col_name == "commitment_density" ~ "strong modals (will/shall/must) per 1000 alpha words",
    col_name == "hedging_density" ~ "weak modals (may/might/could/would/should) per 1000 alpha words",
    col_name == "risk_transparency" ~ "1 - cosine_sim(doc_tfidf, peer_centroid); hierarchical fallback",
    col_name == "risk_disclosure_raw" ~ "LM(unc+neg) counts per 1000 alpha words (Risk Factors)",
    col_name == "tone_lm" ~ "(Pos-Neg)/(Pos+Neg+1) on MD&A (always defined)",
    col_name == "pos_density" ~ "LM positive counts per 1000 alpha words (MD&A)",
    col_name == "neg_density" ~ "LM negative counts per 1000 alpha words (MD&A)",
    col_name == "mda_fog" ~ "Gunning Fog Index (MD&A) via quanteda.textstats",
    col_name == "mda_fk" ~ "Flesch-Kincaid Grade Level (MD&A) via quanteda.textstats",
    col_name == "mda_log_length" ~ "log(alpha word count) (MD&A)",
    col_name == "risk_fog" ~ "Gunning Fog Index (Risk Factors)",
    col_name == "risk_fk" ~ "Flesch-Kincaid Grade Level (Risk Factors)",
    col_name == "risk_log_length" ~ "log(alpha word count) (Risk Factors)",
    col_name == "premium_1d" ~ "= premium_paid_1_day_prior (numeric)",
    col_name == "completion" ~ "1=Completed, 0=Withdrawn, NA=other",
    str_detect(col_name, "_norm$") ~ "Z-scored hierarchical (sic2xyear > sic2 > year > global)",
    str_detect(col_name, "_outlier$") ~ "1 if |raw - mean| > 5*SD",
    TRUE ~ "From source"
  )
}

data_dict <- tibble(
  column = names(merged),
  type = sapply(merged, function(x) class(x)[1]),
  construction = make_construction_v6(names(merged)),
  n_non_na = sapply(merged, function(x) sum(!is.na(x)))
)
write_csv(data_dict, file.path(out_dir_tables, "data_dictionary_final.csv"))

# ---- BLOCK 8: Report ----
report <- c(
  "# Module 6 — Final Dataset Report (v6)", "",
  paste0("Generated: ", Sys.time()),
  sprintf("Rows: %d | Columns: %d", nrow(merged), ncol(merged)), "",
  "## v6 Key changes",
  "- Indices from Module 3 v6 (sentence-based FL, cosine risk, corrected tone)",
  "- Raw indices exported for regression (use raw + FE, not normalised)",
  "- tone_lm always defined (no sparse-denom NA)",
  "- sample_risk separate flag for risk_transparency NAs (singleton groups)",
  "",
  "## Sample sizes",
  sprintf("- sample_core: %d", sum(merged$sample_core)),
  sprintf("- sample_risk: %d", sum(merged$sample_risk)),
  sprintf("- sample_premium: %d", sum(merged$sample_premium)),
  sprintf("- sample_completion: %d", sum(merged$sample_completion)),
  sprintf("- sample_duration: %d", sum(merged$sample_duration))
)

writeLines(report, file.path(out_dir_reports, "06_final_dataset_report.md"))

cat("\n=== MODULE 6 COMPLETE (v6) ===\n")
