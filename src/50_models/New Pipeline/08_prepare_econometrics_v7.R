# ==============================================================================
# 08_prepare_econometrics_v6.R
#
# PURPOSE: Transform analysis_dataset.rds (from Module 06 v6) into
#          regression-ready subsets for premium, completion, and duration.
#
# INPUT:   data/final/analysis_dataset.rds  (Module 06 v6 output)
# OUTPUTS:
#   - data/processed/econ_premium.rds
#   - data/processed/econ_completion.rds
#   - data/processed/econ_duration.rds
#   - output/tables/econ_summary_stats.csv
#   - output/tables/econ_correlation.csv
#
# v6 CHANGES:
#   - Reads single analysis_dataset.rds (not three separate files)
#   - Variable names aligned with NLP pipeline v6:
#       operational_specificity_raw, fl_sentence_share, fl_precision_share,
#       commitment_density, hedging_density,
#       risk_transparency, risk_disclosure_raw, risk_numeric_density,
#       tone_lm, pos_density, neg_density
#   - Processing costs: mda_fog, mda_fk, mda_log_length
#   - Outcomes: premium_1d, completion, time_to_close
#   - FE: ind_year_fe (from Module 06), sic2
#   - Raw indices used in regression + FE (not normalised) per design
#   - Standardised (_z) versions created for coefficient comparability
#
# DESIGN: All transformations explicit. Original values preserved.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

source("src/50_models/econometric_utils.R", local = TRUE)

cat("=== MODULE 08: ECONOMETRIC PREPARATION (v6) ===\n\n")

# ==============================================================================
# CONFIG
# ==============================================================================

CONFIG <- list(
  path_input  = "data/final/analysis_dataset.rds",
  path_out    = "data/processed",
  path_tables = "output/tables",

  # Winsorization: 1%/99% (standard in M&A literature)
  winsor_lower = 0.01,
  winsor_upper = 0.99,

  # NLP indices to standardise (raw → _z)
  # These are the v6 constructs from Module 03
  nlp_indices = c(
    # Core 4
    "operational_specificity_raw",
    "fl_sentence_share",
    "risk_transparency",
    "tone_lm",
    # Supplementary
    "fl_precision_share",
    "commitment_density",
    "hedging_density",
    "risk_disclosure_raw",
    "risk_numeric_density",
    "pos_density",
    "neg_density"
  ),

  # Processing cost variables (from Module 07 v4)
  proc_cost_vars = c("mda_fog", "mda_fk", "mda_log_length",
                      "risk_fog", "risk_fk", "risk_log_length"),

  # Continuous controls to winsorise
  controls_to_winsor = c(
    "premium_1d", "premium_1w", "premium_4w",
    "deal_value_usd_millions",
    "time_to_close"
  ),

  # Variables to log-transform (right-skewed positive)
  log_vars = c("deal_value_usd_millions"),

  # Premium sample restrictions (percentage points)
  premium_min = -100,
  premium_max = 300,
  min_deal_value = 10  # $10M
)

dir.create(CONFIG$path_out, recursive = TRUE, showWarnings = FALSE)
dir.create(CONFIG$path_tables, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# BLOCK 1: LOAD DATA
# ==============================================================================

cat("BLOCK 1: Loading analysis dataset...\n")

if (!file.exists(CONFIG$path_input)) stop("Not found: ", CONFIG$path_input)
df <- readRDS(CONFIG$path_input)
cat(sprintf("  - %d × %d\n", nrow(df), ncol(df)))

# Quick audit: expected columns
expected <- c("deal_id", "operational_specificity_raw", "fl_sentence_share",
              "risk_transparency", "tone_lm", "premium_1d", "completion",
              "time_to_close", "sic2", "ind_year_fe", "year_announced")
missing <- setdiff(expected, names(df))
if (length(missing) > 0) {
  cat(sprintf("  WARNING: Missing expected columns: %s\n", paste(missing, collapse = ", ")))
}

# ==============================================================================
# BLOCK 2: WINSORIZATION
# ==============================================================================

cat("\nBLOCK 2: Winsorizing continuous variables...\n")

vars_to_w <- intersect(CONFIG$controls_to_winsor, names(df))

for (v in vars_to_w) {
  x <- df[[v]]
  if (!is.numeric(x) || all(is.na(x))) next

  q <- quantile(x, c(CONFIG$winsor_lower, CONFIG$winsor_upper), na.rm = TRUE)
  n_lo <- sum(x < q[1], na.rm = TRUE)
  n_hi <- sum(x > q[2], na.rm = TRUE)

  # Preserve original as _raw, create winsorised as _w
  df[[paste0(v, "_raw")]] <- x
  df[[paste0(v, "_w")]]   <- winsorize(x, CONFIG$winsor_lower, CONFIG$winsor_upper)

  if (n_lo + n_hi > 0) {
    cat(sprintf("  %s: %d low, %d high (thresholds: %.2f / %.2f)\n",
                v, n_lo, n_hi, q[1], q[2]))
  }
}

# ==============================================================================
# BLOCK 3: LOG TRANSFORMATIONS
# ==============================================================================

cat("\nBLOCK 3: Log transforms...\n")

for (v in intersect(CONFIG$log_vars, names(df))) {
  x <- df[[v]]
  if (!is.numeric(x)) next
  n_neg <- sum(x < 0, na.rm = TRUE)
  if (n_neg > 0) {
    cat(sprintf("  %s: %d negative values — skipping\n", v, n_neg))
    next
  }
  col_log <- paste0("ln_", v)
  df[[col_log]] <- log(x + 1)
  cat(sprintf("  %s → %s\n", v, col_log))
}

# Also create ln_deal_value from winsorised version if available
if ("deal_value_usd_millions_w" %in% names(df)) {
  df$ln_deal_value <- log(df$deal_value_usd_millions_w + 1)
  cat("  deal_value_usd_millions_w → ln_deal_value\n")
}

# ==============================================================================
# BLOCK 4: STANDARDIZE NLP INDICES
# ==============================================================================

cat("\nBLOCK 4: Standardizing NLP indices (raw → _z)...\n")

nlp_available <- intersect(CONFIG$nlp_indices, names(df))

for (v in nlp_available) {
  x <- df[[v]]
  if (!is.numeric(x) || all(is.na(x))) next
  col_z <- paste0(v, "_z")
  df[[col_z]] <- standardize(x)
  cat(sprintf("  %s → %s (mean=%.3f, sd=%.3f)\n",
              v, col_z, mean(x, na.rm=TRUE), sd(x, na.rm=TRUE)))
}

# Standardize processing costs too
proc_available <- intersect(CONFIG$proc_cost_vars, names(df))
for (v in proc_available) {
  x <- df[[v]]
  if (!is.numeric(x) || all(is.na(x))) next
  df[[paste0(v, "_z")]] <- standardize(x)
}
if (length(proc_available) > 0) {
  cat(sprintf("  + %d processing cost vars standardized\n", length(proc_available)))
}

# ==============================================================================
# BLOCK 5: FIXED EFFECTS (ensure present)
# ==============================================================================

cat("\nBLOCK 5: Fixed effects...\n")

# sic2 and ind_year_fe should already exist from Module 06
if (!"sic2" %in% names(df) && "target_primary_sic" %in% names(df)) {
  df$sic2 <- extract_sic2(df$target_primary_sic)
  cat("  Created sic2\n")
}

if (!"ind_year_fe" %in% names(df) &&
    "sic2" %in% names(df) && "year_announced" %in% names(df)) {
  df$ind_year_fe <- paste0(df$sic2, "_", df$year_announced)
  cat("  Created ind_year_fe\n")
}

if ("year_announced" %in% names(df)) {
  df$fe_year <- factor(df$year_announced)
}

# For fixest: ind_year_fe as factor
if ("ind_year_fe" %in% names(df)) {
  df$fe_ind_year <- factor(df$ind_year_fe)
  n_fe <- n_distinct(df$fe_ind_year, na.rm = TRUE)
  cat(sprintf("  fe_ind_year: %d cells | fe_year: %d levels\n",
              n_fe, n_distinct(df$fe_year, na.rm = TRUE)))
}

# ==============================================================================
# BLOCK 6: ADDITIONAL CONTROLS
# ==============================================================================

cat("\nBLOCK 6: Additional control variables...\n")

# Hostile dummy
if ("attitude" %in% names(df)) {
  df$hostile <- as.integer(str_detect(tolower(df$attitude), "hostil"))
  cat(sprintf("  hostile: %d hostile deals\n", sum(df$hostile == 1, na.rm = TRUE)))
}

# Tender offer dummy
if ("tender_offer" %in% names(df)) {
  df$tender <- as.integer(!is.na(df$tender_offer) & df$tender_offer != "")
} else if ("acquisition_techniques" %in% names(df)) {
  df$tender <- as.integer(str_detect(tolower(df$acquisition_techniques), "tender"))
}
if ("tender" %in% names(df)) {
  cat(sprintf("  tender: %d tender offers\n", sum(df$tender == 1, na.rm = TRUE)))
}

# Cross-border dummy
if ("cross_border" %in% names(df)) {
  df$cross_border_d <- as.integer(str_detect(tolower(df$cross_border), "yes|cross"))
  cat(sprintf("  cross_border_d: %d cross-border\n", sum(df$cross_border_d == 1, na.rm = TRUE)))
}

# Cash/stock percentages (ensure numeric)
for (v in c("percentage_of_cash", "percentage_of_stock")) {
  if (v %in% names(df)) df[[v]] <- as.numeric(df[[v]])
}

# ==============================================================================
# BLOCK 7: SAMPLE RESTRICTION FLAGS
# ==============================================================================

cat("\nBLOCK 7: Sample restriction flags...\n")

# Premium bounds
if ("premium_1d_w" %in% names(df)) {
  df$premium_sample_ok <- as.integer(
    !is.na(df$premium_1d_w) &
    df$premium_1d_w >= CONFIG$premium_min &
    df$premium_1d_w <= CONFIG$premium_max
  )
  n_ok <- sum(df$premium_sample_ok == 1, na.rm = TRUE)
  n_lo <- sum(!is.na(df$premium_1d_w) & df$premium_1d_w < CONFIG$premium_min, na.rm = TRUE)
  n_hi <- sum(!is.na(df$premium_1d_w) & df$premium_1d_w > CONFIG$premium_max, na.rm = TRUE)
  cat(sprintf("  premium bounds [%d, %d]: ok=%d | low=%d | high=%d\n",
              CONFIG$premium_min, CONFIG$premium_max, n_ok, n_lo, n_hi))
}

# Deal value minimum
dv_col <- if ("deal_value_usd_millions" %in% names(df)) "deal_value_usd_millions" else
           if ("rank_value_inc_net_debt_of_target_usd_mil" %in% names(df))
             "rank_value_inc_net_debt_of_target_usd_mil" else NA_character_

if (!is.na(dv_col)) {
  df$small_deal <- as.integer(as.numeric(df[[dv_col]]) < CONFIG$min_deal_value)
  cat(sprintf("  small deals (< $%dM): %d\n", CONFIG$min_deal_value,
              sum(df$small_deal == 1, na.rm = TRUE)))
}

# ==============================================================================
# BLOCK 8: CREATE ANALYSIS SUBSETS
# ==============================================================================

cat("\nBLOCK 8: Creating analysis subsets...\n")

# Common NLP filter: all core indices non-NA
core4 <- c("operational_specificity_raw", "fl_sentence_share",
           "risk_transparency", "tone_lm")
core4_present <- intersect(core4, names(df))

df_core <- df %>% filter(if_all(all_of(core4_present), ~!is.na(.)))
cat(sprintf("  Core NLP sample: %d / %d\n", nrow(df_core), nrow(df)))

# --- Premium subset ---
if ("premium_1d_w" %in% names(df_core)) {
  econ_premium <- df_core %>%
    filter(!is.na(premium_1d_w))

  cat(sprintf("  econ_premium: %d obs | premium mean=%.1f | median=%.1f\n",
              nrow(econ_premium),
              mean(econ_premium$premium_1d_w, na.rm = TRUE),
              median(econ_premium$premium_1d_w, na.rm = TRUE)))

  saveRDS(econ_premium, file.path(CONFIG$path_out, "econ_premium.rds"))
} else {
  cat("  WARNING: premium_1d_w not found. Check Module 06 + winsorization.\n")
}

# --- Completion subset ---
if ("completion" %in% names(df_core)) {
  econ_completion <- df_core %>%
    filter(!is.na(completion))

  cat(sprintf("  econ_completion: %d obs | C=%d W=%d\n",
              nrow(econ_completion),
              sum(econ_completion$completion == 1),
              sum(econ_completion$completion == 0)))

  saveRDS(econ_completion, file.path(CONFIG$path_out, "econ_completion.rds"))
}

# --- Duration subset (with proper censoring per Research Design) ---
# The Research Design requires: "withdrawn deals treated as censored at withdrawal date"
# Two subsets produced:
#   econ_duration_completed: only completed (for AFT, primary)
#   econ_duration_full: completed + withdrawn censored (for Cox, robustness)

if ("time_to_close" %in% names(df_core) &&
    "date_announced" %in% names(df_core)) {

  # (a) Completed-only subset (for AFT — no censoring needed)
  econ_duration_completed <- df_core %>%
    filter(!is.na(completion), completion == 1L,
           !is.na(time_to_close), time_to_close > 0)
  econ_duration_completed$event <- 1L
  if (!"time_to_close_w" %in% names(econ_duration_completed)) {
    econ_duration_completed$time_to_close_w <- winsorize(econ_duration_completed$time_to_close)
  }

  cat(sprintf("  econ_duration (completed): %d obs | mean=%.0f days | median=%.0f days\n",
              nrow(econ_duration_completed),
              mean(econ_duration_completed$time_to_close_w),
              median(econ_duration_completed$time_to_close_w)))

  saveRDS(econ_duration_completed, file.path(CONFIG$path_out, "econ_duration.rds"))

  # (b) Full duration subset with censoring (for Cox)
  # Completed: event=1, time = time_to_close
  # Withdrawn: event=0, time = date_withdrawn - date_announced (censoring time)
  econ_dur_full <- df_core %>% filter(!is.na(completion))

  # Compute duration for withdrawn deals
  if ("date_withdrawn" %in% names(econ_dur_full)) {
    econ_dur_full <- econ_dur_full %>%
      mutate(
        # For completed: use existing time_to_close
        # For withdrawn: compute days from announced to withdrawn
        surv_time = case_when(
          completion == 1L & !is.na(time_to_close) & time_to_close > 0 ~ time_to_close,
          completion == 0L & !is.na(date_withdrawn) & !is.na(date_announced) ~
            as.numeric(difftime(date_withdrawn, date_announced, units = "days")),
          TRUE ~ NA_real_
        ),
        surv_event = as.integer(completion == 1L)
      ) %>%
      filter(!is.na(surv_time), surv_time > 0)

    # Winsorize survival time
    econ_dur_full$surv_time_w <- winsorize(econ_dur_full$surv_time)

    n_events   <- sum(econ_dur_full$surv_event == 1)
    n_censored <- sum(econ_dur_full$surv_event == 0)

    cat(sprintf("  econ_duration_full (Cox): %d obs | events=%d | censored=%d\n",
                nrow(econ_dur_full), n_events, n_censored))
    cat(sprintf("    Completed: mean=%.0f days | Withdrawn (censored): mean=%.0f days\n",
                mean(econ_dur_full$surv_time[econ_dur_full$surv_event == 1]),
                mean(econ_dur_full$surv_time[econ_dur_full$surv_event == 0])))

    saveRDS(econ_dur_full, file.path(CONFIG$path_out, "econ_duration_full.rds"))
  } else {
    cat("  WARNING: date_withdrawn not found. Cox with censoring not possible.\n")
    cat("  Saving completed-only subset as econ_duration_full.rds too.\n")
    saveRDS(econ_duration_completed, file.path(CONFIG$path_out, "econ_duration_full.rds"))
  }
}

# ==============================================================================
# BLOCK 9: SUMMARY STATISTICS
# ==============================================================================

cat("\nBLOCK 9: Summary statistics...\n")

summary_vars <- c(
  # Outcomes
  "premium_1d_w", "completion", "time_to_close_w",
  # Core NLP
  core4_present,
  # Supplementary NLP
  "fl_precision_share", "commitment_density", "hedging_density",
  "risk_disclosure_raw", "pos_density", "neg_density",
  # Processing costs
  "mda_fog", "mda_log_length",
  # Controls
  "ln_deal_value"
)
summary_vars <- intersect(summary_vars, names(df_core))

if (length(summary_vars) > 0) {
  stats <- summary_table(df_core, summary_vars)
  write_csv(stats, file.path(CONFIG$path_tables, "econ_summary_stats.csv"))
  cat(sprintf("  Saved: %s (%d vars)\n",
              file.path(CONFIG$path_tables, "econ_summary_stats.csv"), nrow(stats)))
}

# ==============================================================================
# BLOCK 10: CORRELATION MATRIX
# ==============================================================================

cat("\nBLOCK 10: Correlation matrix...\n")

corr_vars <- c("premium_1d_w",
               paste0(core4_present, "_z"),
               "mda_fog_z", "mda_log_length_z")
corr_vars <- intersect(corr_vars, names(df_core))

if (length(corr_vars) >= 3) {
  cr <- correlation_matrix(df_core, corr_vars)
  if (!is.null(cr$cor)) {
    corr_df <- as.data.frame(cr$cor) %>% tibble::rownames_to_column("variable")
    write_csv(corr_df, file.path(CONFIG$path_tables, "econ_correlation.csv"))
    cat(sprintf("  Saved: %s (%d × %d)\n",
                file.path(CONFIG$path_tables, "econ_correlation.csv"),
                nrow(cr$cor), ncol(cr$cor)))
  }
}

# ==============================================================================
# REPORT
# ==============================================================================

cat("\n=== MODULE 08 COMPLETE (v6) ===\n")
cat(sprintf("  econ_premium:    %s\n", file.path(CONFIG$path_out, "econ_premium.rds")))
cat(sprintf("  econ_completion: %s\n", file.path(CONFIG$path_out, "econ_completion.rds")))
cat(sprintf("  econ_duration:   %s\n", file.path(CONFIG$path_out, "econ_duration.rds")))
cat("Ready for Module 09: Model estimation.\n")
