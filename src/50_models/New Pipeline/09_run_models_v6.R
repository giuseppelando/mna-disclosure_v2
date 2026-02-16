# ==============================================================================
# 09_run_models_v6.R
#
# PURPOSE: Estimate econometric models for M&A disclosure analysis.
#          Consolidated from prior scripts 09 + 10.
#
# MODELS:
#   A. Premium regressions (OLS via fixest::feols)
#   B. Completion probability (Logit via fixest::feglm)
#   C. Time-to-completion (Cox PH via survival::coxph)
#
# INPUTS:  data/processed/econ_premium.rds, econ_completion.rds, econ_duration.rds
#          (from 08_prepare_econometrics_v6.R)
#
# OUTPUTS:
#   - output/tables/premium_results.csv
#   - output/tables/completion_results.csv
#   - output/tables/duration_results.csv
#   - output/tables/model_comparison.csv
#   - reports/09_econometric_report.md
#
# v6 VARIABLE NAMES:
#   Core 4 (raw → _z for comparability):
#     operational_specificity_raw_z, fl_sentence_share_z,
#     risk_transparency_z, tone_lm_z
#   Supplementary:
#     fl_precision_share_z, commitment_density_z, hedging_density_z,
#     risk_disclosure_raw_z, pos_density_z, neg_density_z
#   Processing costs:
#     mda_fog_z, mda_log_length_z
#   Outcomes:
#     premium_1d_w, completion, time_to_close_w (+ event=1 for Cox)
#   FE:
#     fe_year, fe_ind_year
#   Clustering:
#     sic2
#
# DEPENDENCIES: fixest, survival, modelsummary, broom
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(fixest)
  library(survival)
  library(broom)
})

cat("=== MODULE 09: ECONOMETRIC MODELS (v6) ===\n\n")

# ==============================================================================
# CONFIG
# ==============================================================================

CONFIG <- list(
  paths = list(
    premium    = "data/processed/econ_premium.rds",
    completion = "data/processed/econ_completion.rds",
    duration   = "data/processed/econ_duration.rds",
    tables     = "output/tables",
    reports    = "reports"
  ),

  # Core NLP indices (standardized) — used in main specifications
  core4_z = c(
    "operational_specificity_raw_z",
    "fl_sentence_share_z",
    "risk_transparency_z",
    "tone_lm_z"
  ),

  # Extended NLP indices — for augmented specifications
  extended_z = c(
    "fl_precision_share_z",
    "commitment_density_z",
    "hedging_density_z"
  ),

  # Processing costs — for information environment controls
  proc_cost_z = c("mda_fog_z", "mda_log_length_z"),

  # Deal-level controls
  controls = c(
    "ln_deal_value",
    "tender",
    "hostile",
    "percentage_of_cash",
    "cross_border_d"
  ),

  # Outcomes
  outcome_premium = "premium_1d_w",
  outcome_completion = "completion",
  outcome_time = "time_to_close_w",
  outcome_event = "event"
)

dir.create(CONFIG$paths$tables, recursive = TRUE, showWarnings = FALSE)
dir.create(CONFIG$paths$reports, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# HELPERS
# ==============================================================================

#' Filter variable vector to those actually present in df
avail <- function(vars, df) intersect(vars, names(df))

#' Build fixest formula: outcome ~ rhs | fe
build_fe_formula <- function(outcome, rhs_vars, fe_var = NULL) {
  rhs <- paste(rhs_vars, collapse = " + ")
  if (!is.null(fe_var)) {
    as.formula(paste(outcome, "~", rhs, "|", fe_var))
  } else {
    as.formula(paste(outcome, "~", rhs))
  }
}

#' Safe coefficient extraction
safe_coef <- function(model, var) {
  cc <- tryCatch(coef(model), error = function(e) NULL)
  if (is.null(cc) || !var %in% names(cc)) return(NA_real_)
  cc[var]
}

#' Tidy extraction for export
tidy_model <- function(model, model_name, type = "feols") {
  tt <- tryCatch(broom::tidy(model, conf.int = TRUE), error = function(e) NULL)
  if (is.null(tt)) return(NULL)
  tt$model <- model_name
  tt$n_obs <- tryCatch(nobs(model), error = function(e) NA)
  tt
}

# ==============================================================================
# SECTION A: PREMIUM REGRESSIONS (OLS)
# ==============================================================================

cat(rep("=", 78), "\n", sep = "")
cat("A. PREMIUM REGRESSIONS (OLS + FE)\n")
cat(rep("-", 78), "\n", sep = "")

models_premium <- list()

if (file.exists(CONFIG$paths$premium)) {
  dp <- readRDS(CONFIG$paths$premium)
  cat(sprintf("  Loaded: %d obs\n", nrow(dp)))

  core_z   <- avail(CONFIG$core4_z, dp)
  ext_z    <- avail(CONFIG$extended_z, dp)
  proc_z   <- avail(CONFIG$proc_cost_z, dp)
  ctrls    <- avail(CONFIG$controls, dp)
  outcome  <- CONFIG$outcome_premium

  cat(sprintf("  Core NLP: %s\n", paste(core_z, collapse = ", ")))
  cat(sprintf("  Controls: %s\n", paste(ctrls, collapse = ", ")))

  # Complete cases for core specification
  key <- c(outcome, core_z, ctrls)
  dp_cc <- dp %>% filter(if_all(all_of(avail(key, dp)), ~!is.na(.)))
  cat(sprintf("  Analysis sample (complete cases): %d\n\n", nrow(dp_cc)))

  # ---------- P1: Core 4 + Year FE ----------
  cat("[P1] Core 4 + Year FE...\n")
  f1 <- build_fe_formula(outcome, c(core_z, ctrls), "fe_year")
  models_premium$P1_core_yearFE <- tryCatch(
    feols(f1, data = dp_cc, vcov = ~sic2),
    error = function(e) { cat("  Error:", e$message, "\n"); NULL }
  )
  if (!is.null(models_premium$P1_core_yearFE)) {
    cat(sprintf("  N=%d  R²=%.3f\n", nobs(models_premium$P1_core_yearFE),
                r2(models_premium$P1_core_yearFE)["r2"]))
  }

  # ---------- P2: Core 4 + Industry×Year FE ----------
  cat("[P2] Core 4 + Ind×Year FE...\n")
  if ("fe_ind_year" %in% names(dp_cc)) {
    f2 <- build_fe_formula(outcome, c(core_z, ctrls), "fe_ind_year")
    models_premium$P2_core_indyrFE <- tryCatch(
      feols(f2, data = dp_cc, vcov = ~sic2),
      error = function(e) { cat("  Error:", e$message, "\n"); NULL }
    )
    if (!is.null(models_premium$P2_core_indyrFE)) {
      cat(sprintf("  N=%d  R²=%.3f\n", nobs(models_premium$P2_core_indyrFE),
                  r2(models_premium$P2_core_indyrFE)["r2"]))
    }
  }

  # ---------- P3: Extended (Core + FL decomposition + hedging) ----------
  if (length(ext_z) > 0) {
    cat("[P3] Extended (Core + supplementary)...\n")
    f3 <- build_fe_formula(outcome, c(core_z, ext_z, ctrls), "fe_year")
    dp_ext <- dp_cc %>% filter(if_all(all_of(ext_z), ~!is.na(.)))
    models_premium$P3_extended <- tryCatch(
      feols(f3, data = dp_ext, vcov = ~sic2),
      error = function(e) { cat("  Error:", e$message, "\n"); NULL }
    )
    if (!is.null(models_premium$P3_extended)) {
      cat(sprintf("  N=%d  R²=%.3f\n", nobs(models_premium$P3_extended),
                  r2(models_premium$P3_extended)["r2"]))
    }
  }

  # ---------- P4: With processing costs ----------
  if (length(proc_z) > 0) {
    cat("[P4] Core + Processing costs...\n")
    f4 <- build_fe_formula(outcome, c(core_z, proc_z, ctrls), "fe_year")
    dp_proc <- dp_cc %>% filter(if_all(all_of(proc_z), ~!is.na(.)))
    models_premium$P4_proc_costs <- tryCatch(
      feols(f4, data = dp_proc, vcov = ~sic2),
      error = function(e) { cat("  Error:", e$message, "\n"); NULL }
    )
    if (!is.null(models_premium$P4_proc_costs)) {
      cat(sprintf("  N=%d  R²=%.3f\n", nobs(models_premium$P4_proc_costs),
                  r2(models_premium$P4_proc_costs)["r2"]))
    }
  }

  # ---------- P5: Individual indices (one-at-a-time) ----------
  cat("[P5] Individual indices (one-at-a-time)...\n")
  for (nlp_var in core_z) {
    mname <- paste0("P5_", nlp_var)
    f5 <- build_fe_formula(outcome, c(nlp_var, ctrls), "fe_year")
    models_premium[[mname]] <- tryCatch(
      feols(f5, data = dp_cc, vcov = ~sic2),
      error = function(e) NULL
    )
    if (!is.null(models_premium[[mname]])) {
      beta <- safe_coef(models_premium[[mname]], nlp_var)
      cat(sprintf("  %s: β=%.3f\n", nlp_var, beta))
    }
  }

  # ---------- Export premium results ----------
  cat("\nExporting premium results...\n")
  valid_p <- models_premium[!sapply(models_premium, is.null)]
  if (length(valid_p) > 0) {
    res_p <- purrr::map_df(names(valid_p), function(mn) tidy_model(valid_p[[mn]], mn))
    if (!is.null(res_p) && nrow(res_p) > 0) {
      write_csv(res_p, file.path(CONFIG$paths$tables, "premium_results.csv"))
      cat(sprintf("  Saved: %s (%d rows)\n",
                  file.path(CONFIG$paths$tables, "premium_results.csv"), nrow(res_p)))
    }
  }

} else {
  cat("  econ_premium.rds not found. Skipping.\n")
}

# ==============================================================================
# SECTION B: COMPLETION MODELS (Logit)
# ==============================================================================

cat("\n", rep("=", 78), "\n", sep = "")
cat("B. COMPLETION PROBABILITY (Logit)\n")
cat(rep("-", 78), "\n", sep = "")

models_completion <- list()

if (file.exists(CONFIG$paths$completion)) {
  dc <- readRDS(CONFIG$paths$completion)
  cat(sprintf("  Loaded: %d obs | C=%d W=%d\n", nrow(dc),
              sum(dc$completion == 1), sum(dc$completion == 0)))

  core_z <- avail(CONFIG$core4_z, dc)
  ext_z  <- avail(CONFIG$extended_z, dc)
  proc_z <- avail(CONFIG$proc_cost_z, dc)
  ctrls  <- avail(CONFIG$controls, dc)
  outcome <- CONFIG$outcome_completion

  key <- c(outcome, core_z, ctrls)
  dc_cc <- dc %>% filter(if_all(all_of(avail(key, dc)), ~!is.na(.)))
  cat(sprintf("  Analysis sample: %d\n\n", nrow(dc_cc)))

  # ---------- C1: Core 4 + Year FE ----------
  cat("[C1] Core 4 + Year FE...\n")
  f1 <- build_fe_formula(outcome, c(core_z, ctrls), "fe_year")
  models_completion$C1_core_yearFE <- tryCatch(
    feglm(f1, data = dc_cc, family = binomial("logit"), vcov = ~sic2),
    error = function(e) { cat("  Error:", e$message, "\n"); NULL }
  )
  if (!is.null(models_completion$C1_core_yearFE)) {
    cat(sprintf("  N=%d\n", nobs(models_completion$C1_core_yearFE)))
    cc <- coef(models_completion$C1_core_yearFE)
    for (v in core_z) if (v %in% names(cc))
      cat(sprintf("    %s: %.3f (OR=%.3f)\n", v, cc[v], exp(cc[v])))
  }

  # ---------- C2: Core + Ind×Year FE ----------
  cat("[C2] Core 4 + Ind×Year FE...\n")
  if ("fe_ind_year" %in% names(dc_cc)) {
    f2 <- build_fe_formula(outcome, c(core_z, ctrls), "fe_ind_year")
    models_completion$C2_core_indyrFE <- tryCatch(
      feglm(f2, data = dc_cc, family = binomial("logit"), vcov = ~sic2),
      error = function(e) { cat("  Error:", e$message, "\n"); NULL }
    )
    if (!is.null(models_completion$C2_core_indyrFE)) {
      cat(sprintf("  N=%d\n", nobs(models_completion$C2_core_indyrFE)))
    }
  }

  # ---------- C3: Extended ----------
  if (length(ext_z) > 0) {
    cat("[C3] Extended...\n")
    f3 <- build_fe_formula(outcome, c(core_z, ext_z, ctrls), "fe_year")
    dc_ext <- dc_cc %>% filter(if_all(all_of(ext_z), ~!is.na(.)))
    models_completion$C3_extended <- tryCatch(
      feglm(f3, data = dc_ext, family = binomial("logit"), vcov = ~sic2),
      error = function(e) { cat("  Error:", e$message, "\n"); NULL }
    )
    if (!is.null(models_completion$C3_extended)) {
      cat(sprintf("  N=%d\n", nobs(models_completion$C3_extended)))
    }
  }

  # ---------- C4: With processing costs ----------
  if (length(proc_z) > 0) {
    cat("[C4] Core + Processing costs...\n")
    f4 <- build_fe_formula(outcome, c(core_z, proc_z, ctrls), "fe_year")
    dc_proc <- dc_cc %>% filter(if_all(all_of(proc_z), ~!is.na(.)))
    models_completion$C4_proc_costs <- tryCatch(
      feglm(f4, data = dc_proc, family = binomial("logit"), vcov = ~sic2),
      error = function(e) { cat("  Error:", e$message, "\n"); NULL }
    )
    if (!is.null(models_completion$C4_proc_costs)) {
      cat(sprintf("  N=%d\n", nobs(models_completion$C4_proc_costs)))
    }
  }

  # ---------- Export ----------
  cat("\nExporting completion results...\n")
  valid_c <- models_completion[!sapply(models_completion, is.null)]
  if (length(valid_c) > 0) {
    res_c <- purrr::map_df(names(valid_c), function(mn) {
      tt <- tidy_model(valid_c[[mn]], mn)
      if (!is.null(tt)) tt$odds_ratio <- exp(tt$estimate)
      tt
    })
    if (!is.null(res_c) && nrow(res_c) > 0) {
      write_csv(res_c, file.path(CONFIG$paths$tables, "completion_results.csv"))
      cat(sprintf("  Saved: %s (%d rows)\n",
                  file.path(CONFIG$paths$tables, "completion_results.csv"), nrow(res_c)))
    }
  }

} else {
  cat("  econ_completion.rds not found. Skipping.\n")
}

# ==============================================================================
# SECTION C: DURATION MODELS (Cox PH)
# ==============================================================================

cat("\n", rep("=", 78), "\n", sep = "")
cat("C. TIME-TO-COMPLETION (Cox PH)\n")
cat(rep("-", 78), "\n", sep = "")

models_duration <- list()

if (file.exists(CONFIG$paths$duration)) {
  dd <- readRDS(CONFIG$paths$duration)
  cat(sprintf("  Loaded: %d obs | median=%.0f days | mean=%.0f days\n",
              nrow(dd),
              median(dd[[CONFIG$outcome_time]], na.rm = TRUE),
              mean(dd[[CONFIG$outcome_time]], na.rm = TRUE)))

  core_z <- avail(CONFIG$core4_z, dd)
  ext_z  <- avail(CONFIG$extended_z, dd)
  proc_z <- avail(CONFIG$proc_cost_z, dd)
  ctrls  <- avail(CONFIG$controls, dd)

  time_v  <- CONFIG$outcome_time
  event_v <- CONFIG$outcome_event

  key <- c(time_v, event_v, core_z, ctrls)
  dd_cc <- dd %>% filter(if_all(all_of(avail(key, dd)), ~!is.na(.)),
                          .data[[time_v]] > 0)

  cat(sprintf("  Analysis sample: %d\n\n", nrow(dd_cc)))

  # Create Surv object
  dd_cc$surv_obj <- Surv(time = dd_cc[[time_v]], event = dd_cc[[event_v]])

  # ---------- D1: Core 4 + strata(year) ----------
  cat("[D1] Core 4 + strata(year)...\n")
  f1 <- as.formula(paste("surv_obj ~",
                          paste(c(core_z, ctrls), collapse = " + "),
                          "+ strata(fe_year)"))
  models_duration$D1_core <- tryCatch(
    coxph(f1, data = dd_cc, cluster = dd_cc$sic2),
    error = function(e) { cat("  Error:", e$message, "\n"); NULL }
  )
  if (!is.null(models_duration$D1_core)) {
    cat(sprintf("  N=%d  Events=%d  Concordance=%.3f\n",
                models_duration$D1_core$n,
                models_duration$D1_core$nevent,
                summary(models_duration$D1_core)$concordance[1]))
    cc <- coef(models_duration$D1_core)
    for (v in core_z) if (v %in% names(cc))
      cat(sprintf("    %s: β=%.3f (HR=%.3f)\n", v, cc[v], exp(cc[v])))
  }

  # ---------- D2: Extended ----------
  if (length(ext_z) > 0) {
    cat("[D2] Extended...\n")
    dd_ext <- dd_cc %>% filter(if_all(all_of(ext_z), ~!is.na(.)))
    f2 <- as.formula(paste("surv_obj ~",
                            paste(c(core_z, ext_z, ctrls), collapse = " + "),
                            "+ strata(fe_year)"))
    models_duration$D2_extended <- tryCatch(
      coxph(f2, data = dd_ext, cluster = dd_ext$sic2),
      error = function(e) { cat("  Error:", e$message, "\n"); NULL }
    )
    if (!is.null(models_duration$D2_extended)) {
      cat(sprintf("  N=%d  Events=%d\n",
                  models_duration$D2_extended$n,
                  models_duration$D2_extended$nevent))
    }
  }

  # ---------- D3: With processing costs ----------
  if (length(proc_z) > 0) {
    cat("[D3] Core + Processing costs...\n")
    dd_proc <- dd_cc %>% filter(if_all(all_of(proc_z), ~!is.na(.)))
    f3 <- as.formula(paste("surv_obj ~",
                            paste(c(core_z, proc_z, ctrls), collapse = " + "),
                            "+ strata(fe_year)"))
    models_duration$D3_proc_costs <- tryCatch(
      coxph(f3, data = dd_proc, cluster = dd_proc$sic2),
      error = function(e) { cat("  Error:", e$message, "\n"); NULL }
    )
    if (!is.null(models_duration$D3_proc_costs)) {
      cat(sprintf("  N=%d  Events=%d\n",
                  models_duration$D3_proc_costs$n,
                  models_duration$D3_proc_costs$nevent))
    }
  }

  # ---------- PH assumption test ----------
  cat("\n[PH Test] Proportional hazard assumption...\n")
  best_cox <- if (!is.null(models_duration$D2_extended)) models_duration$D2_extended else
               if (!is.null(models_duration$D1_core)) models_duration$D1_core else NULL
  if (!is.null(best_cox)) {
    ph <- tryCatch(cox.zph(best_cox), error = function(e) NULL)
    if (!is.null(ph)) {
      cat("  Global test:\n")
      print(ph$table)
    }
  }

  # ---------- Export ----------
  cat("\nExporting duration results...\n")
  valid_d <- models_duration[!sapply(models_duration, is.null)]
  if (length(valid_d) > 0) {
    res_d <- purrr::map_df(names(valid_d), function(mn) {
      tt <- tidy_model(valid_d[[mn]], mn)
      if (!is.null(tt)) tt$hazard_ratio <- exp(tt$estimate)
      tt
    })
    if (!is.null(res_d) && nrow(res_d) > 0) {
      write_csv(res_d, file.path(CONFIG$paths$tables, "duration_results.csv"))
      cat(sprintf("  Saved: %s (%d rows)\n",
                  file.path(CONFIG$paths$tables, "duration_results.csv"), nrow(res_d)))
    }
  }

} else {
  cat("  econ_duration.rds not found. Skipping.\n")
}

# ==============================================================================
# MODEL COMPARISON TABLE
# ==============================================================================

cat("\n", rep("=", 78), "\n", sep = "")
cat("MODEL COMPARISON\n")
cat(rep("-", 78), "\n", sep = "")

comparison_rows <- list()

# Premium
if (exists("models_premium")) {
  for (mn in names(models_premium)) {
    m <- models_premium[[mn]]
    if (is.null(m)) next
    comparison_rows[[length(comparison_rows) + 1]] <- tibble(
      type = "Premium", model = mn,
      n_obs = nobs(m),
      r2 = tryCatch(r2(m)["r2"], error = function(e) NA_real_),
      adj_r2 = tryCatch(r2(m)["adj.r2"], error = function(e) NA_real_),
      n_coefs = length(coef(m))
    )
  }
}

# Completion
if (exists("models_completion")) {
  for (mn in names(models_completion)) {
    m <- models_completion[[mn]]
    if (is.null(m)) next
    comparison_rows[[length(comparison_rows) + 1]] <- tibble(
      type = "Completion", model = mn,
      n_obs = nobs(m),
      r2 = tryCatch(r2(m, type = "pr2"), error = function(e) NA_real_),
      adj_r2 = NA_real_,
      n_coefs = length(coef(m))
    )
  }
}

# Duration
if (exists("models_duration")) {
  for (mn in names(models_duration)) {
    m <- models_duration[[mn]]
    if (is.null(m)) next
    comparison_rows[[length(comparison_rows) + 1]] <- tibble(
      type = "Duration", model = mn,
      n_obs = m$n,
      r2 = tryCatch(summary(m)$concordance[1], error = function(e) NA_real_),
      adj_r2 = NA_real_,
      n_coefs = length(coef(m))
    )
  }
}

if (length(comparison_rows) > 0) {
  comp_df <- bind_rows(comparison_rows)
  write_csv(comp_df, file.path(CONFIG$paths$tables, "model_comparison.csv"))
  cat(sprintf("  Saved: %s (%d models)\n",
              file.path(CONFIG$paths$tables, "model_comparison.csv"), nrow(comp_df)))
  print(comp_df)
}

# ==============================================================================
# REPORT
# ==============================================================================

cat("\nGenerating report...\n")

report <- c(
  "# Module 09 — Econometric Models Report (v6)", "",
  paste0("Generated: ", Sys.time()), "",
  "## Variable mapping (v6)",
  "| Role | Variable | Description |",
  "|------|----------|-------------|",
  "| Core NLP | operational_specificity_raw_z | Numeric + compound density (standardized) |",
  "| Core NLP | fl_sentence_share_z | FL sentences / total sentences (standardized) |",
  "| Core NLP | risk_transparency_z | 1 - cosine(doc, peer centroid) (standardized) |",
  "| Core NLP | tone_lm_z | (Pos-Neg)/(Pos+Neg+1) (standardized) |",
  "| Supplementary | fl_precision_share_z | FL sentences with numbers / FL sentences |",
  "| Supplementary | commitment_density_z | Strong modals per 1000 words |",
  "| Supplementary | hedging_density_z | Weak modals per 1000 words |",
  "| Processing cost | mda_fog_z | Gunning Fog (MD&A) |",
  "| Processing cost | mda_log_length_z | log(word count) (MD&A) |",
  "| Outcome | premium_1d_w | Premium 1-day prior (winsorized) |",
  "| Outcome | completion | 1=Completed, 0=Withdrawn |",
  "| Outcome | time_to_close_w | Days to close (winsorized) |",
  "| FE | fe_year | Year FE |",
  "| FE | fe_ind_year | Industry × Year FE |",
  "| Clustering | sic2 | 2-digit SIC |",
  "",
  "## Model specifications",
  "### Premium (OLS via feols, clustered at sic2)",
  "- P1: Core 4 + controls + Year FE",
  "- P2: Core 4 + controls + Ind×Year FE",
  "- P3: Core 4 + supplementary + controls + Year FE",
  "- P4: Core 4 + processing costs + controls + Year FE",
  "- P5_*: Individual indices (one-at-a-time)",
  "",
  "### Completion (Logit via feglm, clustered at sic2)",
  "- C1: Core 4 + controls + Year FE",
  "- C2: Core 4 + controls + Ind×Year FE",
  "- C3: Extended + controls + Year FE",
  "- C4: Core 4 + processing costs + controls + Year FE",
  "",
  "### Duration (Cox PH via coxph, clustered at sic2)",
  "- D1: Core 4 + controls + strata(year)",
  "- D2: Extended + controls + strata(year)",
  "- D3: Core 4 + processing costs + strata(year)",
  "",
  "## Notes",
  "- All NLP indices standardized (_z) for coefficient comparability",
  "- Raw indices used + FE absorbs industry×year means (avoids double de-meaning)",
  "- Winsorization at 1%/99% for continuous outcomes and deal value",
  "- Premium bounded [-100, 300] with sample flag"
)

writeLines(report, file.path(CONFIG$paths$reports, "09_econometric_report.md"))

# ==============================================================================
# SUMMARY
# ==============================================================================

cat("\n", rep("=", 78), "\n", sep = "")
cat("MODULE 09 COMPLETE (v6)\n")
cat(rep("=", 78), "\n", sep = "")

n_prem <- sum(!sapply(models_premium, is.null))
n_comp <- sum(!sapply(models_completion, is.null))
n_dur  <- sum(!sapply(models_duration, is.null))

cat(sprintf("\nModels estimated: %d premium + %d completion + %d duration = %d total\n",
            n_prem, n_comp, n_dur, n_prem + n_comp + n_dur))

cat("\nOutput files:\n")
cat(sprintf("  %s/premium_results.csv\n", CONFIG$paths$tables))
cat(sprintf("  %s/completion_results.csv\n", CONFIG$paths$tables))
cat(sprintf("  %s/duration_results.csv\n", CONFIG$paths$tables))
cat(sprintf("  %s/model_comparison.csv\n", CONFIG$paths$tables))
cat(sprintf("  %s/09_econometric_report.md\n", CONFIG$paths$reports))

# Return all models
invisible(list(
  premium = models_premium,
  completion = models_completion,
  duration = models_duration
))
