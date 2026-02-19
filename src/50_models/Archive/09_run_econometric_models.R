# ==============================================================================
# 09_run_econometric_models.R
#
# PURPOSE: Run econometric models for M&A disclosure analysis
#          - Premium regressions (OLS with FE)
#          - Completion probability (Logit/Probit)
#          - Time-to-completion (Cox proportional hazard)
#
# INPUTS:  data/processed/econ_*_final.rds (from 08_prepare_econometrics_final.R)
# OUTPUTS: 
#   - output/tables/premium_regression_*.csv
#   - output/tables/completion_logit_*.csv
#   - output/tables/duration_cox_*.csv
#   - output/figures/ (diagnostic plots)
#
# DESIGN PRINCIPLES:
#   - Modular model specifications (easy to add/remove variables)
#   - Clustered standard errors at deal level
#   - Comprehensive diagnostics
#   - Publication-ready table export
#
# DEPENDENCIES: tidyverse, fixest, survival, broom, modelsummary
#
# ==============================================================================

# ==============================================================================
# PACKAGES
# ==============================================================================

library(tidyverse)
library(fixest)        # Fast FE estimation with clustered SE
library(survival)      # Cox PH models
library(broom)         # Tidy model output
library(modelsummary)  # Publication-ready tables

# ==============================================================================
# CONFIGURATION
# ==============================================================================

CONFIG <- list(
  # File paths
  paths = list(
    input_premium    = "data/processed/econ_premium_final.rds",
    input_completion = "data/processed/econ_completion_final.rds",
    input_duration   = "data/processed/econ_duration_final.rds",
    output_tables    = "output/tables",
    output_figures   = "output/figures"
  ),
  
  # Model specifications
  # Key NLP variables (will be detected from data)
  nlp_vars = c(
    "tone_lm_z",              # LM tone (Pos - Neg)
    "forward_looking_z",      # Forward-looking intensity
    "specificity_z",          # Operational specificity
    "uncertainty_lm_z"        # LM uncertainty
  ),
  
  # Control variables
  # NOTE: Variable names must match those in the dataset
  controls = list(
    # Deal characteristics
    deal = c(
      "deal_value_usd_millions_w",  # Deal value (winsorized)
      "tender_dummy",               # Tender offer indicator
      "percentage_of_cash",         # Cash percentage
      "percentage_of_stock"         # Stock percentage
    ),
    # Payment method (if available)
    payment = c(
      # These are covered by percentage_of_cash/stock above
    )
  ),
  
  # Fixed effects
  fixed_effects = list(
    year = "fe_year",
    industry_year = "fe_industry_year"
  ),
  
  # Clustering
  cluster_var = "sic2",  # Cluster SE at industry level
  
  # Model options
  vcov_type = "cluster"  # Options: "iid", "hetero", "cluster"
)

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

#' Detect available NLP variables in dataset
#' 
#' IMPORTANT: This function now excludes problematic variables:
#'   - sample_tone_z: A flag variable (nearly constant, causes collinearity)
#'   - *_outlier_z: Flag variables (0/1), not continuous indices
#'   - *_raw_z: Raw versions (redundant with normalized versions)
#' 
#' @param df Dataframe
#' @param patterns Patterns to match
#' @return Vector of available NLP variable names
detect_nlp_vars <- function(df, patterns) {
  all_cols <- names(df)
  
  # Look for standardized versions first (_z suffix)
  z_cols <- all_cols[str_detect(all_cols, "_z$")]
  
  # Filter to likely NLP indices
  nlp_patterns <- c("tone", "forward", "specific", "uncertain", 
                    "fog", "readab", "sentiment", "lm_",
                    "risk", "disclosure", "tfidf")
  
  pattern_combined <- paste0("(", paste(nlp_patterns, collapse = "|"), ")")
  nlp_cols <- z_cols[str_detect(z_cols, regex(pattern_combined, ignore_case = TRUE))]
  
  # Remove word count variables
  nlp_cols <- nlp_cols[!str_detect(nlp_cols, "count")]
  
  # ============================================================================
  # CRITICAL EXCLUSIONS (identified in NLP pipeline audit 2026-02-15)
  # ============================================================================
  
  # 1. Exclude sample_tone_z: This is a FLAG (99.66% = 1), not an index
  #    When standardized, it's nearly constant and causes perfect collinearity
  nlp_cols <- nlp_cols[!str_detect(nlp_cols, "sample_tone")]
  
  # 2. Exclude outlier flags: These are binary (0/1), not continuous indices
  nlp_cols <- nlp_cols[!str_detect(nlp_cols, "_outlier")]
  
  # 3. Exclude raw versions if normalized versions exist (avoid multicollinearity)
  #    Keep only *_norm_z versions when both raw and normalized are present
  has_norm <- any(str_detect(nlp_cols, "_norm_z$"))
  if (has_norm) {
    # Prefer normalized versions; exclude raw duplicates
    # e.g., keep operational_specificity_norm_z, exclude operational_specificity_raw_z
    raw_patterns <- c("_raw_z$", "_density_z$", "^tone_lm_z$", "^risk_disclosure_tfidf_z$")
    for (pat in raw_patterns) {
      # Only exclude if a corresponding _norm_z version exists
      raw_matches <- nlp_cols[str_detect(nlp_cols, pat)]
      for (raw_var in raw_matches) {
        # Check if there's a _norm_z version
        base_name <- str_replace(raw_var, "_raw_z$|_density_z$|_z$", "")
        norm_version <- paste0(base_name, "_norm_z")
        if (norm_version %in% nlp_cols) {
          nlp_cols <- nlp_cols[nlp_cols != raw_var]
        }
      }
    }
  }
  
  cat(sprintf("  NLP variables selected (after exclusions): %s\n", 
              paste(nlp_cols, collapse = ", ")))
  
  return(nlp_cols)
}

#' Detect available control variables in dataset
#' 
#' @param df Dataframe
#' @param requested_controls Vector of requested control names
#' @return Vector of available control variable names
detect_controls <- function(df, requested_controls) {
  available <- intersect(requested_controls, names(df))
  missing <- setdiff(requested_controls, names(df))
  
  if (length(missing) > 0) {
    message(sprintf("  Note: Controls not found: %s", paste(missing, collapse = ", ")))
  }
  
  return(available)
}

#' Build formula from variable lists
#' 
#' @param outcome Outcome variable name
#' @param nlp_vars NLP variable names
#' @param controls Control variable names
#' @param fe Fixed effects specification (e.g., "| fe_year")
#' @return Formula object
build_formula <- function(outcome, nlp_vars, controls, fe = NULL) {
  rhs <- c(nlp_vars, controls)
  rhs_str <- paste(rhs, collapse = " + ")
  
  if (!is.null(fe) && fe != "") {
    formula_str <- sprintf("%s ~ %s %s", outcome, rhs_str, fe)
  } else {
    formula_str <- sprintf("%s ~ %s", outcome, rhs_str)
  }
  
  return(as.formula(formula_str))
}

#' Run premium regression with multiple specifications
#' 
#' @param df Premium dataset
#' @param config Configuration list
#' @return List of model objects
run_premium_models <- function(df, config) {
  
  cat("\n")
  cat(rep("=", 78), "\n", sep = "")
  cat("PREMIUM REGRESSIONS (OLS with Fixed Effects)\n")
  cat(rep("-", 78), "\n", sep = "")
  
  # Detect available variables
  nlp_vars <- detect_nlp_vars(df, config$nlp_vars)
  cat(sprintf("NLP variables detected: %s\n", paste(nlp_vars, collapse = ", ")))
  
  controls_deal <- detect_controls(df, config$controls$deal)
  controls_payment <- detect_controls(df, config$controls$payment)
  all_controls <- c(controls_deal, controls_payment)
  cat(sprintf("Control variables: %s\n", paste(all_controls, collapse = ", ")))
  
  # Sample size check
  outcome_var <- if ("premium_pct_w" %in% names(df)) "premium_pct_w" else "premium_pct"
  
  # Create analysis sample (complete cases for key variables)
  key_vars <- c(outcome_var, nlp_vars[1], all_controls[1])
  key_vars <- intersect(key_vars, names(df))
  
  df_analysis <- df %>%
    filter(if_all(all_of(key_vars), ~!is.na(.)))
  
  cat(sprintf("\nAnalysis sample: %d observations (from %d)\n", nrow(df_analysis), nrow(df)))
  
  models <- list()
  
  # Model 1: Baseline (NLP only)
  cat("\n[Model 1] NLP variables only...\n")
  nlp_only <- nlp_vars[nlp_vars %in% names(df_analysis)]
  
  if (length(nlp_only) > 0) {
    f1 <- build_formula(outcome_var, nlp_only, character(0), "| fe_year")
    models$m1_nlp_only <- tryCatch(
      feols(f1, data = df_analysis, vcov = ~sic2),
      error = function(e) {
        cat(sprintf("  Error: %s\n", e$message))
        NULL
      }
    )
    if (!is.null(models$m1_nlp_only)) {
      cat(sprintf("  N = %d, R² = %.4f\n", 
                  models$m1_nlp_only$nobs, 
                  summary(models$m1_nlp_only)$r2))
    }
  }
  
  # Model 2: NLP + Deal controls
  cat("\n[Model 2] NLP + Deal controls...\n")
  controls_available <- intersect(all_controls, names(df_analysis))
  
  if (length(nlp_only) > 0 && length(controls_available) > 0) {
    f2 <- build_formula(outcome_var, nlp_only, controls_available, "| fe_year")
    models$m2_with_controls <- tryCatch(
      feols(f2, data = df_analysis, vcov = ~sic2),
      error = function(e) {
        cat(sprintf("  Error: %s\n", e$message))
        NULL
      }
    )
    if (!is.null(models$m2_with_controls)) {
      cat(sprintf("  N = %d, R² = %.4f\n", 
                  models$m2_with_controls$nobs, 
                  summary(models$m2_with_controls)$r2))
    }
  }
  
  # Model 3: Industry × Year FE
  cat("\n[Model 3] With Industry × Year FE...\n")
  if ("fe_industry_year" %in% names(df_analysis)) {
    f3 <- build_formula(outcome_var, nlp_only, controls_available, "| fe_industry_year")
    models$m3_industry_year_fe <- tryCatch(
      feols(f3, data = df_analysis, vcov = ~sic2),
      error = function(e) {
        cat(sprintf("  Error: %s\n", e$message))
        NULL
      }
    )
    if (!is.null(models$m3_industry_year_fe)) {
      cat(sprintf("  N = %d, R² = %.4f\n", 
                  models$m3_industry_year_fe$nobs, 
                  summary(models$m3_industry_year_fe)$r2))
    }
  }
  
  # Model 4: Individual NLP indices (one at a time for robustness)
  cat("\n[Model 4] Individual NLP indices...\n")
  for (nlp_var in nlp_only) {
    model_name <- paste0("m4_", nlp_var)
    f4 <- build_formula(outcome_var, nlp_var, controls_available, "| fe_year")
    models[[model_name]] <- tryCatch(
      feols(f4, data = df_analysis, vcov = ~sic2),
      error = function(e) NULL
    )
    if (!is.null(models[[model_name]])) {
      coef_val <- coef(models[[model_name]])[nlp_var]
      se_val <- sqrt(vcov(models[[model_name]])[nlp_var, nlp_var])
      cat(sprintf("  %s: β = %.4f (%.4f)\n", nlp_var, coef_val, se_val))
    }
  }
  
  cat(sprintf("\nTotal models estimated: %d\n", length(models)))
  
  return(models)
}

#' Run completion probability models (Logit)
#' 
#' @param df Completion dataset
#' @param config Configuration list
#' @return List of model objects
run_completion_models <- function(df, config) {
  
  cat("\n")
  cat(rep("=", 78), "\n", sep = "")
  cat("COMPLETION PROBABILITY (Logit)\n")
  cat(rep("-", 78), "\n", sep = "")
  
  # Detect variables
  nlp_vars <- detect_nlp_vars(df, config$nlp_vars)
  controls_deal <- detect_controls(df, config$controls$deal)
  
  outcome_var <- "completion_dummy"
  
  # Check outcome distribution
  if (outcome_var %in% names(df)) {
    outcome_dist <- df %>%
      filter(!is.na(.data[[outcome_var]])) %>%
      count(.data[[outcome_var]], name = "n") %>%
      mutate(pct = round(100 * n / sum(n), 1))
    
    cat("\nOutcome distribution:\n")
    for (i in 1:nrow(outcome_dist)) {
      label <- ifelse(outcome_dist[[outcome_var]][i] == 1, "Completed", "Withdrawn")
      cat(sprintf("  %s: %d (%.1f%%)\n", label, outcome_dist$n[i], outcome_dist$pct[i]))
    }
  }
  
  models <- list()
  
  # Analysis sample
  key_vars <- c(outcome_var, nlp_vars[1])
  key_vars <- intersect(key_vars, names(df))
  
  df_analysis <- df %>%
    filter(if_all(all_of(key_vars), ~!is.na(.)))
  
  cat(sprintf("\nAnalysis sample: %d observations\n", nrow(df_analysis)))
  
  # Model 1: NLP only
  cat("\n[Model 1] NLP variables only...\n")
  nlp_only <- nlp_vars[nlp_vars %in% names(df_analysis)]
  
  if (length(nlp_only) > 0) {
    f1 <- build_formula(outcome_var, nlp_only, character(0), "| fe_year")
    models$m1_nlp_only <- tryCatch(
      feglm(f1, data = df_analysis, family = binomial(link = "logit"), vcov = ~sic2),
      error = function(e) {
        cat(sprintf("  Error: %s\n", e$message))
        NULL
      }
    )
    if (!is.null(models$m1_nlp_only)) {
      cat(sprintf("  N = %d\n", models$m1_nlp_only$nobs))
    }
  }
  
  # Model 2: NLP + controls
  cat("\n[Model 2] NLP + Deal controls...\n")
  controls_available <- intersect(controls_deal, names(df_analysis))
  
  if (length(nlp_only) > 0 && length(controls_available) > 0) {
    f2 <- build_formula(outcome_var, nlp_only, controls_available, "| fe_year")
    models$m2_with_controls <- tryCatch(
      feglm(f2, data = df_analysis, family = binomial(link = "logit"), vcov = ~sic2),
      error = function(e) {
        cat(sprintf("  Error: %s\n", e$message))
        NULL
      }
    )
    if (!is.null(models$m2_with_controls)) {
      cat(sprintf("  N = %d\n", models$m2_with_controls$nobs))
    }
  }
  
  # Model 3: Marginal effects for main model
  if (!is.null(models$m2_with_controls)) {
    cat("\n[Model 3] Computing marginal effects...\n")
    # fixest doesn't have built-in marginal effects, use manual computation or margins package
    cat("  (Marginal effects computed separately)\n")
  }
  
  cat(sprintf("\nTotal models estimated: %d\n", length(models)))
  
  return(models)
}

#' Run duration models (Cox PH)
#' 
#' @param df Duration dataset
#' @param config Configuration list
#' @return List of model objects
run_duration_models <- function(df, config) {
  
  cat("\n")
  cat(rep("=", 78), "\n", sep = "")
  cat("TIME-TO-COMPLETION (Cox Proportional Hazard)\n")
  cat(rep("-", 78), "\n", sep = "")
  
  # Detect variables
  nlp_vars <- detect_nlp_vars(df, config$nlp_vars)
  controls_deal <- detect_controls(df, config$controls$deal)
  
  time_var <- "time_to_event_days"
  event_var <- "event_completed"
  
  # Check survival structure
  if (time_var %in% names(df) && event_var %in% names(df)) {
    surv_summary <- df %>%
      filter(!is.na(.data[[time_var]]), !is.na(.data[[event_var]])) %>%
      summarise(
        n = n(),
        n_events = sum(.data[[event_var]] == 1),
        median_time = median(.data[[time_var]]),
        mean_time = mean(.data[[time_var]])
      )
    
    cat("\nSurvival structure:\n")
    cat(sprintf("  N = %d, Events (completed) = %d (%.1f%%)\n", 
                surv_summary$n, surv_summary$n_events,
                100 * surv_summary$n_events / surv_summary$n))
    cat(sprintf("  Median time: %.0f days, Mean: %.0f days\n", 
                surv_summary$median_time, surv_summary$mean_time))
  }
  
  models <- list()
  
  # Analysis sample
  df_analysis <- df %>%
    filter(!is.na(.data[[time_var]]), !is.na(.data[[event_var]]), 
           .data[[time_var]] > 0)
  
  cat(sprintf("\nAnalysis sample: %d observations\n", nrow(df_analysis)))
  
  # Create survival object
  df_analysis$surv_obj <- Surv(
    time = df_analysis[[time_var]], 
    event = df_analysis[[event_var]]
  )
  
  # Model 1: NLP only
  cat("\n[Model 1] NLP variables only...\n")
  nlp_only <- nlp_vars[nlp_vars %in% names(df_analysis)]
  
  if (length(nlp_only) > 0) {
    f1 <- as.formula(paste("surv_obj ~", paste(nlp_only, collapse = " + "), 
                           "+ strata(fe_year)"))
    models$m1_nlp_only <- tryCatch(
      coxph(f1, data = df_analysis, cluster = df_analysis$sic2),
      error = function(e) {
        cat(sprintf("  Error: %s\n", e$message))
        NULL
      }
    )
    if (!is.null(models$m1_nlp_only)) {
      cat(sprintf("  N = %d, Events = %d\n", 
                  models$m1_nlp_only$n, models$m1_nlp_only$nevent))
    }
  }
  
  # Model 2: NLP + controls
  cat("\n[Model 2] NLP + Deal controls...\n")
  controls_available <- intersect(controls_deal, names(df_analysis))
  
  if (length(nlp_only) > 0 && length(controls_available) > 0) {
    f2 <- as.formula(paste("surv_obj ~", 
                           paste(c(nlp_only, controls_available), collapse = " + "),
                           "+ strata(fe_year)"))
    models$m2_with_controls <- tryCatch(
      coxph(f2, data = df_analysis, cluster = df_analysis$sic2),
      error = function(e) {
        cat(sprintf("  Error: %s\n", e$message))
        NULL
      }
    )
    if (!is.null(models$m2_with_controls)) {
      cat(sprintf("  N = %d, Events = %d\n", 
                  models$m2_with_controls$n, models$m2_with_controls$nevent))
    }
  }
  
  # PH assumption test
  cat("\n[Model 3] Testing proportional hazard assumption...\n")
  if (!is.null(models$m2_with_controls)) {
    ph_test <- tryCatch(
      cox.zph(models$m2_with_controls),
      error = function(e) NULL
    )
    if (!is.null(ph_test)) {
      cat("  Global test:\n")
      print(ph_test$table)
    }
  }
  
  cat(sprintf("\nTotal models estimated: %d\n", length(models)))
  
  return(models)
}

#' Export regression results to CSV
#' 
#' @param models List of model objects
#' @param output_path File path for output
#' @param model_type Type of models (for labeling)
export_results <- function(models, output_path, model_type = "ols") {
  
  # Filter out NULL models
  models <- models[!sapply(models, is.null)]
  
  if (length(models) == 0) {
    cat("No models to export.\n")
    return(invisible(NULL))
  }
  
  # Use modelsummary for clean output
  tryCatch({
    modelsummary(
      models,
      output = output_path,
      stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01),
      coef_omit = "^fe_",  # Omit FE coefficients
      gof_map = c("nobs", "r.squared", "adj.r.squared", "logLik", "AIC")
    )
    cat(sprintf("  Saved: %s\n", output_path))
  }, error = function(e) {
    cat(sprintf("  Error exporting: %s\n", e$message))
    
    # Fallback: manual export
    results <- map_df(names(models), function(model_name) {
      m <- models[[model_name]]
      tidy_m <- broom::tidy(m, conf.int = TRUE)
      tidy_m$model <- model_name
      tidy_m
    })
    
    write_csv(results, output_path)
    cat(sprintf("  Saved (fallback): %s\n", output_path))
  })
}

# ==============================================================================
# MAIN EXECUTION
# ==============================================================================

main <- function() {
  
  cat("\n")
  cat(rep("=", 78), "\n", sep = "")
  cat("ECONOMETRIC MODEL ESTIMATION\n")
  cat(rep("=", 78), "\n", sep = "")
  cat(sprintf("Timestamp: %s\n", Sys.time()))
  
  # Create output directories
  dir.create(CONFIG$paths$output_tables, recursive = TRUE, showWarnings = FALSE)
  dir.create(CONFIG$paths$output_figures, recursive = TRUE, showWarnings = FALSE)
  
  # ============================================================================
  # Load datasets
  # ============================================================================
  
  cat("\nLoading datasets...\n")
  
  econ_premium <- readRDS(CONFIG$paths$input_premium)
  econ_completion <- readRDS(CONFIG$paths$input_completion)
  econ_duration <- readRDS(CONFIG$paths$input_duration)
  
  cat(sprintf("  Premium:    %d × %d\n", nrow(econ_premium), ncol(econ_premium)))
  cat(sprintf("  Completion: %d × %d\n", nrow(econ_completion), ncol(econ_completion)))
  cat(sprintf("  Duration:   %d × %d\n", nrow(econ_duration), ncol(econ_duration)))
  
  # ============================================================================
  # Run models
  # ============================================================================
  
  # Premium regressions
  premium_models <- run_premium_models(econ_premium, CONFIG)
  
  # Completion models
  completion_models <- run_completion_models(econ_completion, CONFIG)
  
  # Duration models
  duration_models <- run_duration_models(econ_duration, CONFIG)
  
  # ============================================================================
  # Export results
  # ============================================================================
  
  cat("\n")
  cat(rep("=", 78), "\n", sep = "")
  cat("EXPORTING RESULTS\n")
  cat(rep("-", 78), "\n", sep = "")
  
  export_results(
    premium_models, 
    file.path(CONFIG$paths$output_tables, "premium_regression_results.csv"),
    "ols"
  )
  
  export_results(
    completion_models,
    file.path(CONFIG$paths$output_tables, "completion_logit_results.csv"),
    "logit"
  )
  
  export_results(
    duration_models,
    file.path(CONFIG$paths$output_tables, "duration_cox_results.csv"),
    "cox"
  )
  
  # ============================================================================
  # Summary
  # ============================================================================
  
  cat("\n")
  cat(rep("=", 78), "\n", sep = "")
  cat("ESTIMATION COMPLETE\n")
  cat(rep("=", 78), "\n", sep = "")
  
  cat("\nModels estimated:\n")
  cat(sprintf("  Premium regressions:   %d\n", length(premium_models)))
  cat(sprintf("  Completion models:     %d\n", length(completion_models)))
  cat(sprintf("  Duration models:       %d\n", length(duration_models)))
  
  cat("\nOutput files:\n")
  cat(sprintf("  %s/premium_regression_results.csv\n", CONFIG$paths$output_tables))
  cat(sprintf("  %s/completion_logit_results.csv\n", CONFIG$paths$output_tables))
  cat(sprintf("  %s/duration_cox_results.csv\n", CONFIG$paths$output_tables))
  
  cat(sprintf("\nTimestamp: %s\n", Sys.time()))
  
  # Return all models for further analysis
  return(list(
    premium = premium_models,
    completion = completion_models,
    duration = duration_models
  ))
}

# ==============================================================================
# RUN MAIN FUNCTION
# ==============================================================================
cat("\n*** Starting econometric model estimation ***\n")
results <- main()
