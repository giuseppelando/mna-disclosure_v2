# ==============================================================================
# 10_run_enhanced_models.R
# ==============================================================================
#
# PURPOSE: Run enhanced econometric models using:
#   - Original 4 NLP indices (operational specificity, forward-looking, risk, tone)
#   - New indices (readability, modal certainty, comparative intensity)
#   - Interaction effects
#   - Multiple specifications for robustness
#
# REQUIRES: Run 07_nlp_enhancement.R first to generate enhanced indices
#
# OUTPUTS:
#   - output/tables/enhanced_premium_results.csv
#   - output/tables/enhanced_completion_results.csv
#   - output/tables/model_comparison.csv
#   - reports/enhanced_models_report.md
#
# ==============================================================================

library(tidyverse)
library(fixest)
library(survival)
library(modelsummary)

cat("
==============================================================================
ENHANCED ECONOMETRIC MODELS
==============================================================================
")
cat(sprintf("Timestamp: %s\n\n", Sys.time()))

# ==============================================================================
# CONFIGURATION
# ==============================================================================

CONFIG <- list(
  paths = list(
    # Try enhanced indices first, fall back to original
    indices_enhanced = "data/interim/indices_enhanced.rds",
    indices_original = "data/interim/disclosure_indices.rds",
    base_data = "data/processed/deals_with_10k_text_analysis.rds",
    econ_data = "data/processed/econ_premium_final.rds",
    output_tables = "output/tables",
    output_report = "reports/enhanced_models_report.md"
  ),
  
  # Core NLP indices (original)
  core_indices = c(
    "operational_specificity_norm",
    "forward_looking_norm",
    "risk_disclosure_tfidf_norm",
    "tone_lm_norm"
  ),
  
  # New indices (from enhancement module)
  new_indices = c(
    "reading_difficulty_norm",
    "modal_certainty_raw",
    "comparative_density"
  ),
  
  # Control variables
  controls = c(
    "deal_value_usd_millions_w",
    "tender_dummy",
    "percentage_of_cash",
    "percentage_of_stock"
  ),
  
  # Fixed effects
  fe_year = "fe_year",
  fe_industry_year = "fe_industry_year",
  
  # Clustering
  cluster_var = "sic2"
)

# ==============================================================================
# LOAD DATA
# ==============================================================================

cat("Loading data...\n")

# Try enhanced indices first
if (file.exists(CONFIG$paths$indices_enhanced)) {
  indices <- readRDS(CONFIG$paths$indices_enhanced)
  cat("  - Loaded ENHANCED indices\n")
  has_enhanced <- TRUE
} else if (file.exists(CONFIG$paths$indices_original)) {
  indices <- readRDS(CONFIG$paths$indices_original)
  cat("  - Loaded ORIGINAL indices (run 07_nlp_enhancement.R for new features)\n")
  has_enhanced <- FALSE
} else {
  stop("No indices file found. Run the NLP pipeline first.")
}

# Load econometric-ready data if available
if (file.exists(CONFIG$paths$econ_data)) {
  econ_data <- readRDS(CONFIG$paths$econ_data)
  cat(sprintf("  - Loaded econometric data: %d × %d\n", nrow(econ_data), ncol(econ_data)))
  
  # Merge enhanced indices with econometric data
  indices$deal_id <- as.character(indices$deal_id)
  econ_data$deal_id <- as.character(econ_data$deal_id)
  
  # Get columns from indices that aren't in econ_data
  new_cols <- setdiff(names(indices), names(econ_data))
  new_cols <- c("deal_id", new_cols)
  
  df <- econ_data %>%
    left_join(indices[, new_cols], by = "deal_id")
  
} else {
  # Use indices directly
  df <- indices
  cat("  - Using indices directly (no econometric prep)\n")
}

cat(sprintf("  - Analysis data: %d × %d\n", nrow(df), ncol(df)))

# ==============================================================================
# IDENTIFY AVAILABLE VARIABLES
# ==============================================================================

cat("\nIdentifying available variables...\n")

# Check which indices are available
available_core <- intersect(CONFIG$core_indices, names(df))
available_new <- intersect(CONFIG$new_indices, names(df))
available_controls <- intersect(CONFIG$controls, names(df))

cat(sprintf("  - Core indices: %d/%d available\n", 
            length(available_core), length(CONFIG$core_indices)))
cat(sprintf("  - New indices: %d/%d available\n",
            length(available_new), length(CONFIG$new_indices)))
cat(sprintf("  - Controls: %d/%d available\n",
            length(available_controls), length(CONFIG$controls)))

if (length(available_core) == 0) {
  stop("No core NLP indices found in data")
}

# Check for outcome variables
has_premium <- "premium_pct_w" %in% names(df) || "premium" %in% names(df)
has_completion <- "completion_dummy" %in% names(df) || "completion" %in% names(df)
has_duration <- all(c("time_to_event_days", "event_completed") %in% names(df))

cat(sprintf("  - Premium outcome: %s\n", ifelse(has_premium, "YES", "NO")))
cat(sprintf("  - Completion outcome: %s\n", ifelse(has_completion, "YES", "NO")))
cat(sprintf("  - Duration outcome: %s\n", ifelse(has_duration, "YES", "NO")))

# Standardize outcome names
if (!"premium_pct_w" %in% names(df) && "premium" %in% names(df)) {
  df$premium_pct_w <- df$premium
}
if (!"completion_dummy" %in% names(df) && "completion" %in% names(df)) {
  df$completion_dummy <- df$completion
}

# ==============================================================================
# HELPER: Build formula
# ==============================================================================

build_formula <- function(outcome, nlp_vars, controls, fe = NULL) {
  rhs <- paste(c(nlp_vars, controls), collapse = " + ")
  if (!is.null(fe)) {
    formula_str <- sprintf("%s ~ %s | %s", outcome, rhs, fe)
  } else {
    formula_str <- sprintf("%s ~ %s", outcome, rhs)
  }
  as.formula(formula_str)
}

# ==============================================================================
# MODEL SPECIFICATIONS
# ==============================================================================

cat("\n")
cat(rep("=", 78), "\n", sep = "")
cat("PREMIUM REGRESSIONS\n")
cat(rep("-", 78), "\n", sep = "")

if (!has_premium) {
  cat("Skipping: No premium variable available\n")
} else {
  
  # Prepare analysis sample
  df_premium <- df %>%
    filter(!is.na(premium_pct_w)) %>%
    filter(if_all(all_of(available_core), ~!is.na(.)))
  
  cat(sprintf("Analysis sample: %d observations\n\n", nrow(df_premium)))
  
  models_premium <- list()
  
  # ─────────────────────────────────────────────────────────────────────────
  # Model 1: Core indices only (baseline)
  # ─────────────────────────────────────────────────────────────────────────
  cat("[Model 1] Core indices + Year FE...\n")
  
  f1 <- build_formula("premium_pct_w", available_core, available_controls, CONFIG$fe_year)
  
  models_premium$m1_core <- tryCatch({
    feols(f1, data = df_premium, vcov = ~sic2)
  }, error = function(e) {
    cat(sprintf("  Error: %s\n", e$message))
    NULL
  })
  
  if (!is.null(models_premium$m1_core)) {
    cat(sprintf("  N = %d, R² = %.3f\n", 
                nobs(models_premium$m1_core),
                r2(models_premium$m1_core)["r2"]))
  }
  
  # ─────────────────────────────────────────────────────────────────────────
  # Model 2: Core indices + Industry×Year FE
  # ─────────────────────────────────────────────────────────────────────────
  cat("[Model 2] Core indices + Industry×Year FE...\n")
  
  if (CONFIG$fe_industry_year %in% names(df_premium)) {
    f2 <- build_formula("premium_pct_w", available_core, available_controls, 
                        CONFIG$fe_industry_year)
    
    models_premium$m2_core_iy <- tryCatch({
      feols(f2, data = df_premium, vcov = ~sic2)
    }, error = function(e) {
      cat(sprintf("  Error: %s\n", e$message))
      NULL
    })
    
    if (!is.null(models_premium$m2_core_iy)) {
      cat(sprintf("  N = %d, R² = %.3f\n", 
                  nobs(models_premium$m2_core_iy),
                  r2(models_premium$m2_core_iy)["r2"]))
    }
  }
  
  # ─────────────────────────────────────────────────────────────────────────
  # Model 3: Core + New indices (if available)
  # ─────────────────────────────────────────────────────────────────────────
  if (length(available_new) > 0) {
    cat("[Model 3] Core + NEW indices...\n")
    
    all_nlp <- c(available_core, available_new)
    
    # Filter to non-NA for new indices
    df_premium_new <- df_premium %>%
      filter(if_all(all_of(available_new), ~!is.na(.)))
    
    f3 <- build_formula("premium_pct_w", all_nlp, available_controls, CONFIG$fe_year)
    
    models_premium$m3_extended <- tryCatch({
      feols(f3, data = df_premium_new, vcov = ~sic2)
    }, error = function(e) {
      cat(sprintf("  Error: %s\n", e$message))
      NULL
    })
    
    if (!is.null(models_premium$m3_extended)) {
      cat(sprintf("  N = %d, R² = %.3f\n", 
                  nobs(models_premium$m3_extended),
                  r2(models_premium$m3_extended)["r2"]))
      
      # Print new index coefficients
      coefs <- coef(models_premium$m3_extended)
      ses <- sqrt(diag(vcov(models_premium$m3_extended)))
      
      cat("\n  New index coefficients:\n")
      for (var in available_new) {
        if (var %in% names(coefs)) {
          cat(sprintf("    %s: β = %.3f (%.3f)\n", 
                      var, coefs[var], ses[var]))
        }
      }
    }
  }
  
  # ─────────────────────────────────────────────────────────────────────────
  # Model 4: Interaction - Specificity × Tender
  # ─────────────────────────────────────────────────────────────────────────
  if ("tender_dummy" %in% available_controls && "operational_specificity_norm" %in% available_core) {
    cat("\n[Model 4] Interaction: Specificity × Tender...\n")
    
    f4 <- as.formula(paste(
      "premium_pct_w ~",
      "operational_specificity_norm * tender_dummy +",
      paste(setdiff(available_core, "operational_specificity_norm"), collapse = " + "), "+",
      paste(setdiff(available_controls, "tender_dummy"), collapse = " + "),
      "|", CONFIG$fe_year
    ))
    
    models_premium$m4_interaction <- tryCatch({
      feols(f4, data = df_premium, vcov = ~sic2)
    }, error = function(e) {
      cat(sprintf("  Error: %s\n", e$message))
      NULL
    })
    
    if (!is.null(models_premium$m4_interaction)) {
      coefs <- coef(models_premium$m4_interaction)
      ses <- sqrt(diag(vcov(models_premium$m4_interaction)))
      
      # Find interaction term
      int_term <- names(coefs)[str_detect(names(coefs), ":")]
      
      cat(sprintf("  Main effect (specificity): β = %.3f (%.3f)\n",
                  coefs["operational_specificity_norm"],
                  ses["operational_specificity_norm"]))
      
      if (length(int_term) > 0) {
        cat(sprintf("  Interaction: β = %.3f (%.3f)\n",
                    coefs[int_term[1]], ses[int_term[1]]))
      }
    }
  }
  
  # ─────────────────────────────────────────────────────────────────────────
  # Model 5: Interaction - Specificity × Readability (if available)
  # ─────────────────────────────────────────────────────────────────────────
  if ("reading_difficulty_norm" %in% available_new && "operational_specificity_norm" %in% available_core) {
    cat("\n[Model 5] Interaction: Specificity × Readability...\n")
    
    df_for_m5 <- df_premium %>% filter(!is.na(reading_difficulty_norm))
    
    f5 <- as.formula(paste(
      "premium_pct_w ~",
      "operational_specificity_norm * reading_difficulty_norm +",
      paste(setdiff(available_core, "operational_specificity_norm"), collapse = " + "), "+",
      paste(available_controls, collapse = " + "),
      "|", CONFIG$fe_year
    ))
    
    models_premium$m5_read_interact <- tryCatch({
      feols(f5, data = df_for_m5, vcov = ~sic2)
    }, error = function(e) {
      cat(sprintf("  Error: %s\n", e$message))
      NULL
    })
    
    if (!is.null(models_premium$m5_read_interact)) {
      coefs <- coef(models_premium$m5_read_interact)
      ses <- sqrt(diag(vcov(models_premium$m5_read_interact)))
      
      int_term <- names(coefs)[str_detect(names(coefs), ":")]
      
      cat(sprintf("  Main (specificity): β = %.3f (%.3f)\n",
                  coefs["operational_specificity_norm"],
                  ses["operational_specificity_norm"]))
      cat(sprintf("  Main (readability): β = %.3f (%.3f)\n",
                  coefs["reading_difficulty_norm"],
                  ses["reading_difficulty_norm"]))
      
      if (length(int_term) > 0) {
        cat(sprintf("  Interaction: β = %.3f (%.3f)\n",
                    coefs[int_term[1]], ses[int_term[1]]))
        cat("  Interpretation: Does readability moderate the specificity effect?\n")
      }
    }
  }
  
  # ─────────────────────────────────────────────────────────────────────────
  # Model 6: Quadratic effects
  # ─────────────────────────────────────────────────────────────────────────
  cat("\n[Model 6] Quadratic: Specificity + Specificity²...\n")
  
  f6 <- as.formula(paste(
    "premium_pct_w ~",
    "operational_specificity_norm + I(operational_specificity_norm^2) +",
    paste(setdiff(available_core, "operational_specificity_norm"), collapse = " + "), "+",
    paste(available_controls, collapse = " + "),
    "|", CONFIG$fe_year
  ))
  
  models_premium$m6_quadratic <- tryCatch({
    feols(f6, data = df_premium, vcov = ~sic2)
  }, error = function(e) {
    cat(sprintf("  Error: %s\n", e$message))
    NULL
  })
  
  if (!is.null(models_premium$m6_quadratic)) {
    coefs <- coef(models_premium$m6_quadratic)
    ses <- sqrt(diag(vcov(models_premium$m6_quadratic)))
    
    lin_term <- "operational_specificity_norm"
    sq_term <- names(coefs)[str_detect(names(coefs), "\\^2")]
    
    cat(sprintf("  Linear: β = %.3f (%.3f)\n", coefs[lin_term], ses[lin_term]))
    if (length(sq_term) > 0) {
      cat(sprintf("  Quadratic: β = %.3f (%.3f)\n", coefs[sq_term[1]], ses[sq_term[1]]))
      
      # Test for U-shape or inverted-U
      if (coefs[sq_term[1]] > 0 && coefs[lin_term] < 0) {
        cat("  Shape: U-shaped (minimum at intermediate values)\n")
      } else if (coefs[sq_term[1]] < 0 && coefs[lin_term] > 0) {
        cat("  Shape: Inverted-U (maximum at intermediate values)\n")
      }
    }
  }
  
  # ─────────────────────────────────────────────────────────────────────────
  # Save premium results
  # ─────────────────────────────────────────────────────────────────────────
  cat("\nSaving premium results...\n")
  
  # Create results table
  valid_models <- models_premium[!sapply(models_premium, is.null)]
  
  if (length(valid_models) > 0) {
    results_df <- map_df(names(valid_models), function(m_name) {
      m <- valid_models[[m_name]]
      coefs <- coef(m)
      ses <- sqrt(diag(vcov(m)))
      
      tibble(
        model = m_name,
        variable = names(coefs),
        estimate = coefs,
        std_error = ses,
        t_stat = coefs / ses,
        p_value = 2 * pt(-abs(coefs / ses), df = nobs(m) - length(coefs)),
        n_obs = nobs(m),
        r_squared = r2(m)["r2"]
      )
    })
    
    write_csv(results_df, file.path(CONFIG$paths$output_tables, "enhanced_premium_results.csv"))
    cat(sprintf("  Saved: %s\n", file.path(CONFIG$paths$output_tables, "enhanced_premium_results.csv")))
  }
}

# ==============================================================================
# COMPLETION MODELS
# ==============================================================================

cat("\n")
cat(rep("=", 78), "\n", sep = "")
cat("COMPLETION MODELS (LOGIT)\n")
cat(rep("-", 78), "\n", sep = "")

if (!has_completion) {
  cat("Skipping: No completion variable available\n")
} else {
  
  df_completion <- df %>%
    filter(!is.na(completion_dummy)) %>%
    filter(if_all(all_of(available_core), ~!is.na(.)))
  
  cat(sprintf("Analysis sample: %d observations\n", nrow(df_completion)))
  cat(sprintf("  Completed: %d (%.1f%%)\n", 
              sum(df_completion$completion_dummy == 1),
              100 * mean(df_completion$completion_dummy == 1)))
  
  models_completion <- list()
  
  # Model 1: Core indices
  cat("\n[Model 1] Core indices...\n")
  
  f1 <- build_formula("completion_dummy", available_core, available_controls, CONFIG$fe_year)
  
  models_completion$m1_core <- tryCatch({
    feglm(f1, data = df_completion, family = binomial(link = "logit"), vcov = ~sic2)
  }, error = function(e) {
    cat(sprintf("  Error: %s\n", e$message))
    NULL
  })
  
  if (!is.null(models_completion$m1_core)) {
    cat(sprintf("  N = %d\n", nobs(models_completion$m1_core)))
    
    coefs <- coef(models_completion$m1_core)
    
    # Print NLP index coefficients (log-odds)
    cat("  NLP coefficients (log-odds):\n")
    for (var in available_core) {
      if (var %in% names(coefs)) {
        cat(sprintf("    %s: %.3f\n", var, coefs[var]))
      }
    }
  }
  
  # Model 2: With new indices
  if (length(available_new) > 0) {
    cat("\n[Model 2] Core + New indices...\n")
    
    all_nlp <- c(available_core, available_new)
    df_comp_new <- df_completion %>%
      filter(if_all(all_of(available_new), ~!is.na(.)))
    
    f2 <- build_formula("completion_dummy", all_nlp, available_controls, CONFIG$fe_year)
    
    models_completion$m2_extended <- tryCatch({
      feglm(f2, data = df_comp_new, family = binomial(link = "logit"), vcov = ~sic2)
    }, error = function(e) {
      cat(sprintf("  Error: %s\n", e$message))
      NULL
    })
    
    if (!is.null(models_completion$m2_extended)) {
      cat(sprintf("  N = %d\n", nobs(models_completion$m2_extended)))
    }
  }
  
  # Save completion results
  cat("\nSaving completion results...\n")
  
  valid_models <- models_completion[!sapply(models_completion, is.null)]
  
  if (length(valid_models) > 0) {
    results_df <- map_df(names(valid_models), function(m_name) {
      m <- valid_models[[m_name]]
      coefs <- coef(m)
      ses <- sqrt(diag(vcov(m)))
      
      tibble(
        model = m_name,
        variable = names(coefs),
        estimate = coefs,
        std_error = ses,
        z_stat = coefs / ses,
        p_value = 2 * pnorm(-abs(coefs / ses)),
        odds_ratio = exp(coefs),
        n_obs = nobs(m)
      )
    })
    
    write_csv(results_df, file.path(CONFIG$paths$output_tables, "enhanced_completion_results.csv"))
    cat(sprintf("  Saved: %s\n", file.path(CONFIG$paths$output_tables, "enhanced_completion_results.csv")))
  }
}

# ==============================================================================
# MODEL COMPARISON TABLE
# ==============================================================================

cat("\n")
cat(rep("=", 78), "\n", sep = "")
cat("MODEL COMPARISON\n")
cat(rep("-", 78), "\n", sep = "")

# Combine all premium models for comparison
if (exists("models_premium") && length(models_premium) > 0) {
  valid_premium <- models_premium[!sapply(models_premium, is.null)]
  
  if (length(valid_premium) >= 2) {
    comparison_df <- tibble(
      model = names(valid_premium),
      n_obs = sapply(valid_premium, nobs),
      r_squared = sapply(valid_premium, function(m) r2(m)["r2"]),
      adj_r_squared = sapply(valid_premium, function(m) r2(m)["adj.r2"]),
      n_vars = sapply(valid_premium, function(m) length(coef(m)))
    )
    
    cat("\nPremium Models Comparison:\n")
    print(comparison_df)
    
    write_csv(comparison_df, file.path(CONFIG$paths$output_tables, "model_comparison.csv"))
    cat(sprintf("\nSaved: %s\n", file.path(CONFIG$paths$output_tables, "model_comparison.csv")))
  }
}

# ==============================================================================
# GENERATE REPORT
# ==============================================================================

cat("\nGenerating report...\n")

report_lines <- c(
  "# Enhanced Econometric Models Report",
  "",
  sprintf("**Generated:** %s", Sys.time()),
  "",
  "## Data Summary",
  sprintf("- Total observations: %d", nrow(df)),
  sprintf("- Core indices available: %s", paste(available_core, collapse = ", ")),
  sprintf("- New indices available: %s", 
          ifelse(length(available_new) > 0, paste(available_new, collapse = ", "), "None")),
  "",
  "## Model Specifications",
  "",
  "### Premium Models",
  "1. **M1 (Baseline):** Core indices + Year FE",
  "2. **M2:** Core indices + Industry×Year FE", 
  "3. **M3 (Extended):** Core + New indices + Year FE",
  "4. **M4 (Interaction):** Specificity × Tender offer",
  "5. **M5 (Moderation):** Specificity × Readability",
  "6. **M6 (Nonlinear):** Specificity + Specificity²",
  "",
  "### Completion Models",
  "1. **M1:** Logit with core indices",
  "2. **M2:** Logit with core + new indices",
  "",
  "## Key Findings",
  ""
)

# Add findings if models exist
if (exists("models_premium") && !is.null(models_premium$m1_core)) {
  coefs <- coef(models_premium$m1_core)
  ses <- sqrt(diag(vcov(models_premium$m1_core)))
  
  report_lines <- c(report_lines,
    "### Operational Specificity Effect",
    sprintf("- Baseline coefficient: %.3f (SE: %.3f)", 
            coefs["operational_specificity_norm"],
            ses["operational_specificity_norm"]),
    sprintf("- Interpretation: 1 SD increase in specificity → %.1f pp change in premium",
            coefs["operational_specificity_norm"]),
    ""
  )
}

if (exists("models_premium") && !is.null(models_premium$m3_extended) && length(available_new) > 0) {
  report_lines <- c(report_lines,
    "### New Indices",
    "See enhanced_premium_results.csv for detailed coefficients.",
    ""
  )
}

report_lines <- c(report_lines,
  "## Output Files",
  sprintf("- `%s`", file.path(CONFIG$paths$output_tables, "enhanced_premium_results.csv")),
  sprintf("- `%s`", file.path(CONFIG$paths$output_tables, "enhanced_completion_results.csv")),
  sprintf("- `%s`", file.path(CONFIG$paths$output_tables, "model_comparison.csv")),
  ""
)

dir.create(dirname(CONFIG$paths$output_report), recursive = TRUE, showWarnings = FALSE)
writeLines(report_lines, CONFIG$paths$output_report)
cat(sprintf("  Saved: %s\n", CONFIG$paths$output_report))

# ==============================================================================
# SUMMARY
# ==============================================================================

cat("\n")
cat(rep("=", 78), "\n", sep = "")
cat("ENHANCED MODELS COMPLETE\n")
cat(rep("=", 78), "\n", sep = "")

cat("\nModels estimated:\n")
if (exists("models_premium")) {
  cat(sprintf("  - Premium models: %d\n", 
              sum(!sapply(models_premium, is.null))))
}
if (exists("models_completion")) {
  cat(sprintf("  - Completion models: %d\n",
              sum(!sapply(models_completion, is.null))))
}

cat("\nNew features tested:\n")
if (length(available_new) > 0) {
  for (idx in available_new) {
    cat(sprintf("  - %s\n", idx))
  }
} else {
  cat("  - None (run 07_nlp_enhancement.R first)\n")
}

cat(sprintf("\nTimestamp: %s\n", Sys.time()))
