# ==============================================================================
# 08_prepare_econometrics_final.R
#
# PURPOSE: Apply econometric best practices to analysis-ready datasets
#          - Winsorization of continuous variables
#          - Log transformations where appropriate
#          - Standardization of NLP indices
#          - Creation of interaction terms and fixed effects
#          - Final variable documentation
#
# INPUTS:  data/processed/econ_*.rds files (from 07_prepare_econometrics_dataset.R)
# OUTPUTS: 
#   - data/processed/econ_premium_final.rds
#   - data/processed/econ_completion_final.rds
#   - data/processed/econ_duration_final.rds
#   - output/tables/variable_summary.csv
#   - output/tables/correlation_matrix.csv
#
# DESIGN PRINCIPLES:
#   - All transformations explicit and traceable
#   - Original variables preserved alongside transformed versions
#   - Configurable winsorization levels
#   - No silent data modification
#
# REFERENCES:
#   - Officer (2003) JFE - Premium winsorization
#   - Betton et al. (2008) - M&A deal controls
#   - Loughran & McDonald (2011) - Textual analysis standardization
#
# ==============================================================================

library(tidyverse)
library(lubridate)
library(janitor)

# ==============================================================================
# CONFIGURATION
# ==============================================================================

CONFIG <- list(
  # File paths
  paths = list(
    input_premium     = "data/processed/econ_premium.rds",
    input_completion  = "data/processed/econ_completion.rds",
    input_duration    = "data/processed/econ_duration.rds",
    input_main        = "data/processed/econ_dataset.rds",
    output_premium    = "data/processed/econ_premium_final.rds",
    output_completion = "data/processed/econ_completion_final.rds",
    output_duration   = "data/processed/econ_duration_final.rds",
    output_summary    = "output/tables/variable_summary.csv",
    output_corr       = "output/tables/correlation_matrix.csv"
  ),
  

  # Winsorization settings
  # Standard in M&A literature: 1% and 99% tails
  winsor = list(
    lower_pct = 0.01,
    upper_pct = 0.99,
    # Variables to winsorize (pattern-based detection)
    patterns_to_winsor = c(
      "premium",          # Deal premium
      "deal_value",       # Transaction value
      "rank_value",       # Alternative deal value
      "time_to",          # Duration measures
      "days_to",          # Duration measures
      "word_count",       # Document length
      "toehold"           # Prior stake
    ),
    # Variables to NEVER winsorize (binary, bounded indices)
    patterns_never_winsor = c(
      "dummy",            # Binary indicators
      "_pct$",            # Already percentages
      "completed",        # Binary
      "event_",           # Event indicators
      "sample_",          # Sample flags
      "_defined$"         # Definition flags
    )
  ),
  
  # Log transformation settings
  # Apply to right-skewed positive variables
  log_transform = list(
    vars = c(
      "deal_value",       # Transaction value (highly skewed)
      "rank_value",       # Alternative deal value
      "word_count_mda",   # Document length
      "word_count_risk",  # Document length
      "time_to_close_days",    # Duration (optional)
      "time_to_event_days"     # Duration for Cox
    ),
    # Shift constant for log(x + c) when zeros present
    shift_constant = 1
  ),
  
  # NLP index standardization
  # Standardize to mean=0, sd=1 within sample for comparability
  standardize_indices = TRUE,
  index_patterns = c(
    "tone", "forward", "specificity", "uncertainty",
    "tfidf", "lm_", "sentiment", "readab", "fog",
    "operational", "disclosure"
  ),
  
  # Fixed effects construction
  fixed_effects = list(
    # Industry × Year FE
    industry_var = "target_sic_code",  # Primary industry variable
    year_var = "year_announced",       # Deal year
    # Alternative: 2-digit SIC
    use_sic2 = TRUE
  ),
  
  # Sample restrictions for robustness
  # NOTE: Premiums are in PERCENTAGE POINTS (e.g., 25 = 25%), not decimals
  sample_restrictions = list(
    # Premium bounds (standard in literature)
    premium_min = -50,     # Exclude < -50% premium
    premium_max = 300,     # Exclude > 300% premium
    # Minimum deal value (often $10M or $50M in literature)
    min_deal_value = 10    # In millions
  )
)

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

#' Winsorize a numeric vector at specified percentiles
#' 
#' @param x Numeric vector
#' @param lower Lower percentile (e.g., 0.01)
#' @param upper Upper percentile (e.g., 0.99)
#' @return Winsorized numeric vector
winsorize <- function(x, lower = 0.01, upper = 0.99) {
  if (!is.numeric(x)) return(x)
  if (all(is.na(x))) return(x)
  
  q_lower <- quantile(x, probs = lower, na.rm = TRUE)
  q_upper <- quantile(x, probs = upper, na.rm = TRUE)
  
  x_wins <- pmax(pmin(x, q_upper), q_lower)
  
  return(x_wins)
}

#' Standardize a numeric vector (z-score)
#' 
#' @param x Numeric vector
#' @return Standardized vector (mean=0, sd=1)
standardize <- function(x) {
  if (!is.numeric(x)) return(x)
  if (all(is.na(x))) return(x)
  
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)
  
  if (s == 0 || is.na(s)) {
    warning("Standard deviation is zero; returning centered values")
    return(x - m)
  }
  
  return((x - m) / s)
}

#' Detect columns matching patterns
#' 
#' @param col_names Vector of column names
#' @param patterns Vector of regex patterns
#' @return Vector of matching column names
find_matching_cols <- function(col_names, patterns) {
  pattern_combined <- paste0("(", paste(patterns, collapse = "|"), ")")
  matches <- col_names[str_detect(col_names, regex(pattern_combined, ignore_case = TRUE))]
  return(matches)
}

#' Create 2-digit SIC code from full SIC
#' 
#' @param sic_code SIC code (character or numeric)
#' @return 2-digit SIC code as character
extract_sic2 <- function(sic_code) {
  sic_str <- as.character(sic_code)
  # Extract first 2 digits
  sic2 <- str_sub(str_pad(sic_str, width = 4, side = "left", pad = "0"), 1, 2)
  return(sic2)
}

#' Compute summary statistics for a numeric variable
#' 
#' @param x Numeric vector
#' @param var_name Variable name for reporting
#' @return Tibble with summary statistics
compute_var_summary <- function(x, var_name) {
  tibble(
    variable = var_name,
    n = sum(!is.na(x)),
    n_missing = sum(is.na(x)),
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    p01 = quantile(x, 0.01, na.rm = TRUE),
    p05 = quantile(x, 0.05, na.rm = TRUE),
    p25 = quantile(x, 0.25, na.rm = TRUE),
    p50 = median(x, na.rm = TRUE),
    p75 = quantile(x, 0.75, na.rm = TRUE),
    p95 = quantile(x, 0.95, na.rm = TRUE),
    p99 = quantile(x, 0.99, na.rm = TRUE),
    max = max(x, na.rm = TRUE)
  )
}

#' Print formatted section header
#' 
#' @param title Section title
print_section <- function(title) {
  cat("\n")
  cat(rep("=", 78), "\n", sep = "")
  cat(title, "\n")
  cat(rep("-", 78), "\n", sep = "")
}

# ==============================================================================
# MAIN PROCESSING FUNCTION
# ==============================================================================

#' Process dataset with econometric best practices
#' 
#' @param df Input dataframe
#' @param dataset_name Name for reporting (e.g., "premium", "completion")
#' @param config Configuration list
#' @return Processed dataframe
process_econometric_dataset <- function(df, dataset_name, config) {
  
  print_section(sprintf("PROCESSING: %s DATASET", toupper(dataset_name)))
  cat(sprintf("Input dimensions: %d rows × %d columns\n", nrow(df), ncol(df)))
  
  df_processed <- df
  n_original <- nrow(df)
  
  # ============================================================================
  # STEP 1: IDENTIFY VARIABLE TYPES
  # ============================================================================
  
  cat("\n[1] Identifying variable types...\n")
  
  all_cols <- names(df_processed)
  numeric_cols <- all_cols[sapply(df_processed, is.numeric)]
  
  # Columns to winsorize
  winsor_cols <- find_matching_cols(numeric_cols, config$winsor$patterns_to_winsor)
  never_winsor <- find_matching_cols(winsor_cols, config$winsor$patterns_never_winsor)
  winsor_cols <- setdiff(winsor_cols, never_winsor)
  
  cat(sprintf("   Numeric columns: %d\n", length(numeric_cols)))
  cat(sprintf("   Columns to winsorize: %d\n", length(winsor_cols)))
  if (length(winsor_cols) > 0) {
    cat(sprintf("     %s\n", paste(head(winsor_cols, 10), collapse = ", ")))
  }
  
  # NLP index columns
  index_cols <- find_matching_cols(numeric_cols, config$index_patterns)
  index_cols <- setdiff(index_cols, find_matching_cols(index_cols, c("word_count", "_count$")))
  
  cat(sprintf("   NLP index columns: %d\n", length(index_cols)))
  if (length(index_cols) > 0) {
    cat(sprintf("     %s\n", paste(head(index_cols, 10), collapse = ", ")))
  }
  
  # ============================================================================
  # STEP 2: WINSORIZATION
  # ============================================================================
  
  cat("\n[2] Winsorizing continuous variables...\n")
  cat(sprintf("   Lower bound: %.1f%%, Upper bound: %.1f%%\n", 
              100 * config$winsor$lower_pct, 100 * config$winsor$upper_pct))
  
  winsor_report <- tibble(
    variable = character(),
    n_lower = integer(),
    n_upper = integer(),
    lower_threshold = numeric(),
    upper_threshold = numeric()
  )
  
  for (col in winsor_cols) {
    x_orig <- df_processed[[col]]
    
    if (all(is.na(x_orig))) next
    
    # Compute thresholds
    q_lower <- quantile(x_orig, config$winsor$lower_pct, na.rm = TRUE)
    q_upper <- quantile(x_orig, config$winsor$upper_pct, na.rm = TRUE)
    
    # Count affected observations
    n_lower <- sum(x_orig < q_lower, na.rm = TRUE)
    n_upper <- sum(x_orig > q_upper, na.rm = TRUE)
    
    # Create winsorized version (preserve original with _raw suffix)
    col_raw <- paste0(col, "_raw")
    col_wins <- paste0(col, "_w")
    
    df_processed[[col_raw]] <- x_orig
    df_processed[[col_wins]] <- winsorize(x_orig, config$winsor$lower_pct, config$winsor$upper_pct)
    
    # Report
    if (n_lower > 0 || n_upper > 0) {
      winsor_report <- bind_rows(winsor_report, tibble(
        variable = col,
        n_lower = n_lower,
        n_upper = n_upper,
        lower_threshold = q_lower,
        upper_threshold = q_upper
      ))
    }
  }
  
  if (nrow(winsor_report) > 0) {
    cat("   Winsorization applied:\n")
    for (i in 1:nrow(winsor_report)) {
      cat(sprintf("     %s: %d obs below %.4f, %d obs above %.4f\n",
                  winsor_report$variable[i],
                  winsor_report$n_lower[i], winsor_report$lower_threshold[i],
                  winsor_report$n_upper[i], winsor_report$upper_threshold[i]))
    }
  } else {
    cat("   No observations required winsorization.\n")
  }
  
  # ============================================================================
  # STEP 3: LOG TRANSFORMATIONS
  # ============================================================================
  
  cat("\n[3] Applying log transformations...\n")
  
  log_vars <- intersect(config$log_transform$vars, names(df_processed))
  log_shift <- config$log_transform$shift_constant
  
  for (col in log_vars) {
    x <- df_processed[[col]]
    
    if (!is.numeric(x)) next
    if (all(is.na(x))) next
    
    # Check for negative values
    n_neg <- sum(x < 0, na.rm = TRUE)
    n_zero <- sum(x == 0, na.rm = TRUE)
    
    if (n_neg > 0) {
      cat(sprintf("   ⚠ %s: %d negative values - skipping log transform\n", col, n_neg))
      next
    }
    
    # Apply log(x + c) transformation
    col_log <- paste0("ln_", col)
    df_processed[[col_log]] <- log(x + log_shift)
    
    cat(sprintf("   %s → %s (shift=%.0f, zeros=%d)\n", col, col_log, log_shift, n_zero))
  }
  
  # ============================================================================
  # STEP 4: STANDARDIZE NLP INDICES
  # ============================================================================
  
  if (config$standardize_indices && length(index_cols) > 0) {
    cat("\n[4] Standardizing NLP indices...\n")
    
    for (col in index_cols) {
      x <- df_processed[[col]]
      
      if (!is.numeric(x)) next
      if (all(is.na(x))) next
      
      # Create standardized version
      col_std <- paste0(col, "_z")
      df_processed[[col_std]] <- standardize(x)
      
      cat(sprintf("   %s → %s (mean=%.4f, sd=%.4f)\n", 
                  col, col_std, mean(x, na.rm = TRUE), sd(x, na.rm = TRUE)))
    }
  }
  
  # ============================================================================
  # STEP 5: CONSTRUCT FIXED EFFECTS
  # ============================================================================
  
  cat("\n[5] Constructing fixed effects...\n")
  
  # Extract year from announcement date if not present
  if (!"year_announced" %in% names(df_processed)) {
    if ("date_announced" %in% names(df_processed)) {
      df_processed <- df_processed %>%
        mutate(year_announced = year(as.Date(date_announced)))
      cat("   Created year_announced from date_announced\n")
    } else if ("announce_date_parsed" %in% names(df_processed)) {
      df_processed <- df_processed %>%
        mutate(year_announced = year(announce_date_parsed))
      cat("   Created year_announced from announce_date_parsed\n")
    }
  }
  
  # Create 2-digit SIC code
  sic_col <- config$fixed_effects$industry_var
  if (sic_col %in% names(df_processed) && config$fixed_effects$use_sic2) {
    df_processed <- df_processed %>%
      mutate(sic2 = extract_sic2(.data[[sic_col]]))
    
    n_sic2 <- n_distinct(df_processed$sic2, na.rm = TRUE)
    cat(sprintf("   Created sic2 from %s: %d unique industries\n", sic_col, n_sic2))
  }
  
  # Create Industry × Year FE identifier
  if ("sic2" %in% names(df_processed) && "year_announced" %in% names(df_processed)) {
    df_processed <- df_processed %>%
      mutate(fe_industry_year = paste0(sic2, "_", year_announced))
    
    n_fe <- n_distinct(df_processed$fe_industry_year, na.rm = TRUE)
    cat(sprintf("   Created fe_industry_year: %d unique cells\n", n_fe))
  }
  
  # Create year FE as factor
  if ("year_announced" %in% names(df_processed)) {
    df_processed <- df_processed %>%
      mutate(fe_year = factor(year_announced))
    
    year_range <- range(df_processed$year_announced, na.rm = TRUE)
    cat(sprintf("   Created fe_year: %d to %d\n", year_range[1], year_range[2]))
  }
  
  # ============================================================================
  # STEP 6: CREATE ADDITIONAL CONTROLS
  # ============================================================================
  
  cat("\n[6] Creating additional control variables...\n")
  
  # Cash vs stock payment indicator
  payment_cols <- names(df_processed)[str_detect(names(df_processed), regex("cash|stock|payment", ignore_case = TRUE))]
  if (length(payment_cols) > 0) {
    cat(sprintf("   Payment method variables detected: %s\n", paste(payment_cols, collapse = ", ")))
  }
  
  # Hostile indicator
  if ("attitude" %in% names(df_processed)) {
    df_processed <- df_processed %>%
      mutate(hostile_dummy = as.integer(str_detect(str_to_lower(attitude), "hostil")))
    n_hostile <- sum(df_processed$hostile_dummy == 1, na.rm = TRUE)
    cat(sprintf("   Created hostile_dummy: %d hostile deals\n", n_hostile))
  }
  
  # Tender offer indicator
  if ("tender_offer" %in% names(df_processed)) {
    df_processed <- df_processed %>%
      mutate(tender_dummy = as.integer(!is.na(tender_offer) & tender_offer != ""))
  } else if ("acquisition_techniques" %in% names(df_processed)) {
    df_processed <- df_processed %>%
      mutate(tender_dummy = as.integer(str_detect(str_to_lower(acquisition_techniques), "tender")))
  }
  if ("tender_dummy" %in% names(df_processed)) {
    n_tender <- sum(df_processed$tender_dummy == 1, na.rm = TRUE)
    cat(sprintf("   Created tender_dummy: %d tender offers\n", n_tender))
  }
  
  # Cross-border indicator
  if ("cross_border" %in% names(df_processed)) {
    df_processed <- df_processed %>%
      mutate(cross_border_dummy = as.integer(!is.na(cross_border) & 
                                              str_detect(str_to_lower(cross_border), "yes|cross")))
    n_cross <- sum(df_processed$cross_border_dummy == 1, na.rm = TRUE)
    cat(sprintf("   Created cross_border_dummy: %d cross-border deals\n", n_cross))
  }
  
  # ============================================================================
  # STEP 7: APPLY SAMPLE RESTRICTIONS (FLAG ONLY)
  # ============================================================================
  
  cat("\n[7] Flagging observations outside sample bounds...\n")
  
  # Premium bounds
  if ("premium_pct" %in% names(df_processed)) {
    df_processed <- df_processed %>%
      mutate(
        premium_outlier = case_when(
          premium_pct < config$sample_restrictions$premium_min ~ "low",
          premium_pct > config$sample_restrictions$premium_max ~ "high",
          TRUE ~ "ok"
        )
      )
    
    outlier_summary <- df_processed %>%
      filter(!is.na(premium_pct)) %>%
      count(premium_outlier)
    
    cat("   Premium bounds check:\n")
    for (i in 1:nrow(outlier_summary)) {
      cat(sprintf("     %s: %d\n", outlier_summary$premium_outlier[i], outlier_summary$n[i]))
    }
  }
  
  # Deal value minimum
  deal_val_col <- if ("deal_value" %in% names(df_processed)) "deal_value" else 
                  if ("rank_value" %in% names(df_processed)) "rank_value" else NA_character_
  
  if (!is.na(deal_val_col)) {
    df_processed <- df_processed %>%
      mutate(
        small_deal_flag = as.integer(.data[[deal_val_col]] < config$sample_restrictions$min_deal_value)
      )
    
    n_small <- sum(df_processed$small_deal_flag == 1, na.rm = TRUE)
    cat(sprintf("   Deals below $%dM threshold: %d\n", config$sample_restrictions$min_deal_value, n_small))
  }
  
  # ============================================================================
  # STEP 8: FINAL CLEANUP
  # ============================================================================
  
  cat("\n[8] Final cleanup...\n")
  
  # Remove fully empty columns (if any)
  all_na_cols <- names(df_processed)[sapply(df_processed, function(x) all(is.na(x)))]
  if (length(all_na_cols) > 0) {
    df_processed <- df_processed %>% select(-all_of(all_na_cols))
    cat(sprintf("   Removed %d all-NA columns\n", length(all_na_cols)))
  }
  
  # Sort columns logically
  # Order: ID, dates, outcomes, NLP indices, controls, FE, flags
  
  cat(sprintf("\nOutput dimensions: %d rows × %d columns\n", nrow(df_processed), ncol(df_processed)))
  
  return(df_processed)
}

# ==============================================================================
# MAIN EXECUTION
# ==============================================================================

main <- function() {
  
  cat("\n")
  cat(rep("=", 78), "\n", sep = "")
  cat("ECONOMETRIC DATASET PREPARATION - FINAL STEP\n")
  cat(rep("=", 78), "\n", sep = "")
  cat(sprintf("Timestamp: %s\n", Sys.time()))
  cat("\n")
  
  # ============================================================================
  # Load datasets
  # ============================================================================
  
  cat("Loading input datasets...\n")
  
  if (!file.exists(CONFIG$paths$input_premium)) {
    stop(sprintf("Premium dataset not found: %s", CONFIG$paths$input_premium))
  }
  
  econ_premium_raw <- readRDS(CONFIG$paths$input_premium)
  econ_completion_raw <- readRDS(CONFIG$paths$input_completion)
  econ_duration_raw <- readRDS(CONFIG$paths$input_duration)
  
  cat(sprintf("  Premium dataset: %d × %d\n", nrow(econ_premium_raw), ncol(econ_premium_raw)))
  cat(sprintf("  Completion dataset: %d × %d\n", nrow(econ_completion_raw), ncol(econ_completion_raw)))
  cat(sprintf("  Duration dataset: %d × %d\n", nrow(econ_duration_raw), ncol(econ_duration_raw)))
  
  # ============================================================================
  # Process each dataset
  # ============================================================================
  
  econ_premium_final <- process_econometric_dataset(econ_premium_raw, "premium", CONFIG)
  econ_completion_final <- process_econometric_dataset(econ_completion_raw, "completion", CONFIG)
  econ_duration_final <- process_econometric_dataset(econ_duration_raw, "duration", CONFIG)
  
  # ============================================================================
  # Generate summary statistics
  # ============================================================================
  
  print_section("GENERATING SUMMARY STATISTICS")
  
  # Key variables for summary
  key_vars <- c(
    # Outcomes
    "premium_pct", "premium_pct_w", "completion_dummy", "time_to_close_days",
    # NLP indices (detect dynamically)
    find_matching_cols(names(econ_premium_final), CONFIG$index_patterns),
    # Controls
    "deal_value", "ln_deal_value", "hostile_dummy", "tender_dummy", "cross_border_dummy"
  )
  key_vars <- intersect(key_vars, names(econ_premium_final))
  
  var_summaries <- map_df(key_vars, function(v) {
    if (is.numeric(econ_premium_final[[v]])) {
      compute_var_summary(econ_premium_final[[v]], v)
    } else {
      NULL
    }
  })
  
  cat(sprintf("Computed summary statistics for %d variables\n", nrow(var_summaries)))
  
  # ============================================================================
  # Generate correlation matrix for key variables
  # ============================================================================
  
  print_section("GENERATING CORRELATION MATRIX")
  
  # Select numeric NLP indices and outcomes for correlation
  corr_vars <- c(
    "premium_pct_w",
    find_matching_cols(names(econ_premium_final), c("_z$"))  # Standardized indices
  )
  corr_vars <- intersect(corr_vars, names(econ_premium_final))
  corr_vars <- corr_vars[sapply(econ_premium_final[corr_vars], is.numeric)]
  
  if (length(corr_vars) >= 2) {
    corr_data <- econ_premium_final %>%
      select(all_of(corr_vars)) %>%
      filter(complete.cases(.))
    
    if (nrow(corr_data) > 10) {
      corr_matrix <- cor(corr_data, use = "pairwise.complete.obs")
      corr_df <- as.data.frame(corr_matrix) %>%
        rownames_to_column("variable")
      
      cat(sprintf("Computed correlation matrix: %d × %d\n", nrow(corr_matrix), ncol(corr_matrix)))
    } else {
      corr_df <- NULL
      cat("Insufficient observations for correlation matrix\n")
    }
  } else {
    corr_df <- NULL
    cat("Not enough numeric variables for correlation matrix\n")
  }
  
  # ============================================================================
  # Save outputs
  # ============================================================================
  
  print_section("SAVING OUTPUTS")
  
  # Ensure output directories exist
  dir.create(dirname(CONFIG$paths$output_premium), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(CONFIG$paths$output_summary), recursive = TRUE, showWarnings = FALSE)
  
  # Save processed datasets
  saveRDS(econ_premium_final, CONFIG$paths$output_premium)
  cat(sprintf("  Saved: %s (%d × %d)\n", CONFIG$paths$output_premium, 
              nrow(econ_premium_final), ncol(econ_premium_final)))
  
  saveRDS(econ_completion_final, CONFIG$paths$output_completion)
  cat(sprintf("  Saved: %s (%d × %d)\n", CONFIG$paths$output_completion,
              nrow(econ_completion_final), ncol(econ_completion_final)))
  
  saveRDS(econ_duration_final, CONFIG$paths$output_duration)
  cat(sprintf("  Saved: %s (%d × %d)\n", CONFIG$paths$output_duration,
              nrow(econ_duration_final), ncol(econ_duration_final)))
  
  # Save summary statistics
  write_csv(var_summaries, CONFIG$paths$output_summary)
  cat(sprintf("  Saved: %s\n", CONFIG$paths$output_summary))
  
  # Save correlation matrix
  if (!is.null(corr_df)) {
    write_csv(corr_df, CONFIG$paths$output_corr)
    cat(sprintf("  Saved: %s\n", CONFIG$paths$output_corr))
  }
  
  # ============================================================================
  # Create combined output object
  # ============================================================================
  
  econ_analysis_final <- list(
    premium = econ_premium_final,
    completion = econ_completion_final,
    duration = econ_duration_final,
    config = CONFIG,
    variable_summary = var_summaries,
    correlation_matrix = corr_df,
    processing_timestamp = Sys.time()
  )
  
  saveRDS(econ_analysis_final, "data/processed/econ_analysis_final.rds")
  cat(sprintf("  Saved: data/processed/econ_analysis_final.rds\n"))
  
  # ============================================================================
  # Final report
  # ============================================================================
  
  print_section("PROCESSING COMPLETE")
  
  cat("\nFinal dataset dimensions:\n")
  cat(sprintf("  Premium analysis:    %d rows × %d columns\n", 
              nrow(econ_premium_final), ncol(econ_premium_final)))
  cat(sprintf("  Completion analysis: %d rows × %d columns\n", 
              nrow(econ_completion_final), ncol(econ_completion_final)))
  cat(sprintf("  Duration analysis:   %d rows × %d columns\n", 
              nrow(econ_duration_final), ncol(econ_duration_final)))
  
  cat("\nKey transformations applied:\n")
  cat("  ✓ Winsorization at 1%/99% for continuous variables\n")
  cat("  ✓ Log transformation for skewed variables (deal value, word counts)\n")
  cat("  ✓ Z-score standardization for NLP indices\n")
  cat("  ✓ Fixed effects: SIC2 × Year\n")
  cat("  ✓ Additional controls: hostile, tender, cross-border dummies\n")
  cat("  ✓ Sample restriction flags: premium bounds, minimum deal value\n")
  
  cat("\nOutput files:\n")
  cat(sprintf("  - %s\n", CONFIG$paths$output_premium))
  cat(sprintf("  - %s\n", CONFIG$paths$output_completion))
  cat(sprintf("  - %s\n", CONFIG$paths$output_duration))
  cat(sprintf("  - %s\n", CONFIG$paths$output_summary))
  cat(sprintf("  - %s\n", CONFIG$paths$output_corr))
  cat("  - data/processed/econ_analysis_final.rds\n")
  
  cat(sprintf("\nTimestamp: %s\n", Sys.time()))
  cat("\n")
  
  return(invisible(econ_analysis_final))
}

# ==============================================================================
# RUN MAIN FUNCTION
# ==============================================================================
cat("\n*** Starting econometric preparation ***\n")
main()
