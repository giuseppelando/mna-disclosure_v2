# ==============================================================================
# src/50_models/econometric_utils.R
#
# PURPOSE: Utility functions for econometric data preparation and analysis
#          - Variable transformations (winsorization, log, standardization)
#          - Fixed effects construction
#          - Diagnostic functions
#          - Summary statistics
#
# USAGE:
#   source("src/50_models/econometric_utils.R")
#   df <- df %>% mutate(x_w = winsorize(x))
#
# ==============================================================================

# ==============================================================================
# VARIABLE TRANSFORMATIONS
# ==============================================================================

#' Winsorize a numeric vector at specified percentiles
#'
#' Replaces values below lower percentile with the lower threshold,
#' and values above upper percentile with the upper threshold.
#' Standard practice in M&A literature: 1% and 99%.
#'
#' @param x Numeric vector
#' @param lower Lower percentile (default 0.01 = 1%)
#' @param upper Upper percentile (default 0.99 = 99%)
#' @return Winsorized numeric vector
#' @examples
#' x <- c(-100, 1:100, 500)
#' winsorize(x)  # Replaces -100 and 500 with percentile values
#' @export
winsorize <- function(x, lower = 0.01, upper = 0.99) {
  if (!is.numeric(x)) {
    warning("winsorize() received non-numeric input; returning unchanged")
    return(x)
  }
  
  if (all(is.na(x))) return(x)
  
  q_lower <- quantile(x, probs = lower, na.rm = TRUE)
  q_upper <- quantile(x, probs = upper, na.rm = TRUE)
  
  x_winsorized <- pmax(pmin(x, q_upper), q_lower)
  
  # Preserve NA positions
  x_winsorized[is.na(x)] <- NA
  
  return(x_winsorized)
}

#' Standardize a numeric vector to z-scores (mean=0, sd=1)
#'
#' Standard practice for comparing coefficients across different scales.
#' Particularly useful for NLP indices with different ranges.
#'
#' @param x Numeric vector
#' @return Standardized vector with mean=0, sd=1
#' @examples
#' x <- rnorm(100, mean = 50, sd = 10)
#' standardize(x)  # Returns z-scores
#' @export
standardize <- function(x) {
  if (!is.numeric(x)) {
    warning("standardize() received non-numeric input; returning unchanged")
    return(x)
  }
  
  if (all(is.na(x))) return(x)
  
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)
  
  if (s == 0 || is.na(s)) {
    warning("Standard deviation is zero or NA; returning centered values")
    return(x - m)
  }
  
  z <- (x - m) / s
  
  return(z)
}

#' Apply log transformation with handling for zeros and negatives
#'
#' Computes log(x + c) where c is a shift constant.
#' Returns NA for negative values after warning.
#'
#' @param x Numeric vector
#' @param shift Constant to add before log (default 1)
#' @param base Logarithm base (default natural log)
#' @return Log-transformed vector
#' @examples
#' x <- c(0, 1, 10, 100)
#' log_transform(x)  # Returns log(x + 1)
#' @export
log_transform <- function(x, shift = 1, base = exp(1)) {
  if (!is.numeric(x)) {
    warning("log_transform() received non-numeric input; returning unchanged")
    return(x)
  }
  
  if (all(is.na(x))) return(x)
  
  x_shifted <- x + shift
  
  # Check for negative values (after shift)
  n_negative <- sum(x_shifted <= 0, na.rm = TRUE)
  if (n_negative > 0) {
    warning(sprintf("log_transform(): %d values are <= 0 after shift; returning NA for these", n_negative))
    x_shifted[x_shifted <= 0] <- NA
  }
  
  x_log <- log(x_shifted, base = base)
  
  return(x_log)
}

#' Inverse hyperbolic sine transformation
#'
#' Alternative to log for variables that can be zero or negative.
#' asinh(x) ≈ log(2x) for large x, but handles x <= 0.
#'
#' @param x Numeric vector
#' @return IHS-transformed vector
#' @examples
#' x <- c(-10, 0, 10, 100)
#' ihs_transform(x)  # Handles all values
#' @export
ihs_transform <- function(x) {
  if (!is.numeric(x)) {
    warning("ihs_transform() received non-numeric input; returning unchanged")
    return(x)
  }
  
  return(asinh(x))
}

# ==============================================================================
# FIXED EFFECTS CONSTRUCTION
# ==============================================================================

#' Extract 2-digit SIC code from full SIC code
#'
#' SEC standard uses 4-digit SIC codes. This extracts first 2 digits
#' for industry-level fixed effects.
#'
#' @param sic_code SIC code (character or numeric)
#' @return 2-digit SIC code as character
#' @examples
#' extract_sic2(3711)  # "37" (Motor Vehicles)
#' extract_sic2("0100")  # "01" (Agricultural Crops)
#' @export
extract_sic2 <- function(sic_code) {
  sic_str <- as.character(sic_code)
  
  # Pad to 4 digits if needed
  sic_padded <- stringr::str_pad(sic_str, width = 4, side = "left", pad = "0")
  
  # Extract first 2 digits
  sic2 <- stringr::str_sub(sic_padded, 1, 2)
  
  # Handle NAs
  sic2[is.na(sic_code)] <- NA_character_
  
  return(sic2)
}

#' Create industry-year fixed effect identifier
#'
#' Combines industry (SIC2) and year into a single FE group.
#'
#' @param sic2 2-digit SIC code
#' @param year Year (numeric or character)
#' @return Character identifier like "37_2020"
#' @export
create_fe_industry_year <- function(sic2, year) {
  fe_id <- paste0(sic2, "_", year)
  
  # Handle NAs
  fe_id[is.na(sic2) | is.na(year)] <- NA_character_
  
  return(fe_id)
}

# ==============================================================================
# OUTLIER DETECTION AND FLAGGING
# ==============================================================================

#' Flag outliers based on percentile bounds
#'
#' Creates categorical flag: "low", "ok", "high"
#'
#' @param x Numeric vector
#' @param lower_pct Lower percentile threshold
#' @param upper_pct Upper percentile threshold
#' @return Character vector with outlier flags
#' @export
flag_outliers <- function(x, lower_pct = 0.01, upper_pct = 0.99) {
  if (!is.numeric(x)) return(rep(NA_character_, length(x)))
  
  q_lower <- quantile(x, lower_pct, na.rm = TRUE)
  q_upper <- quantile(x, upper_pct, na.rm = TRUE)
  
  flag <- case_when(
    is.na(x) ~ NA_character_,
    x < q_lower ~ "low",
    x > q_upper ~ "high",
    TRUE ~ "ok"
  )
  
  return(flag)
}

#' Flag observations outside absolute bounds
#'
#' For economic significance checks (e.g., premium bounds).
#'
#' @param x Numeric vector
#' @param lower Absolute lower bound
#' @param upper Absolute upper bound
#' @return Character vector with flags
#' @export
flag_bounds <- function(x, lower, upper) {
  if (!is.numeric(x)) return(rep(NA_character_, length(x)))
  
  flag <- case_when(
    is.na(x) ~ NA_character_,
    x < lower ~ "below_bound",
    x > upper ~ "above_bound",
    TRUE ~ "within_bounds"
  )
  
  return(flag)
}

# ==============================================================================
# SUMMARY STATISTICS
# ==============================================================================

#' Compute comprehensive summary statistics for a numeric variable
#'
#' Returns a tibble with N, missing, mean, sd, and percentiles.
#'
#' @param x Numeric vector
#' @param var_name Name for the variable column
#' @return Tibble with summary statistics
#' @export
summarize_numeric <- function(x, var_name = "variable") {
  if (!is.numeric(x)) {
    return(tibble(variable = var_name, note = "non-numeric"))
  }
  
  tibble(
    variable = var_name,
    n = sum(!is.na(x)),
    n_missing = sum(is.na(x)),
    pct_missing = round(100 * mean(is.na(x)), 2),
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
    max = max(x, na.rm = TRUE),
    skewness = e1071::skewness(x, na.rm = TRUE),
    kurtosis = e1071::kurtosis(x, na.rm = TRUE)
  )
}

#' Generate summary statistics table for multiple variables
#'
#' @param df Dataframe
#' @param vars Character vector of variable names (NULL = all numeric)
#' @return Tibble with summary statistics for each variable
#' @export
summary_table <- function(df, vars = NULL) {
  if (is.null(vars)) {
    vars <- names(df)[sapply(df, is.numeric)]
  }
  
  # Filter to existing columns
  vars <- intersect(vars, names(df))
  
  if (length(vars) == 0) {
    return(tibble(note = "No numeric variables found"))
  }
  
  purrr::map_df(vars, function(v) {
    summarize_numeric(df[[v]], v)
  })
}

# ==============================================================================
# CORRELATION ANALYSIS
# ==============================================================================

#' Compute pairwise correlation matrix with significance
#'
#' @param df Dataframe
#' @param vars Variables to include (NULL = all numeric)
#' @param method Correlation method ("pearson", "spearman", "kendall")
#' @return List with correlation matrix and p-value matrix
#' @export
correlation_matrix <- function(df, vars = NULL, method = "pearson") {
  if (is.null(vars)) {
    vars <- names(df)[sapply(df, is.numeric)]
  }
  
  vars <- intersect(vars, names(df))
  
  if (length(vars) < 2) {
    return(list(cor = NULL, pval = NULL, note = "Need at least 2 variables"))
  }
  
  df_subset <- df[, vars, drop = FALSE]
  
  # Correlation matrix
  cor_mat <- cor(df_subset, use = "pairwise.complete.obs", method = method)
  
  # P-value matrix (using Hmisc if available)
  n_vars <- length(vars)
  pval_mat <- matrix(NA, nrow = n_vars, ncol = n_vars,
                     dimnames = list(vars, vars))
  
  for (i in 1:(n_vars - 1)) {
    for (j in (i + 1):n_vars) {
      test <- cor.test(df_subset[[vars[i]]], df_subset[[vars[j]]], method = method)
      pval_mat[i, j] <- test$p.value
      pval_mat[j, i] <- test$p.value
    }
  }
  diag(pval_mat) <- 0
  
  return(list(
    cor = cor_mat,
    pval = pval_mat,
    n = nrow(df_subset),
    method = method
  ))
}

#' Format correlation matrix for display
#'
#' @param cor_result Output from correlation_matrix()
#' @param stars Add significance stars
#' @param digits Decimal places
#' @return Character matrix for display
#' @export
format_correlation <- function(cor_result, stars = TRUE, digits = 3) {
  cor_mat <- cor_result$cor
  pval_mat <- cor_result$pval
  
  if (is.null(cor_mat)) return(NULL)
  
  formatted <- matrix("", nrow = nrow(cor_mat), ncol = ncol(cor_mat),
                      dimnames = dimnames(cor_mat))
  
  for (i in 1:nrow(cor_mat)) {
    for (j in 1:ncol(cor_mat)) {
      val <- round(cor_mat[i, j], digits)
      
      if (stars && i != j) {
        p <- pval_mat[i, j]
        star <- if (p < 0.01) "***" else if (p < 0.05) "**" else if (p < 0.10) "*" else ""
        formatted[i, j] <- paste0(sprintf("%.*f", digits, val), star)
      } else {
        formatted[i, j] <- sprintf("%.*f", digits, val)
      }
    }
  }
  
  return(formatted)
}

# ==============================================================================
# MULTICOLLINEARITY DIAGNOSTICS
# ==============================================================================
#' Calculate Variance Inflation Factors
#'
#' @param model lm model object
#' @return Named vector of VIF values
#' @export
calculate_vif <- function(model) {
  if (!inherits(model, "lm")) {
    stop("Input must be an lm object")
  }
  
  # Use car::vif if available, otherwise manual calculation
  if (requireNamespace("car", quietly = TRUE)) {
    vif_vals <- car::vif(model)
    return(vif_vals)
  }
  
  # Manual VIF calculation
  X <- model.matrix(model)
  # Remove intercept if present
  if (colnames(X)[1] == "(Intercept)") {
    X <- X[, -1, drop = FALSE]
  }
  
  n_vars <- ncol(X)
  vif_vals <- numeric(n_vars)
  names(vif_vals) <- colnames(X)
  
  for (i in 1:n_vars) {
    y <- X[, i]
    X_others <- X[, -i, drop = FALSE]
    r_squared <- summary(lm(y ~ X_others))$r.squared
    vif_vals[i] <- 1 / (1 - r_squared)
  }
  
  return(vif_vals)
}

# ==============================================================================
# SAMPLE BALANCE CHECKS
# ==============================================================================

#' Check covariate balance between groups
#'
#' Useful for comparing treatment/control or subsamples.
#'
#' @param df Dataframe
#' @param group_var Name of grouping variable
#' @param covariates Names of covariates to check
#' @return Tibble with balance statistics
#' @export
check_balance <- function(df, group_var, covariates) {
  # Filter to complete cases for group variable
  df <- df[!is.na(df[[group_var]]), ]
  
  groups <- unique(df[[group_var]])
  if (length(groups) != 2) {
    warning("Balance check designed for 2 groups; using first two unique values")
    groups <- groups[1:2]
  }
  
  balance_stats <- purrr::map_df(covariates, function(cov) {
    if (!cov %in% names(df)) return(NULL)
    if (!is.numeric(df[[cov]])) return(NULL)
    
    g1 <- df[[cov]][df[[group_var]] == groups[1]]
    g2 <- df[[cov]][df[[group_var]] == groups[2]]
    
    # Standardized difference
    pooled_sd <- sqrt((var(g1, na.rm = TRUE) + var(g2, na.rm = TRUE)) / 2)
    std_diff <- (mean(g1, na.rm = TRUE) - mean(g2, na.rm = TRUE)) / pooled_sd
    
    # T-test
    t_test <- t.test(g1, g2)
    
    tibble(
      covariate = cov,
      mean_g1 = mean(g1, na.rm = TRUE),
      mean_g2 = mean(g2, na.rm = TRUE),
      std_diff = std_diff,
      p_value = t_test$p.value
    )
  })
  
  # Add group names to column headers
  names(balance_stats)[2:3] <- paste0("mean_", groups)
  
  return(balance_stats)
}

# ==============================================================================
# PRINT UTILITIES
# ==============================================================================

#' Print formatted section header
#'
#' @param title Section title
#' @param width Line width (default 78)
#' @export
print_section <- function(title, width = 78) {
  cat("\n")
  cat(rep("=", width), "\n", sep = "")
  cat(title, "\n")
  cat(rep("-", width), "\n", sep = "")
}

#' Print progress message with timestamp
#'
#' @param message Message to print
#' @param level Level: INFO, WARNING, ERROR
#' @export
print_log <- function(message, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(sprintf("[%s] [%s] %s\n", timestamp, level, message))
}
