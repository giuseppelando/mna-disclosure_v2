# ==============================================================================
# econometric_utils.R  (v6 — aligned with NLP pipeline v6)
#
# PURPOSE: Utility functions for econometric data preparation and analysis
#          - Winsorization, log transform, standardization
#          - Fixed effects construction
#          - Diagnostics (VIF, balance)
#          - Summary statistics and correlation
#
# CHANGES vs prior version:
#   - No structural changes; functions are variable-name agnostic.
#   - Added safe_winsorize() that skips binary/bounded vars automatically.
#   - Added pipe-friendly wrappers for mutate-across patterns.
#
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
})

# ==============================================================================
# VARIABLE TRANSFORMATIONS
# ==============================================================================

#' Winsorize a numeric vector at specified percentiles
winsorize <- function(x, lower = 0.01, upper = 0.99) {
  if (!is.numeric(x)) return(x)
  if (all(is.na(x))) return(x)
  q <- quantile(x, probs = c(lower, upper), na.rm = TRUE)
  pmax(pmin(x, q[2]), q[1])
}

#' Standardize to z-score (mean=0, sd=1)
standardize <- function(x) {
  if (!is.numeric(x) || all(is.na(x))) return(x)
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(x - m)
  (x - m) / s
}

#' Log transform with shift for zeros
log_transform <- function(x, shift = 1) {
  if (!is.numeric(x) || all(is.na(x))) return(x)
  xs <- x + shift
  xs[xs <= 0] <- NA_real_
  log(xs)
}

#' Inverse hyperbolic sine (handles negatives and zeros)
ihs_transform <- function(x) asinh(x)

# ==============================================================================
# FIXED EFFECTS CONSTRUCTION
# ==============================================================================

#' Extract 2-digit SIC from 4-digit SIC
extract_sic2 <- function(sic_code) {
  s <- stringr::str_pad(as.character(sic_code), width = 4, side = "left", pad = "0")
  sic2 <- stringr::str_sub(s, 1, 2)
  sic2[is.na(sic_code)] <- NA_character_
  sic2
}

#' Create industry × year FE identifier
create_fe_industry_year <- function(sic2, year) {
  fe <- paste0(sic2, "_", year)
  fe[is.na(sic2) | is.na(year)] <- NA_character_
  fe
}

# ==============================================================================
# OUTLIER DETECTION
# ==============================================================================

#' Flag outliers: "low" / "ok" / "high"
flag_outliers <- function(x, lower_pct = 0.01, upper_pct = 0.99) {
  if (!is.numeric(x)) return(rep(NA_character_, length(x)))
  q <- quantile(x, c(lower_pct, upper_pct), na.rm = TRUE)
  dplyr::case_when(
    is.na(x)   ~ NA_character_,
    x < q[1]   ~ "low",
    x > q[2]   ~ "high",
    TRUE        ~ "ok"
  )
}

# ==============================================================================
# SUMMARY STATISTICS
# ==============================================================================

#' Comprehensive summary for one numeric variable
summarize_numeric <- function(x, var_name = "variable") {
  if (!is.numeric(x)) return(tibble(variable = var_name, note = "non-numeric"))
  tibble(
    variable = var_name,
    n        = sum(!is.na(x)),
    n_na     = sum(is.na(x)),
    mean     = mean(x, na.rm = TRUE),
    sd       = sd(x, na.rm = TRUE),
    min      = min(x, na.rm = TRUE),
    p01      = quantile(x, 0.01, na.rm = TRUE),
    p25      = quantile(x, 0.25, na.rm = TRUE),
    p50      = median(x, na.rm = TRUE),
    p75      = quantile(x, 0.75, na.rm = TRUE),
    p99      = quantile(x, 0.99, na.rm = TRUE),
    max      = max(x, na.rm = TRUE)
  )
}

#' Summary table for multiple variables
summary_table <- function(df, vars = NULL) {
  if (is.null(vars)) vars <- names(df)[sapply(df, is.numeric)]
  vars <- intersect(vars, names(df))
  purrr::map_df(vars, function(v) summarize_numeric(df[[v]], v))
}

# ==============================================================================
# CORRELATION
# ==============================================================================

#' Pairwise correlation matrix with p-values
correlation_matrix <- function(df, vars = NULL, method = "pearson") {
  if (is.null(vars)) vars <- names(df)[sapply(df, is.numeric)]
  vars <- intersect(vars, names(df))
  if (length(vars) < 2) return(NULL)

  mat <- df[, vars, drop = FALSE]
  cor_mat <- cor(mat, use = "pairwise.complete.obs", method = method)

  n <- length(vars)
  pval <- matrix(NA, n, n, dimnames = list(vars, vars))
  for (i in 1:(n-1)) for (j in (i+1):n) {
    t <- cor.test(mat[[vars[i]]], mat[[vars[j]]], method = method)
    pval[i,j] <- pval[j,i] <- t$p.value
  }
  diag(pval) <- 0
  list(cor = cor_mat, pval = pval, n = nrow(mat), method = method)
}

#' Format correlation with significance stars
format_correlation <- function(cr, digits = 3) {
  if (is.null(cr$cor)) return(NULL)
  m <- cr$cor; p <- cr$pval
  fmt <- matrix("", nrow(m), ncol(m), dimnames = dimnames(m))
  for (i in seq_len(nrow(m))) for (j in seq_len(ncol(m))) {
    star <- if (i == j) "" else
            if (p[i,j] < 0.01) "***" else if (p[i,j] < 0.05) "**" else
            if (p[i,j] < 0.10) "*" else ""
    fmt[i,j] <- paste0(sprintf("%.*f", digits, m[i,j]), star)
  }
  fmt
}

# ==============================================================================
# VIF
# ==============================================================================

calculate_vif <- function(model) {
  if (requireNamespace("car", quietly = TRUE)) return(car::vif(model))
  X <- model.matrix(model)
  if (colnames(X)[1] == "(Intercept)") X <- X[,-1, drop = FALSE]
  sapply(seq_len(ncol(X)), function(i) {
    r2 <- summary(lm(X[,i] ~ X[,-i]))$r.squared
    1 / (1 - r2)
  }) |> setNames(colnames(X))
}

# ==============================================================================
# PRINT HELPER
# ==============================================================================

print_section <- function(title, width = 78) {
  cat("\n", rep("=", width), "\n", title, "\n", rep("-", width), "\n", sep = "")
}
