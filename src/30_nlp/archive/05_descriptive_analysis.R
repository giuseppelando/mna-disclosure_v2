# =============================================================================
# MODULE 5: DESCRIPTIVE ANALYSIS AND CROSS-VALIDATION
# =============================================================================
# Project: M&A Disclosure Quality (10-K textual analysis)
# Purpose:
#   (i) Characterize distributions of disclosure indices
#   (ii) Check multicollinearity among indices
#   (iii) Known-groups validity (industry differences)
#   (iv) Time trends (drift over calendar time)
#
# Inputs (produced by earlier modules):
#   - data/interim/disclosure_indices.rds        (Module 3 output)
#   - data/deals_with_10k_text_analysis.rds      (base deal dataset; optional merge)
#
# Outputs:
#   - reports/05_descriptive_analysis.md
#   - output/figures/ : histograms, QQ plots, correlation heatmap, time trends
#   - output/tables/  : summary_stats.csv, correlation_matrix.csv,
#                       high_correlation_pairs_ge_0p70.csv,
#                       anova_industry_by_index.csv, industry_means_indices.csv,
#                       time_trends_means_by_year.csv,
#                       fallback_usage_by_index.csv (if present)
#
# Notes:
#   - This module is descriptive only. It does not estimate outcome models.
#   - Plots are saved to disk for inclusion in thesis appendix.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readr)
  library(stringr)
})

# ----------------------------
# Helper: safe dir creation
# ----------------------------
ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

# ----------------------------
# Helper: robust file read
# ----------------------------
read_rds_safe <- function(path) {
  if (!file.exists(path)) stop("Missing file: ", path)
  readRDS(path)
}

# ----------------------------
# Paths (project-root relative)
# ----------------------------
cat("=== MODULE 5: DESCRIPTIVE ANALYSIS (v2) ===\n")

path_indices <- file.path("data", "interim", "disclosure_indices.rds")
path_base    <- file.path("data", "deals_with_10k_text_analysis.rds") # optional for year/sic if needed

out_fig  <- file.path("output", "figures")
out_tab  <- file.path("output", "tables")
out_rep  <- file.path("reports")
ensure_dir(out_fig); ensure_dir(out_tab); ensure_dir(out_rep)

# ----------------------------
# Load data
# ----------------------------
cat("\nBLOCK 0: Loading indices dataset...\n")
idx <- read_rds_safe(path_indices)

cat("  - Indices rows: ", nrow(idx), "\n", sep = "")
cat("  - Indices cols: ", ncol(idx), "\n", sep = "")

# Required join key
if (!("deal_id" %in% names(idx))) stop("disclosure_indices.rds must contain deal_id.")

# Try to identify year + industry variables for grouping
candidate_year <- c("year_announced", "year", "announce_year")
candidate_sic2 <- c("sic2", "target_primary_sic_2digit", "target_primary_sic2")

year_col <- candidate_year[candidate_year %in% names(idx)][1]
sic2_col <- candidate_sic2[candidate_sic2 %in% names(idx)][1]

# If not present in idx, merge from base dataset (only if base exists)
if (is.na(year_col) || is.na(sic2_col)) {
  if (file.exists(path_base)) {
    cat("  - year/sic2 not found in indices; merging from base dataset...\n")
    base <- read_rds_safe(path_base)

    if (!("deal_id" %in% names(base))) stop("Base dataset missing deal_id for merge.")

    year_base <- candidate_year[candidate_year %in% names(base)][1]
    sic2_base <- candidate_sic2[candidate_sic2 %in% names(base)][1]
    if (is.na(year_base) || is.na(sic2_base)) {
      stop("Could not locate year and sic2 columns in base dataset to support Module 5.")
    }

    idx <- idx %>%
      left_join(base %>% select(deal_id, !!year_base, !!sic2_base),
                by = "deal_id") %>%
      rename(year_announced = !!year_base,
             sic2 = !!sic2_base)

    year_col <- "year_announced"
    sic2_col <- "sic2"
  } else {
    stop("Need year and sic2 for Module 5, but they are missing in indices and base file not found.")
  }
}

# Standardize types
idx <- idx %>%
  mutate(
    year_announced = as.integer(.data[[year_col]]),
    sic2 = as.character(.data[[sic2_col]])
  )

# Identify index columns (prefer normalized versions)
index_candidates <- c(
  "operational_specificity_norm",
  "forward_looking_norm",
  "risk_disclosure_tfidf_norm",
  "tone_lm_norm",
  # fallbacks (if normalized not present)
  "operational_specificity",
  "forward_looking_intensity",
  "risk_disclosure_tfidf",
  "tone_lm"
)

indices <- index_candidates[index_candidates %in% names(idx)]
if (length(indices) == 0) {
  stop("No index columns found in disclosure_indices.rds. Expected one of: ",
       paste(index_candidates, collapse = ", "))
}

cat("  - Indices used: ", paste(indices, collapse = ", "), "\n", sep = "")

# REVISED: separate normalized (for correlation/histograms) from raw (for trends)
indices_norm <- c("operational_specificity_norm", "forward_looking_norm",
                  "risk_disclosure_tfidf_norm", "tone_lm_norm")
indices_norm <- indices_norm[indices_norm %in% names(idx)]

# ----------------------------
# BLOCK 1: Summary statistics
# ----------------------------
cat("\nBLOCK 1: Computing summary statistics...\n")

summ_stats <- idx %>%
  summarise(across(all_of(indices), list(
    n = ~sum(!is.na(.)),
    mean = ~mean(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    p01 = ~quantile(., 0.01, na.rm = TRUE, names = FALSE),
    p05 = ~quantile(., 0.05, na.rm = TRUE, names = FALSE),
    p25 = ~quantile(., 0.25, na.rm = TRUE, names = FALSE),
    p50 = ~quantile(., 0.50, na.rm = TRUE, names = FALSE),
    p75 = ~quantile(., 0.75, na.rm = TRUE, names = FALSE),
    p95 = ~quantile(., 0.95, na.rm = TRUE, names = FALSE),
    p99 = ~quantile(., 0.99, na.rm = TRUE, names = FALSE)
  ), .names = "{.col}__{.fn}"))

summ_stats_long <- summ_stats %>%
  pivot_longer(cols = everything(),
               names_to = c("index", "stat"), names_sep = "__",
               values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  arrange(index)

write_csv(summ_stats_long, file.path(out_tab, "summary_stats.csv"))
cat("  - Saved: ", file.path(out_tab, "summary_stats.csv"), "\n", sep = "")

# ----------------------------
# BLOCK 2: Univariate distributions
# ----------------------------
cat("\nBLOCK 2: Plotting histograms and QQ plots...\n")

for (v in indices) {
  dfv <- idx %>% select(all_of(v)) %>% filter(!is.na(.data[[v]]))

  p_hist <- ggplot(dfv, aes(x = .data[[v]])) +
    geom_histogram(bins = 50) +
    labs(title = paste0("Histogram: ", v), x = v, y = "Frequency") +
    theme_minimal()

  ggsave(filename = file.path(out_fig, paste0("hist_", v, ".png")),
         plot = p_hist, width = 8, height = 5, dpi = 300)

  p_qq <- ggplot(dfv, aes(sample = .data[[v]])) +
    stat_qq() +
    stat_qq_line() +
    labs(title = paste0("Q-Q Plot: ", v),
         x = "Theoretical Quantiles", y = "Sample Quantiles") +
    theme_minimal()

  ggsave(filename = file.path(out_fig, paste0("qq_", v, ".png")),
         plot = p_qq, width = 8, height = 5, dpi = 300)
}
cat("  - Saved figures to: ", out_fig, "\n", sep = "")

# ----------------------------
# BLOCK 3: Correlation matrix and heatmap
# ----------------------------
cat("\nBLOCK 3: Correlation matrix...\n")

# REVISED: use only the four normalized indices (avoids mechanical raw/norm pairs)
corr_df <- idx %>% select(all_of(indices_norm))
cor_matrix <- suppressWarnings(cor(corr_df, use = "pairwise.complete.obs"))

cor_out <- as.data.frame(cor_matrix) %>%
  tibble::rownames_to_column("index_row")
write_csv(cor_out, file.path(out_tab, "correlation_matrix.csv"))
cat("  - Saved: ", file.path(out_tab, "correlation_matrix.csv"), "\n", sep = "")

cor_long <- as.data.frame(as.table(cor_matrix)) %>%
  rename(index_row = Var1, index_col = Var2, corr = Freq)

p_cor <- ggplot(cor_long, aes(x = index_col, y = index_row, fill = corr)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", corr)), size = 3) +
  labs(title = "Correlation Heatmap (indices)", x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = file.path(out_fig, "corr_heatmap_indices.png"),
       plot = p_cor, width = 7.5, height = 6.5, dpi = 300)
cat("  - Saved: ", file.path(out_fig, "corr_heatmap_indices.png"), "\n", sep = "")

high_pairs <- cor_long %>%
  mutate(
    index_row = as.character(index_row),
    index_col = as.character(index_col),
    row_id = match(index_row, colnames(cor_matrix)),
    col_id = match(index_col, colnames(cor_matrix))
  ) %>%
  filter(!is.na(corr), row_id < col_id) %>%
  mutate(abs_corr = abs(corr)) %>%
  arrange(desc(abs_corr)) %>%
  filter(abs_corr >= 0.70) %>%
  select(index_row, index_col, corr, abs_corr)

write_csv(high_pairs, file.path(out_tab, "high_correlation_pairs_ge_0p70.csv"))
cat("  - Saved: ", file.path(out_tab, "high_correlation_pairs_ge_0p70.csv"), "\n", sep = "")

# ----------------------------
# BLOCK 4: Known-groups validity (industry differences)
# ----------------------------
cat("\nBLOCK 4: Known-groups validity (industry differences via ANOVA)...\n")

# NOTE (defensibility): if an index is normalized within sic2 or sic2×year,
# testing differences across sic2 is mechanically uninformative. Therefore,
# we run industry ANOVA only on *raw* (pre-normalization) indices.

raw_candidates <- list(
  operational_specificity = c("operational_specificity", "operational_specificity_raw"),
  forward_looking = c("forward_looking_density", "forward_looking_intensity"),
  risk_disclosure = c("risk_disclosure_tfidf"),
  tone = c("tone_lm")
)

raw_vars <- unlist(lapply(raw_candidates, function(opts) opts[opts %in% names(idx)][1]), use.names = FALSE)
raw_vars <- raw_vars[!is.na(raw_vars)]

if (length(raw_vars) == 0) {
  cat("  - No raw indices found in dataset. Skipping industry ANOVA (by design).\n")
}

anova_results <- list()

for (v in raw_vars) {
  dfv <- idx %>%
    select(all_of(c("sic2", v))) %>%
    filter(!is.na(.data[[v]]), !is.na(sic2))

  grp_counts <- dfv %>% count(sic2, name = "n") %>% filter(n >= 10)
  dfv <- dfv %>% semi_join(grp_counts, by = "sic2")

  if (nrow(dfv) < 50 || dplyr::n_distinct(dfv$sic2) < 3) {
    anova_results[[v]] <- tibble::tibble(
      index = v,
      status = "skipped_insufficient_groups",
      df1 = NA_real_, df2 = NA_real_, f_stat = NA_real_, p_value = NA_real_,
      n_groups = dplyr::n_distinct(dfv$sic2), n_obs = nrow(dfv)
    )
    next
  }

  fit <- aov(dfv[[v]] ~ as.factor(dfv$sic2))
  s <- summary(fit)[[1]]

  anova_results[[v]] <- tibble::tibble(
    index = v,
    status = "ok",
    df1 = s[1, "Df"],
    df2 = s[2, "Df"],
    f_stat = s[1, "F value"],
    p_value = s[1, "Pr(>F)"],
    n_groups = dplyr::n_distinct(dfv$sic2),
    n_obs = nrow(dfv)
  )
}

anova_tbl <- dplyr::bind_rows(anova_results)
write_csv(anova_tbl, file.path(out_tab, "anova_industry_by_index.csv"))
cat("  - Saved: ", file.path(out_tab, "anova_industry_by_index.csv"), "\n", sep = "")

industry_means <- idx %>%
  group_by(sic2) %>%
  summarise(across(all_of(raw_vars), ~mean(., na.rm = TRUE)),
            n = n(),
            .groups = "drop") %>%
  arrange(desc(n))

write_csv(industry_means, file.path(out_tab, "industry_means_indices.csv"))
cat("  - Saved: ", file.path(out_tab, "industry_means_indices.csv"), "\n", sep = "")

# ----------------------------
# BLOCK 5: Time trends
# ----------------------------
cat("\nBLOCK 5: Time trends...\n")

# REVISED: plot time trends on raw (pre-normalisation) indices.
# Normalised indices are z-scored within sic2/year cells so annual means
# are mechanically near zero — uninformative for detecting secular drift.
raw_for_trends <- raw_vars  # already identified in Block 4 (raw indices)

trends <- idx %>%
  filter(!is.na(year_announced)) %>%
  group_by(year_announced) %>%
  summarise(across(any_of(raw_for_trends), ~mean(., na.rm = TRUE)),
            n = n(),
            .groups = "drop") %>%
  arrange(year_announced)

write_csv(trends, file.path(out_tab, "time_trends_means_by_year.csv"))
cat("  - Saved: ", file.path(out_tab, "time_trends_means_by_year.csv"), "\n", sep = "")

trends_long <- trends %>%
  pivot_longer(cols = any_of(raw_for_trends), names_to = "index", values_to = "mean_value")

p_trend <- ggplot(trends_long, aes(x = year_announced, y = mean_value)) +
  geom_line() +
  facet_wrap(~index, scales = "free_y") +
  labs(title = "Average raw index value by announcement year",
       x = "Announcement year", y = "Mean raw index value") +
  theme_minimal()

ggsave(filename = file.path(out_fig, "time_trends_indices.png"),
       plot = p_trend, width = 10, height = 6, dpi = 300)
cat("  - Saved: ", file.path(out_fig, "time_trends_indices.png"), "\n", sep = "")

# ----------------------------
# BLOCK 6: Optional: normalization fallback usage (if present)
# ----------------------------
cat("\nBLOCK 6: Normalization fallback audit export (if present)...\n")

level_cols <- names(idx)[stringr::str_detect(names(idx), "_level$")]

if (length(level_cols) > 0) {
  fallback_usage <- idx %>%
    select(deal_id, all_of(level_cols)) %>%
    pivot_longer(cols = all_of(level_cols),
                 names_to = "index_level_var",
                 values_to = "level") %>%
    mutate(index = stringr::str_remove(index_level_var, "_level$")) %>%
    count(index, level, name = "n") %>%
    group_by(index) %>%
    mutate(share = n / sum(n)) %>%
    ungroup() %>%
    arrange(index, desc(n))

  write_csv(fallback_usage, file.path(out_tab, "fallback_usage_by_index.csv"))
  cat("  - Saved: ", file.path(out_tab, "fallback_usage_by_index.csv"), "\n", sep = "")
} else {
  cat("  - No *_level columns found. Skipping fallback usage export.\n")
}

# ----------------------------
# BLOCK 7: Markdown report
# ----------------------------
cat("\nBLOCK 7: Writing markdown report...\n")

report_path <- file.path(out_rep, "05_descriptive_analysis.md")

lines <- c(
  "# Module 5 — Descriptive analysis and cross-validation",
  "",
  "## Inputs",
  paste0("- Indices dataset: `", path_indices, "`"),
  if (file.exists(path_base)) paste0("- Base dataset (optional merge): `", path_base, "`") else "- Base dataset: (not used)",
  "",
  "## Indices analyzed",
  paste0("- ", paste(indices, collapse = ", ")),
  "",
  "## Outputs",
  "- Tables in `output/tables/`: summary stats, correlations (and flagged high-correlation pairs), ANOVA summaries, industry means, time-trend means.",
  "- Figures in `output/figures/`: histograms, Q-Q plots, correlation heatmap, time trends.",
  "",
  "## Interpretation guide (for thesis write-up)",
  "- Distribution plots: normalized indices should be centered near zero. Departures from normality are documented as empirical properties.",
  "- Correlations: pairs with |r| ≥ 0.70 are flagged for multicollinearity risk in later regressions.",
  "- Industry ANOVA: run only on raw (pre-normalization) indices; normalized indices are standardized within sic2/sic2×year and are therefore not suitable for this check.",
  "- Time trends: year-level drift supports the use of year fixed effects (or industry×year fixed effects) in outcome models."
)

# REVISED: concatenate session info BEFORE writing (avoids overwrite bug)
lines <- c(lines, "", "## Session info", "", capture.output(sessionInfo()))
writeLines(lines, report_path)

cat("  - Saved: ", report_path, "\n", sep = "")
cat("\n=== MODULE 5 COMPLETE (v2) ===\n")
