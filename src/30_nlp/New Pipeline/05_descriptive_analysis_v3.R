# =============================================================================
# MODULE 5: DESCRIPTIVE ANALYSIS AND CROSS-VALIDATION (REVISED v3)
# =============================================================================
# v3 CHANGES (feedback-driven):
#   1. BLOCK 1B: signal attenuation diagnostics — zero-count rates,
#      within-cell variance, coefficient of variation
#   2. Expanded correlation matrix with v6 indices
#   3. ANOVA and trends on all raw indices from v6
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(readr); library(stringr)
})

ensure_dir <- function(p) if (!dir.exists(p)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
read_rds_safe <- function(p) { if (!file.exists(p)) stop("Missing: ", p); readRDS(p) }

cat("=== MODULE 5: DESCRIPTIVE ANALYSIS (v3) ===\n")

path_indices <- file.path("data", "interim", "disclosure_indices.rds")
path_base    <- file.path("data", "deals_with_10k_text_analysis.rds")
out_fig <- "output/figures"; out_tab <- "output/tables"; out_rep <- "reports"
ensure_dir(out_fig); ensure_dir(out_tab); ensure_dir(out_rep)

idx <- read_rds_safe(path_indices)
cat("  - Rows: ", nrow(idx), " | Cols: ", ncol(idx), "\n")

# Locate year + sic2 (merge from base if needed)
year_col <- c("year_announced", "year")[c("year_announced", "year") %in% names(idx)][1]
sic2_col <- c("sic2")[c("sic2") %in% names(idx)][1]

if (is.na(year_col) || is.na(sic2_col)) {
  if (file.exists(path_base)) {
    base <- read_rds_safe(path_base)
    merge_cols <- c("deal_id")
    if ("year_announced" %in% names(base) && is.na(year_col)) merge_cols <- c(merge_cols, "year_announced")
    if (!"sic2" %in% names(idx) && "target_primary_sic" %in% names(base)) {
      base$sic2 <- substr(as.character(base$target_primary_sic), 1, 2)
      merge_cols <- c(merge_cols, "sic2")
    }
    idx <- idx %>% left_join(base %>% select(all_of(merge_cols)), by = "deal_id")
    year_col <- "year_announced"; sic2_col <- "sic2"
  }
}

idx <- idx %>% mutate(
  year_announced = as.integer(.data[[year_col]]),
  sic2 = as.character(.data[[sic2_col]])
)

# Identify index columns
indices_norm <- c("operational_specificity_norm", "forward_looking_norm",
                  "risk_transparency_norm", "tone_lm_norm")
indices_norm <- indices_norm[indices_norm %in% names(idx)]

raw_vars <- c("operational_specificity_raw", "fl_sentence_share", "fl_precision_share",
              "commitment_density", "hedging_density", "risk_transparency",
              "risk_disclosure_raw", "risk_numeric_density",
              "tone_lm", "pos_density", "neg_density")
raw_vars <- raw_vars[raw_vars %in% names(idx)]

all_indices <- unique(c(indices_norm, raw_vars))

# ---- BLOCK 1: Summary stats ----
cat("\nBLOCK 1: Summary statistics...\n")
summ <- idx %>%
  summarise(across(all_of(all_indices), list(
    n = ~sum(!is.na(.)), mean = ~mean(., na.rm=T), sd = ~sd(., na.rm=T),
    p05 = ~quantile(., .05, na.rm=T, names=F), p25 = ~quantile(., .25, na.rm=T, names=F),
    p50 = ~quantile(., .50, na.rm=T, names=F), p75 = ~quantile(., .75, na.rm=T, names=F),
    p95 = ~quantile(., .95, na.rm=T, names=F)
  ), .names = "{.col}__{.fn}")) %>%
  pivot_longer(everything(), names_to = c("index", "stat"), names_sep = "__", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value) %>% arrange(index)

write_csv(summ, file.path(out_tab, "summary_stats.csv"))

# ---- BLOCK 1B: SIGNAL ATTENUATION DIAGNOSTICS (v3 NEW) ----
cat("\nBLOCK 1B: Signal attenuation diagnostics...\n")

attenuation <- lapply(raw_vars, function(v) {
  vals <- idx[[v]][!is.na(idx[[v]])]
  n_zero <- sum(vals == 0)
  within <- idx %>% filter(!is.na(.data[[v]]), !is.na(sic2), !is.na(year_announced)) %>%
    group_by(sic2, year_announced) %>%
    summarise(sd = sd(.data[[v]], na.rm=T), n = n(), .groups = "drop") %>%
    filter(n >= 5)
  tibble(index = v, n = length(vals), n_zero = n_zero,
         pct_zero = round(100 * n_zero / max(length(vals), 1), 1),
         overall_sd = round(sd(vals), 6),
         median_within_sd = round(median(within$sd, na.rm = T), 6),
         cv = round(sd(vals) / (abs(mean(vals)) + 1e-10), 3))
})
attenuation_tbl <- bind_rows(attenuation)
write_csv(attenuation_tbl, file.path(out_tab, "signal_attenuation_diagnostics.csv"))
cat("  - Key diagnostics:\n")
for (i in seq_len(nrow(attenuation_tbl))) {
  cat(sprintf("    %-30s zeros=%.1f%% sd=%.4f within_sd=%.4f cv=%.3f\n",
              attenuation_tbl$index[i], attenuation_tbl$pct_zero[i],
              attenuation_tbl$overall_sd[i], attenuation_tbl$median_within_sd[i],
              attenuation_tbl$cv[i]))
}

# ---- BLOCK 2: Histograms / QQ ----
cat("\nBLOCK 2: Plotting...\n")
for (v in all_indices) {
  dfv <- idx %>% filter(!is.na(.data[[v]]))
  p <- ggplot(dfv, aes(x = .data[[v]])) + geom_histogram(bins = 50) +
    labs(title = v) + theme_minimal()
  ggsave(file.path(out_fig, paste0("hist_", v, ".png")), p, width = 8, height = 5, dpi = 200)
}

# ---- BLOCK 3: Correlation matrix ----
cat("\nBLOCK 3: Correlations...\n")
corr_vars <- all_indices[all_indices %in% names(idx)]
cor_mat <- suppressWarnings(cor(idx[corr_vars], use = "pairwise.complete.obs"))
write_csv(as.data.frame(cor_mat) %>% tibble::rownames_to_column("row"),
          file.path(out_tab, "correlation_matrix.csv"))

cor_long <- as.data.frame(as.table(cor_mat)) %>% rename(row = Var1, col = Var2, r = Freq)
p <- ggplot(cor_long, aes(col, row, fill = r)) + geom_tile() +
  geom_text(aes(label = sprintf("%.2f", r)), size = 2) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
                           axis.text.y = element_text(size = 6))
ggsave(file.path(out_fig, "corr_heatmap_indices.png"), p, width = 11, height = 9, dpi = 200)

high_pairs <- cor_long %>%
  mutate(row = as.character(row), col = as.character(col),
         ri = match(row, colnames(cor_mat)), ci = match(col, colnames(cor_mat))) %>%
  filter(!is.na(r), ri < ci, abs(r) >= 0.70) %>%
  select(row, col, r) %>% arrange(desc(abs(r)))
write_csv(high_pairs, file.path(out_tab, "high_correlation_pairs_ge_0p70.csv"))

# ---- BLOCK 4: Industry ANOVA (raw) ----
cat("\nBLOCK 4: Industry ANOVA...\n")
anova_res <- lapply(raw_vars, function(v) {
  d <- idx %>% filter(!is.na(.data[[v]]), !is.na(sic2)) %>%
    semi_join(idx %>% count(sic2) %>% filter(n >= 10), by = "sic2")
  if (nrow(d) < 50 || n_distinct(d$sic2) < 3) return(tibble(index = v, status = "skip"))
  s <- summary(aov(d[[v]] ~ as.factor(d$sic2)))[[1]]
  tibble(index = v, status = "ok", F = s[1, "F value"], p = s[1, "Pr(>F)"],
         n_groups = n_distinct(d$sic2), n = nrow(d))
})
write_csv(bind_rows(anova_res), file.path(out_tab, "anova_industry_by_index.csv"))

# ---- BLOCK 5: Time trends ----
cat("\nBLOCK 5: Time trends...\n")
trends <- idx %>% filter(!is.na(year_announced)) %>%
  group_by(year_announced) %>%
  summarise(across(any_of(raw_vars), ~mean(., na.rm=T)), n = n(), .groups = "drop")
write_csv(trends, file.path(out_tab, "time_trends_means_by_year.csv"))

trends_long <- trends %>% pivot_longer(any_of(raw_vars), names_to = "index", values_to = "val")
p <- ggplot(trends_long, aes(year_announced, val)) + geom_line() +
  facet_wrap(~index, scales = "free_y") + theme_minimal()
ggsave(file.path(out_fig, "time_trends_indices.png"), p, width = 12, height = 8, dpi = 200)

# ---- BLOCK 6: Fallback audit ----
level_cols <- names(idx)[str_detect(names(idx), "_level$")]
if (length(level_cols) > 0) {
  fb <- idx %>% select(deal_id, all_of(level_cols)) %>%
    pivot_longer(all_of(level_cols), names_to = "var", values_to = "level") %>%
    count(var, level) %>% group_by(var) %>% mutate(share = n / sum(n)) %>% ungroup()
  write_csv(fb, file.path(out_tab, "fallback_usage_by_index.csv"))
}

# ---- Report ----
writeLines(c(
  "# Module 5 — Descriptive Analysis (v3)", "",
  "## Key v3 addition: signal attenuation diagnostics",
  "See signal_attenuation_diagnostics.csv for zero-count rates and within-cell variance.",
  "Indices with >50% zeros or very low within-cell SD may be quasi-degenerate.",
  "", "## Outputs", "- Tables: output/tables/", "- Figures: output/figures/"
), file.path(out_rep, "05_descriptive_analysis.md"))

cat("\n=== MODULE 5 COMPLETE (v3) ===\n")
