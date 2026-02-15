# ==============================================================================
# check_final_dataset.R
# Diagnostic script to validate the final dataset
# ==============================================================================

library(tidyverse)
library(glue)

cat("\n")
cat("==============================================================================\n")
cat("FINAL DATASET VALIDATION\n")
cat("==============================================================================\n")

# Load dataset
df <- readRDS("data/processed/deals_with_10k_text_analysis.rds")

cat(glue("\nDataset: {nrow(df)} rows, {ncol(df)} columns\n\n"))

# ==============================================================================
# 1. UNIQUENESS CHECK
# ==============================================================================

cat("--- 1. UNIQUENESS CHECKS ---\n")

# One row per deal
n_unique_deals <- n_distinct(df$logical_deal_id)
check_unique <- n_unique_deals == nrow(df)
cat(glue("  Unique logical_deal_id: {n_unique_deals} (rows: {nrow(df)}) → {if(check_unique) '✓' else '✗'}\n"))

# One accession per row (but same accession can appear in multiple rows)
n_unique_acc <- n_distinct(df$accession_number)
cat(glue("  Unique accession_numbers: {n_unique_acc}\n"))
cat(glue("  Deals sharing a filing: {nrow(df) - n_unique_acc}\n"))

# ==============================================================================
# 2. MISSING VALUES
# ==============================================================================

cat("\n--- 2. MISSING VALUES ---\n")

critical_cols <- c("logical_deal_id", "target_name", "date_announced", 
                   "matched_cik", "accession_number", "filing_date", "days_lag",
                   "mda_text", "risk_factors_text")

for (col in critical_cols) {
  if (col %in% names(df)) {
    n_missing <- sum(is.na(df[[col]]))
    pct <- round(100 * n_missing / nrow(df), 1)
    status <- if (n_missing == 0) "✓" else "⚠"
    cat(glue("  {status} {col}: {n_missing} missing ({pct}%)\n"))
  } else {
    cat(glue("  ✗ {col}: COLUMN NOT FOUND\n"))
  }
}

# ==============================================================================
# 3. LAG DISTRIBUTION
# ==============================================================================

cat("\n--- 3. LAG DISTRIBUTION ---\n")

lag_stats <- df %>%
  summarise(
    min = min(days_lag),
    q25 = quantile(days_lag, 0.25),
    median = median(days_lag),
    mean = round(mean(days_lag), 1),
    q75 = quantile(days_lag, 0.75),
    max = max(days_lag)
  )

cat(glue("  Min: {lag_stats$min} days\n"))
cat(glue("  25th percentile: {lag_stats$q25} days\n"))
cat(glue("  Median: {lag_stats$median} days\n"))
cat(glue("  Mean: {lag_stats$mean} days\n"))
cat(glue("  75th percentile: {lag_stats$q75} days\n"))
cat(glue("  Max: {lag_stats$max} days\n"))

# Check lag < 90 days (potential issue)
n_short_lag <- sum(df$days_lag < 90)
if (n_short_lag > 0) {
  cat(glue("\n  ⚠ WARNING: {n_short_lag} deals have lag < 90 days\n"))
  cat("  Examples:\n")
  short_lag_examples <- df %>%
    filter(days_lag < 90) %>%
    arrange(days_lag) %>%
    select(target_name, date_announced, filing_date, days_lag) %>%
    head(5)
  print(short_lag_examples)
}

# ==============================================================================
# 4. TEMPORAL ORDER
# ==============================================================================

cat("\n--- 4. TEMPORAL ORDER ---\n")

df <- df %>%
  mutate(
    filing_date = as.Date(filing_date),
    date_announced = as.Date(date_announced)
  )

wrong_order <- df %>%
  filter(filing_date >= date_announced)

if (nrow(wrong_order) == 0) {
  cat("  ✓ All filings are before announcements\n")
} else {
  cat(glue("  ✗ {nrow(wrong_order)} deals have filing >= announcement\n"))
  print(wrong_order %>% select(target_name, filing_date, date_announced))
}

# ==============================================================================
# 5. TEXT QUALITY
# ==============================================================================

cat("\n--- 5. TEXT QUALITY ---\n")

text_stats <- df %>%
  summarise(
    mda_mean = round(mean(mda_word_count)),
    mda_median = median(mda_word_count),
    mda_min = min(mda_word_count),
    mda_max = max(mda_word_count),
    risk_mean = round(mean(risk_word_count)),
    risk_median = median(risk_word_count),
    risk_min = min(risk_word_count),
    risk_max = max(risk_word_count)
  )

cat("  MD&A word counts:\n")
cat(glue("    Mean: {text_stats$mda_mean}, Median: {text_stats$mda_median}\n"))
cat(glue("    Range: [{text_stats$mda_min}, {text_stats$mda_max}]\n"))

cat("  Risk Factors word counts:\n")
cat(glue("    Mean: {text_stats$risk_mean}, Median: {text_stats$risk_median}\n"))
cat(glue("    Range: [{text_stats$risk_min}, {text_stats$risk_max}]\n"))

# Check for suspiciously short texts
n_short_mda <- sum(df$mda_word_count < 1000)
n_short_risk <- sum(df$risk_word_count < 500)

if (n_short_mda > 0) {
  cat(glue("\n  ⚠ {n_short_mda} deals have MD&A < 1000 words\n"))
}
if (n_short_risk > 0) {
  cat(glue("  ⚠ {n_short_risk} deals have Risk Factors < 500 words\n"))
}

# ==============================================================================
# 6. DATE RANGE
# ==============================================================================

cat("\n--- 6. DATE RANGE ---\n")

date_range <- df %>%
  summarise(
    min_ann = min(date_announced),
    max_ann = max(date_announced),
    min_fil = min(filing_date),
    max_fil = max(filing_date)
  )

cat(glue("  Announcements: {date_range$min_ann} to {date_range$max_ann}\n"))
cat(glue("  Filings: {date_range$min_fil} to {date_range$max_fil}\n"))

# ==============================================================================
# 7. SAMPLE TEXT INSPECTION
# ==============================================================================

cat("\n--- 7. SAMPLE TEXT INSPECTION ---\n")

sample_row <- df %>% slice_sample(n = 1)

cat(glue("  Target: {sample_row$target_name}\n"))
cat(glue("  Announced: {sample_row$date_announced}\n"))
cat(glue("  Filing: {sample_row$filing_date} (lag: {sample_row$days_lag} days)\n"))
cat(glue("  MD&A words: {sample_row$mda_word_count}\n"))
cat(glue("  Risk words: {sample_row$risk_word_count}\n"))

cat("\n  MD&A preview (first 500 chars):\n")
cat("  ", str_sub(sample_row$mda_text, 1, 500), "...\n")

cat("\n  Risk Factors preview (first 500 chars):\n")
cat("  ", str_sub(sample_row$risk_factors_text, 1, 500), "...\n")

# ==============================================================================
# SUMMARY
# ==============================================================================

cat("\n")
cat("==============================================================================\n")
cat("SUMMARY\n")
cat("==============================================================================\n")

issues <- c()

if (!check_unique) issues <- c(issues, "Duplicate deals")
if (n_short_lag > 0) issues <- c(issues, glue("{n_short_lag} deals with lag < 90 days"))
if (nrow(wrong_order) > 0) issues <- c(issues, "Temporal order violations")

if (length(issues) == 0) {
  cat("\n✓ Dataset appears correct and ready for analysis!\n")
} else {
  cat("\n⚠ Issues found:\n")
  for (issue in issues) {
    cat(glue("  - {issue}\n"))
  }
}

cat(glue("\nFinal sample size: {nrow(df)} deals with complete text data\n"))
cat(glue("Total corpus: {scales::comma(sum(df$mda_word_count) + sum(df$risk_word_count))} words\n"))
cat("\n")
