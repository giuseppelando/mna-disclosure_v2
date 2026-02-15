# ==============================================================================
# 00a_match_cik_automatic.R
# Matching automatico tra target SDC e CIK da companies.rds
#
# WORKFLOW:
#   1. Esegui questo script per match automatico
#   2. Apri l'output Excel e aggiungi/correggi CIK manualmente
#   3. Salva come deals_with_cik_matches.xlsx
#   4. Esegui la pipeline (01 → 02 → 03 → ...)
#
# INPUT:  data/raw/GridExport_*.xlsx (SDC export)
#         data/raw/companies.rds (SEC company list)
# OUTPUT: data/interim/deals_cik_auto_matched.xlsx (per review manuale)
# ==============================================================================

library(tidyverse)
library(readxl)
library(writexl)
library(glue)

cat("\n")
cat("==============================================================================\n")
cat("AUTOMATIC CIK MATCHING\n")
cat("==============================================================================\n")

# Detect project root
current_dir <- getwd()
if (dir.exists(file.path(current_dir, "config"))) {
  PROJECT_ROOT <- current_dir
} else {
  PROJECT_ROOT <- dirname(current_dir)
}

# ==============================================================================
# Load SDC deals
# ==============================================================================

cat("\nLoading SDC deals...\n")

sdc_files <- list.files(
  file.path(PROJECT_ROOT, "data/raw"), 
  pattern = "GridExport.*\\.xlsx$", 
  full.names = TRUE
)

if (length(sdc_files) == 0) {
  stop("No SDC export file found in data/raw/")
}

sdc_path <- sdc_files[1]  # Use most recent
cat(glue("  → File: {basename(sdc_path)}\n"))

deals_raw <- read_xlsx(sdc_path)
cat(glue("  → Loaded {nrow(deals_raw)} deals\n"))

# Standardize column names
deals <- deals_raw %>%
  janitor::clean_names()

# Identify target name column
target_col <- names(deals)[str_detect(names(deals), "target.*name|target.*full")]
if (length(target_col) == 0) {
  stop("Cannot find target name column")
}
target_col <- target_col[1]
cat(glue("  → Target column: {target_col}\n"))

# ==============================================================================
# Load companies database
# ==============================================================================

cat("\nLoading companies database...\n")

companies_path <- file.path(PROJECT_ROOT, "data/raw/companies.rds")
if (!file.exists(companies_path)) {
  stop("companies.rds not found in data/raw/")
}

companies <- readRDS(companies_path)
cat(glue("  → Loaded {nrow(companies)} companies\n"))
cat(glue("  → Unique CIKs: {n_distinct(companies$cik_key)}\n"))

# ==============================================================================
# Name normalization function
# ==============================================================================

normalize_name <- function(name) {
  name %>%
    str_to_lower() %>%
    str_remove_all("[[:punct:]]") %>%
    str_remove_all("\\b(inc|corp|co|ltd|llc|lp|plc|sa|nv|ag)\\b") %>%
    str_squish() %>%
    str_trim()
}

# ==============================================================================
# Tier 1: Exact match
# ==============================================================================

cat("\nTier 1: Exact matching...\n")

deals <- deals %>%
  mutate(
    target_name_clean = .data[[target_col]],
    target_name_norm = normalize_name(target_name_clean)
  )

companies_lookup <- companies %>%
  select(name, cik_key) %>%
  distinct() %>%
  mutate(name_norm = normalize_name(name))

# Exact match on normalized names
tier1 <- deals %>%
  left_join(
    companies_lookup %>% select(name_norm, cik_key),
    by = c("target_name_norm" = "name_norm"),
    relationship = "many-to-many"
  ) %>%
  rename(target_cik = cik_key)

n_matched_t1 <- sum(!is.na(tier1$target_cik))
cat(glue("  → Matched: {n_distinct(tier1$target_name_clean[!is.na(tier1$target_cik)])} unique targets\n"))

# ==============================================================================
# Consolidate results
# ==============================================================================

cat("\nConsolidating results...\n")

# For deals with multiple CIK matches, keep all (user will review)
# For deals with no match, keep with NA CIK

result <- tier1 %>%
  mutate(
    match_method = case_when(
      !is.na(target_cik) ~ "auto_normalized",
      TRUE ~ "NO_MATCH"
    ),
    needs_manual_review = is.na(target_cik) | match_method == "auto_normalized"
  )

# Summary
n_total <- n_distinct(result$target_name_clean)
n_matched <- n_distinct(result$target_name_clean[!is.na(result$target_cik)])
n_unmatched <- n_total - n_matched

cat(glue("  → Total unique targets: {n_total}\n"))
cat(glue("  → Matched: {n_matched} ({round(100*n_matched/n_total, 1)}%)\n"))
cat(glue("  → Unmatched (need manual CIK): {n_unmatched}\n"))

# ==============================================================================
# Save for manual review
# ==============================================================================

cat("\nSaving for manual review...\n")

output_path <- file.path(PROJECT_ROOT, "data/interim/deals_cik_auto_matched.xlsx")

# Sort: unmatched first (for easy review), then by target name
result_sorted <- result %>%
  arrange(is.na(target_cik), target_name_clean)

write_xlsx(result_sorted, output_path)
cat(glue("  → Saved: {output_path}\n"))

# Also save list of unmatched for quick reference
unmatched <- result %>%
  filter(is.na(target_cik)) %>%
  distinct(target_name_clean) %>%
  arrange(target_name_clean)

unmatched_path <- file.path(PROJECT_ROOT, "data/interim/targets_need_manual_cik.csv")
write_csv(unmatched, unmatched_path)
cat(glue("  → Unmatched list: {unmatched_path}\n"))

# ==============================================================================
# Instructions
# ==============================================================================

cat("\n")
cat("==============================================================================\n")
cat("NEXT STEPS\n")
cat("==============================================================================\n")
cat("\n")
cat("1. Open: data/interim/deals_cik_auto_matched.xlsx\n")
cat("2. Review and correct CIK matches (especially for large deals)\n")
cat("3. Add CIK manually for unmatched targets:\n")
cat(glue("   - See: data/interim/targets_need_manual_cik.csv ({nrow(unmatched)} targets)\n"))
cat("   - Search SEC EDGAR: https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany\n")
cat("4. Save as: data/interim/deals_with_cik_matches.xlsx\n")
cat("5. Run the pipeline: source('scripts/01_ingest_clean_deals.R')\n")
cat("\n")
