# ==============================================================================
# MODULE 7: PROCESSING COSTS — READABILITY & VOLUME (v4)
# ==============================================================================
# Purpose: Compute processing-cost proxies (readability, disclosure volume)
#          as defined in the research design's fourth construct.
#
# Scope (strict):
#   This module adds ONLY what is NOT already in Modules 1–3 v6:
#   - Gunning Fog Index (standard implementation)
#   - Flesch-Kincaid Grade Level
#   - log(word count) as volume proxy
#   - Risk Factors readability + volume (parallel measures)
#
#   It does NOT compute tone, forward-looking, risk transparency, modal
#   decomposition, or any other index already in Module 3 v6.
#
# Design rationale:
#   Processing costs are conceptually distinct from the other three
#   constructs (informational precision, credibility/tone, risk
#   transparency). They capture the effort required to extract information,
#   not the information itself. Keeping them in a separate module:
#   (a) avoids bloating Module 3 with a different measurement logic;
#   (b) makes it easy to include/exclude processing costs in regressions;
#   (c) isolates the known Fog-on-10K measurement issues (Loughran &
#       McDonald 2014) from the core indices.
#
# Caveats (to discuss in thesis):
#   - Fog is inflated on 10-Ks because standard financial terms (e.g.
#     "amortization", "collateralized") count as complex words.
#   - log(length) correlates with firm complexity and regulatory exposure;
#     it is not a pure obfuscation proxy. Always include size/complexity
#     controls in regression.
#
# Inputs:
#   - data/interim/cleaned_text.rds  (Module 1 v3)
#
# Outputs:
#   - data/interim/processing_costs.rds
#   - reports/07_processing_costs_report.md
#
# Integration:
#   Module 6 v6 can left_join processing_costs.rds by deal_id.
#   No other module depends on this output.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(quanteda)
  library(quanteda.textstats)
})

cat("=== MODULE 7: PROCESSING COSTS (v4) ===\n\n")

# ---- Paths -------------------------------------------------------------------
project_root <- Sys.getenv("PROJECT_ROOT", unset = getwd())
path_cleaned <- file.path(project_root, "data", "interim", "cleaned_text.rds")
path_out     <- file.path(project_root, "data", "interim", "processing_costs.rds")
path_report  <- file.path(project_root, "reports", "07_processing_costs_report.md")

dir.create(dirname(path_out),    recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(path_report), recursive = TRUE, showWarnings = FALSE)

stopifnot(file.exists(path_cleaned))

# ---- BLOCK 1: Load cleaned text ---------------------------------------------
cat("BLOCK 1: Loading cleaned text...\n")
cleaned <- readRDS(path_cleaned)
cleaned$deal_id <- as.character(cleaned$deal_id)
cat(sprintf("  - %d documents\n\n", nrow(cleaned)))

# Resolve word counts (prefer v3 alpha; fall back gracefully)
if ("mda_wc_alpha" %in% names(cleaned)) {
  mda_wc  <- cleaned$mda_wc_alpha
  risk_wc <- cleaned$risk_wc_alpha
  wc_type <- "wc_alpha (v3)"
} else if ("mda_word_count_clean" %in% names(cleaned)) {
  mda_wc  <- cleaned$mda_word_count_clean
  risk_wc <- cleaned$risk_word_count_clean
  wc_type <- "word_count_clean (legacy)"
} else {
  stop("No word count columns found in cleaned_text.rds")
}
cat(sprintf("  - Word count source: %s\n\n", wc_type))


# ---- BLOCK 2: Readability via quanteda.textstats ----------------------------
cat("BLOCK 2: Computing readability (quanteda.textstats)...\n")

# v4.1: Use case-preserved text (mda_clean_cased) for readability.
# textstat_readability relies on sentence boundary detection which uses
# capitalisation after periods. Lowercased text breaks this entirely,
# producing Fog values >2000 (entire doc = one sentence).
# If cased columns not available (old Module 01), fall back with warning.

if ("mda_clean_cased" %in% names(cleaned)) {
  mda_text_for_read  <- cleaned$mda_clean_cased
  risk_text_for_read <- cleaned$risk_clean_cased
  cat("  - Using case-preserved text (mda_clean_cased)\n")
} else {
  mda_text_for_read  <- cleaned$mda_clean
  risk_text_for_read <- cleaned$risk_clean
  cat("  - WARNING: mda_clean_cased not found. Using lowercased text.\n")
  cat("  - Fog/FK values will be unreliable. Re-run Module 01 v3.1.\n")
}

make_safe_corpus <- function(texts, ids, suffix) {
  texts <- ifelse(is.na(texts) | nchar(texts) < 100, "", texts)
  corpus(texts, docnames = paste0("deal_", ids, "_", suffix))
}

corpus_mda  <- make_safe_corpus(mda_text_for_read,  cleaned$deal_id, "mda")
corpus_risk <- make_safe_corpus(risk_text_for_read, cleaned$deal_id, "risk")

mda_read <- tryCatch(
  textstat_readability(corpus_mda, measure = c("Flesch.Kincaid", "FOG")),
  error = function(e) {
    warning("textstat_readability failed on MD&A: ", e$message)
    data.frame(document = docnames(corpus_mda),
               Flesch.Kincaid = NA_real_, FOG = NA_real_)
  }
)

risk_read <- tryCatch(
  textstat_readability(corpus_risk, measure = c("Flesch.Kincaid", "FOG")),
  error = function(e) {
    warning("textstat_readability failed on Risk: ", e$message)
    data.frame(document = docnames(corpus_risk),
               Flesch.Kincaid = NA_real_, FOG = NA_real_)
  }
)

cat(sprintf("  - MD&A  Fog: mean=%.2f | sd=%.2f | NAs=%d\n",
            mean(mda_read$FOG, na.rm = TRUE),
            sd(mda_read$FOG, na.rm = TRUE),
            sum(is.na(mda_read$FOG))))
cat(sprintf("  - MD&A  FK:  mean=%.2f | sd=%.2f\n",
            mean(mda_read$Flesch.Kincaid, na.rm = TRUE),
            sd(mda_read$Flesch.Kincaid, na.rm = TRUE)))
cat(sprintf("  - Risk  Fog: mean=%.2f | sd=%.2f\n",
            mean(risk_read$FOG, na.rm = TRUE),
            sd(risk_read$FOG, na.rm = TRUE)))
cat(sprintf("  - Risk  FK:  mean=%.2f | sd=%.2f\n\n",
            mean(risk_read$Flesch.Kincaid, na.rm = TRUE),
            sd(risk_read$Flesch.Kincaid, na.rm = TRUE)))

# Sanity check: Fog on English prose should be 10–25; values >50 are suspect
fog_mean <- mean(mda_read$FOG, na.rm = TRUE)
if (!is.na(fog_mean) && fog_mean > 50) {
  cat("  *** SANITY WARNING: Fog mean > 50 suggests broken sentence detection.\n")
  cat("  *** Most likely cause: lowercased input (sentence splitter needs capitals).\n")
  cat("  *** Ensure Module 01 v3.1 has been run to produce mda_clean_cased.\n\n")
} else if (!is.na(fog_mean)) {
  cat(sprintf("  Sanity OK: Fog mean %.1f is in plausible range.\n\n", fog_mean))
}


# ---- BLOCK 3: Volume --------------------------------------------------------
cat("BLOCK 3: Computing volume measures...\n")

mda_log_length  <- ifelse(mda_wc > 0, log(mda_wc), NA_real_)
risk_log_length <- ifelse(risk_wc > 0, log(risk_wc), NA_real_)

cat(sprintf("  - MD&A  log(length): mean=%.2f | sd=%.2f\n",
            mean(mda_log_length, na.rm = TRUE),
            sd(mda_log_length, na.rm = TRUE)))
cat(sprintf("  - Risk  log(length): mean=%.2f | sd=%.2f\n\n",
            mean(risk_log_length, na.rm = TRUE),
            sd(risk_log_length, na.rm = TRUE)))


# ---- BLOCK 4: Assemble and save ---------------------------------------------
cat("BLOCK 4: Assembling output...\n")

processing_costs <- tibble(
  deal_id = cleaned$deal_id,

  # Readability — MD&A
  mda_fog = mda_read$FOG,
  mda_fk  = mda_read$Flesch.Kincaid,

  # Readability — Risk Factors
  risk_fog = risk_read$FOG,
  risk_fk  = risk_read$Flesch.Kincaid,

  # Volume
  mda_log_length  = mda_log_length,
  risk_log_length = risk_log_length,

  # Raw word counts (for reference / alternative specifications)
  mda_wc  = mda_wc,
  risk_wc = risk_wc
)

saveRDS(processing_costs, path_out)
cat(sprintf("  - Saved: %s (%d obs × %d vars)\n\n",
            path_out, nrow(processing_costs), ncol(processing_costs)))


# ---- BLOCK 5: Report --------------------------------------------------------
cat("BLOCK 5: Writing report...\n")

report <- c(
  "# Module 7 — Processing Costs Report (v4)", "",
  paste0("Generated: ", Sys.time()),
  sprintf("Documents: %d", nrow(processing_costs)), "",

  "## Scope",
  "This module computes ONLY processing-cost proxies (readability + volume).",
  "All other disclosure indices are in Module 3 v6.", "",

  "## Measures", "",
  "### Readability",
  sprintf("- MD&A Fog Index: mean=%.2f, sd=%.2f",
          mean(processing_costs$mda_fog, na.rm = TRUE),
          sd(processing_costs$mda_fog, na.rm = TRUE)),
  sprintf("- MD&A FK Grade: mean=%.2f, sd=%.2f",
          mean(processing_costs$mda_fk, na.rm = TRUE),
          sd(processing_costs$mda_fk, na.rm = TRUE)),
  sprintf("- Risk Fog Index: mean=%.2f, sd=%.2f",
          mean(processing_costs$risk_fog, na.rm = TRUE),
          sd(processing_costs$risk_fog, na.rm = TRUE)),
  sprintf("- Risk FK Grade: mean=%.2f, sd=%.2f",
          mean(processing_costs$risk_fk, na.rm = TRUE),
          sd(processing_costs$risk_fk, na.rm = TRUE)), "",

  "### Volume",
  sprintf("- MD&A log(length): mean=%.2f, sd=%.2f",
          mean(processing_costs$mda_log_length, na.rm = TRUE),
          sd(processing_costs$mda_log_length, na.rm = TRUE)),
  sprintf("- Risk log(length): mean=%.2f, sd=%.2f",
          mean(processing_costs$risk_log_length, na.rm = TRUE),
          sd(processing_costs$risk_log_length, na.rm = TRUE)), "",

  "## Implementation",
  "- Readability: quanteda.textstats::textstat_readability() (standard, replicable)",
  sprintf("- Word count denominator: %s", wc_type), "",

  "## Caveats (document in thesis)",
  "- Fog is systematically inflated on 10-Ks because routine financial terms",
  "  (amortization, collateralized, etc.) are counted as complex words.",
  "  Reference: Loughran & McDonald (2014), 'Measuring Readability in",
  "  Financial Disclosures', Journal of Finance.",
  "- log(length) correlates with firm size and regulatory complexity. Always",
  "  include size controls (log assets, number of segments) in regressions.",
  "- These are processing-cost proxies, not information-content measures.",
  "  Their expected sign in premium regressions is ambiguous: longer/harder",
  "  disclosures may indicate complexity (negative) or thoroughness (positive).", "",

  "## Integration with Module 6",
  "```r",
  "# In Module 6, add after the main merge:",
  "proc_costs <- readRDS('data/interim/processing_costs.rds')",
  "merged <- merged %>% left_join(proc_costs, by = 'deal_id')",
  "```", "",

  "## Output",
  sprintf("- %s", path_out)
)

writeLines(report, path_report)
cat(sprintf("  - Report: %s\n", path_report))

cat("\n=== MODULE 7 COMPLETE (v4) ===\n")
