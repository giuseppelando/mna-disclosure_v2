# ==============================================================================
# build_pipeline.R  (CORRECTED — includes all scripts/ orchestrators)
# ==============================================================================
# Run ONCE from the project root to create the pipeline/ folder.
#
# The CORRECT execution order has two phases:
#   Phase A (src/): Raw SDC → ingested → restricted → CIK matched → Excel
#   Phase B (scripts/): Excel → clean → EDGAR index → match → manifest →
#                        SEC-API parse → merge → NLP → econometrics
#
# Usage:
#   setwd("C:/Users/giuse/Documents/GitHub/mna-disclosure")
#   source("scripts/build_pipeline.R")
# ==============================================================================

PROJECT_ROOT <- "C:/Users/giuse/Documents/GitHub/mna-disclosure"
setwd(PROJECT_ROOT)

PIPELINE_DIR <- file.path(PROJECT_ROOT, "pipeline")

# Clean previous build
if (dir.exists(PIPELINE_DIR)) {
  old_files <- list.files(PIPELINE_DIR, pattern = "\\.R$", full.names = TRUE)
  file.remove(old_files)
  cat(sprintf("Cleaned %d old pipeline scripts\n", length(old_files)))
}
dir.create(PIPELINE_DIR, showWarnings = FALSE)

cat("\n")
cat(strrep("=", 70), "\n")
cat("BUILDING PIPELINE FOLDER (CORRECTED)\n")
cat(strrep("=", 70), "\n\n")

# ==============================================================================
# COMPLETE step definitions — correct execution order
# ==============================================================================

steps <- list(
  # --- PHASE A: SDC → CIK matching → Excel ---
  list(id="01",  src="src/10_ingest/01_analyze_columns.R",
       dst="01_analyze_columns.R",          log="01_analyze_columns",      init=FALSE),
  list(id="02",  src="src/10_ingest/02_ingest_deals_final.R",
       dst="02_ingest_deals.R",             log="02_ingest_deals",         init=FALSE),
  list(id="03",  src="src/20_clean/03_apply_sample_restrictions.R",
       dst="03_sample_restrictions.R",      log="03_sample_restrictions",  init=FALSE),
  list(id="04",  src="src/20_resolve/04_match_target_cik.R",
       dst="04_match_target_cik.R",         log="04_match_cik",            init=FALSE),
  # ⏸ MANUAL: review deals_with_cik_matches.xlsx in Excel
  list(id="04b", src="src/20_resolve/04b_import_cik_matches.R",
       dst="04b_import_cik_matches.R",      log="04b_import_cik",          init=FALSE),

  # --- PHASE B: Curated Excel → EDGAR → SEC-API → NLP → Econometrics ---
  list(id="05",  src="scripts/01_ingest_clean_deals.R",
       dst="05_clean_deals_for_matching.R", log="05_clean_deals",          init=TRUE),
  list(id="06",  src="scripts/02_ingest_build_edgar_index.R",
       dst="06_build_edgar_index.R",        log="06_edgar_index",          init=TRUE),
  list(id="07",  src="scripts/03_merge_match_deals_to_filings.R",
       dst="07_match_deals_to_filings.R",   log="07_match_filings",        init=TRUE),
  list(id="08",  src="scripts/04_ingest_download_10k_filings.R",
       dst="08_prepare_filings_manifest.R", log="08_filings_manifest",     init=TRUE),
  list(id="09",  src="scripts/05_nlp_parse_sections.R",
       dst="09_parse_sections.R",           log="09_parse_sections",       init=TRUE),
  list(id="10",  src="scripts/06_merge_final_dataset.R",
       dst="10_merge_deals_text.R",         log="10_merge_deals_text",     init=TRUE),

  # --- NLP PIPELINE ---
  list(id="11",  src="src/30_nlp/New Pipeline/01_text_cleaning_v3.R",
       dst="11_text_cleaning.R",            log="11_text_cleaning",        init=FALSE),
  list(id="12",  src="src/30_nlp/New Pipeline/02_tokenization_dtm_v4_1.R",
       dst="12_tokenization_dtm.R",         log="12_tokenization_dtm",     init=FALSE),
  list(id="13",  src="src/30_nlp/New Pipeline/03_construct_indices_v6_1.R",
       dst="13_construct_indices.R",        log="13_construct_indices",    init=FALSE),
  list(id="14",  src="src/30_nlp/New Pipeline/04_kwic_validation_v3.R",
       dst="14_kwic_validation.R",          log="14_kwic_validation",      init=FALSE),
  list(id="15",  src="src/30_nlp/New Pipeline/05_descriptive_analysis_v3.R",
       dst="15_descriptive_analysis.R",     log="15_descriptive_analysis", init=FALSE),
  list(id="16",  src="src/30_nlp/New Pipeline/06_processing_costs_v4.R",
       dst="16_processing_costs.R",         log="16_processing_costs",     init=FALSE),
  list(id="17",  src="src/30_nlp/New Pipeline/07_assemble_analysis_dataset_FINAL.R",
       dst="17_assemble_dataset.R",         log="17_assemble_dataset",     init=FALSE),

  # --- ECONOMETRICS ---
  list(id="18",  src="src/50_models/New Pipeline/08_prepare_econometrics_v7.R",
       dst="18_prepare_econometrics.R",     log="18_prepare_econ",         init=FALSE),
  list(id="19",  src="src/50_models/New Pipeline/09_run_models_v7.R",
       dst="19_run_models.R",               log="19_run_models",           init=FALSE)
)

# ==============================================================================
# Preamble/postamble templates
# ==============================================================================

make_preamble_sink <- function(step) {
  paste0(
    '# ==============================================================================\n',
    '# PIPELINE STEP ', step$id, ': ', step$log, '\n',
    '# Original source: ', step$src, '\n',
    '# ==============================================================================\n',
    '.pipeline_log_dir <- file.path("', PROJECT_ROOT, '", "logs")\n',
    'if (!dir.exists(.pipeline_log_dir)) dir.create(.pipeline_log_dir, recursive = TRUE)\n',
    '.pipeline_log_file <- file.path(\n',
    '  .pipeline_log_dir,\n',
    '  paste0("', step$log, '_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log")\n',
    ')\n',
    '.pipeline_log_conn <- file(.pipeline_log_file, open = "wt")\n',
    'sink(.pipeline_log_conn, split = TRUE)\n',
    'sink(.pipeline_log_conn, type = "message")\n',
    'cat(paste(rep("=", 70), collapse = ""), "\\n")\n',
    'cat("PIPELINE STEP ', step$id, ': ', step$log, '\\n")\n',
    'cat(paste("Started:", Sys.time()), "\\n")\n',
    'cat(paste("Source:", "', step$src, '"), "\\n")\n',
    'cat(paste(rep("=", 70), collapse = ""), "\\n\\n")\n',
    '\n',
    '# --- ORIGINAL CODE BEGINS ---\n',
    '\n'
  )
}

postamble_sink <- paste0(
  '\n',
  '# --- ORIGINAL CODE ENDS ---\n',
  '\n',
  'cat("\\n")\n',
  'cat(paste(rep("=", 70), collapse = ""), "\\n")\n',
  'cat(paste("Completed:", Sys.time()), "\\n")\n',
  'cat(paste("Log saved:", .pipeline_log_file), "\\n")\n',
  'cat(paste(rep("=", 70), collapse = ""), "\\n")\n',
  'sink(type = "message")\n',
  'sink()\n',
  'close(.pipeline_log_conn)\n',
  'message(paste("Log saved:", .pipeline_log_file))\n'
)

make_preamble_init <- function(step) {
  paste0(
    '# ==============================================================================\n',
    '# PIPELINE STEP ', step$id, ': ', step$log, '\n',
    '# Original source: ', step$src, '\n',
    '# NOTE: PATHS$log_dir is overridden to logs/ after config loads.\n',
    '# ==============================================================================\n',
    '.pipeline_log_dir_override <- file.path("', PROJECT_ROOT, '", "logs")\n',
    'if (!dir.exists(.pipeline_log_dir_override)) dir.create(.pipeline_log_dir_override, recursive = TRUE)\n',
    '.pipeline_sink_file <- file.path(\n',
    '  .pipeline_log_dir_override,\n',
    '  paste0("', step$log, '_console_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log")\n',
    ')\n',
    '.pipeline_sink_conn <- file(.pipeline_sink_file, open = "wt")\n',
    'sink(.pipeline_sink_conn, split = TRUE)\n',
    'sink(.pipeline_sink_conn, type = "message")\n',
    '\n',
    '# --- ORIGINAL CODE BEGINS ---\n',
    '\n'
  )
}

postamble_init <- paste0(
  '\n',
  '# --- ORIGINAL CODE ENDS ---\n',
  '\n',
  'sink(type = "message")\n',
  'sink()\n',
  'close(.pipeline_sink_conn)\n',
  'message(paste("Console log saved:", .pipeline_sink_file))\n'
)

# ==============================================================================
# Patch: override PATHS$log_dir for init_logging scripts
# ==============================================================================

patch_log_dir <- function(code_lines) {
  config_idx <- grep("pipeline_config\\.R", code_lines)
  if (length(config_idx) > 0) {
    insert_at <- max(config_idx)
    override_line <- 'PATHS$log_dir <- .pipeline_log_dir_override  # PIPELINE OVERRIDE'
    code_lines <- append(code_lines, override_line, after = insert_at)
  }
  return(code_lines)
}

# ==============================================================================
# Build each pipeline script
# ==============================================================================

built <- 0
failed <- 0

for (step in steps) {
  src_path <- file.path(PROJECT_ROOT, step$src)
  dst_path <- file.path(PIPELINE_DIR, step$dst)
  
  if (!file.exists(src_path)) {
    cat(sprintf("  ✗ MISSING: %s\n", step$src))
    failed <- failed + 1
    next
  }
  
  code_lines <- readLines(src_path, warn = FALSE)
  
  if (step$init) {
    preamble <- make_preamble_init(step)
    postamble <- postamble_init
    code_lines <- patch_log_dir(code_lines)
  } else {
    preamble <- make_preamble_sink(step)
    postamble <- postamble_sink
  }
  
  full_content <- paste0(
    preamble,
    paste(code_lines, collapse = "\n"),
    postamble
  )
  
  writeLines(full_content, dst_path)
  
  n_lines <- length(code_lines)
  cat(sprintf("  ✓ [%3s] %-40s ← %s (%d lines)\n",
              step$id, step$dst, basename(step$src), n_lines))
  built <- built + 1
}

# ==============================================================================
# Create README
# ==============================================================================

readme_lines <- c(
  "# Pipeline Execution Order (CORRECTED)",
  "",
  paste("Generated:", Sys.time()),
  "",
  "## How to run",
  "",
  "```r",
  'setwd("C:/Users/giuse/Documents/GitHub/mna-disclosure")',
  'source("pipeline/run_pipeline.R")',
  "```",
  "",
  "## Full step sequence",
  "",
  "| Step | Script | Original Source | Phase |",
  "|------|--------|-----------------|-------|"
)

phases <- c(
  "01"="A: SDC Ingest", "02"="A: SDC Ingest", "03"="A: Sample Restrict",
  "04"="A: CIK Match", "04b"="A: CIK Import",
  "05"="B: Clean for EDGAR", "06"="B: EDGAR Index",
  "07"="B: Deal-Filing Match", "08"="B: SEC-API Manifest",
  "09"="B: Parse Sections", "10"="B: Merge Text",
  "11"="C: NLP", "12"="C: NLP", "13"="C: NLP",
  "14"="C: NLP", "15"="C: NLP", "16"="C: NLP", "17"="C: NLP",
  "18"="D: Econometrics", "19"="D: Econometrics"
)

for (step in steps) {
  ph <- phases[[step$id]]
  readme_lines <- c(readme_lines,
    sprintf("| %s | `%s` | `%s` | %s |", step$id, step$dst, step$src, ph))
}

readme_lines <- c(readme_lines, "",
  "## Phase descriptions",
  "",
  "**Phase A (steps 01–04b):** Raw SDC data → ingest → sample restrictions → CIK matching → Excel for manual curation.",
  "",
  "**Phase B (steps 05–10):** Curated Excel → remove no-CIK → build EDGAR index → match deals to filings (creates `match_status`) → prepare SEC-API manifest → extract MD&A + Risk Factors → merge text with deals.",
  "",
  "**Phase C (steps 11–17):** Text cleaning → tokenization → index construction → KWIC validation → descriptive analysis → processing costs → assemble final dataset.",
  "",
  "**Phase D (steps 18–19):** Prepare econometric variables → run models (OLS, logit, Cox).",
  "",
  "## Important notes",
  "",
  "- Step 04 produces an Excel file for manual CIK curation. After reviewing, run step 04b.",
  "- Step 06 queries the EDGAR Submissions API (uses submissions_cache/).",
  "- Step 09 calls the SEC-API Extractor (external, rate-limited).",
  "- Steps 11–19 are fully local computation."
)

writeLines(readme_lines, file.path(PIPELINE_DIR, "README.md"))

cat("\n")
cat(strrep("=", 70), "\n")
cat(sprintf("PIPELINE BUILD COMPLETE: %d scripts built, %d failed\n", built, failed))
cat(sprintf("Location: %s\n", PIPELINE_DIR))
cat(strrep("=", 70), "\n")
