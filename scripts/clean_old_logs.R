# clean_old_logs.R — Keep only final successful run logs
logs_dir <- "C:/Users/giuse/Documents/GitHub/mna-disclosure/logs"

keep <- c(
  # MASTER of final run
  "MASTER_20260219_004416.log",
  
  # Steps 01-04: run manually before the pipeline runner (same session)
  "01_analyze_columns_20260218_231149.log",       # step 01
  "02_ingest_deals_20260218_231151.log",           # step 02
  "03_sample_restrictions_20260218_231151.log",    # step 03
  "04_match_cik_20260218_231152.log",              # step 04
  
  # Steps 04b-19: from MASTER_20260219_004416 run
  "04b_import_cik_20260219_004416.log",
  "01_ingest_clean_deals_20260219_004416.log",     # step 05 internal
  "05_clean_deals_console_20260219_004416.log",    # step 05 console
  "02_ingest_build_edgar_index_20260219_004417.log",  # step 06 internal
  "06_edgar_index_console_20260219_004417.log",       # step 06 console
  "03_merge_match_deals_to_filings_20260219_010138.log",  # step 07 internal
  "07_match_filings_console_20260219_010138.log",         # step 07 console
  "04_ingest_download_10k_filings_sec_api_20260219_010201.log",  # step 08 internal
  "08_filings_manifest_console_20260219_010201.log",             # step 08 console
  "05_nlp_parse_sections_sec_api_20260219_010201.log",   # step 09 internal
  "09_parse_sections_console_20260219_010201.log",       # step 09 console
  "06_merge_final_dataset_20260219_015543.log",          # step 10 internal
  "10_merge_deals_text_console_20260219_015543.log",     # step 10 console
  "11_text_cleaning_20260219_015621.log",
  "12_tokenization_dtm_20260219_015929.log",
  "13_construct_indices_20260219_020503.log",
  "14_kwic_validation_20260219_020901.log",
  "15_descriptive_analysis_20260219_020927.log",
  "16_processing_costs_20260219_020931.log",
  "17_assemble_dataset_20260219_021259.log",
  "18_prepare_econ_20260219_021326.log",
  "19_run_models_20260219_021432.log",
  
  # Static
  "README.md"
)

all_files <- list.files(logs_dir)
to_delete <- setdiff(all_files, keep)

cat("=== LOG CLEANUP ===\n")
cat("Total:", length(all_files), "| Keep:", length(intersect(all_files, keep)),
    "| Delete:", length(to_delete), "\n\n")

cat("DELETING:\n")
for (f in to_delete) {
  ok <- file.remove(file.path(logs_dir, f))
  cat(ifelse(ok, "  x", "  !"), f, "\n")
}

cat("\nREMAINING:\n")
for (f in list.files(logs_dir)) cat("  ✓", f, "\n")
