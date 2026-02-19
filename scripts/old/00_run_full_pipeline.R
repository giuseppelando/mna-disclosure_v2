# ==============================================================================
# Master Pipeline Script
# Run entire 10-K download and parsing pipeline
# ==============================================================================

library(glue)

# ==============================================================================
# Configuration
# ==============================================================================

cat("\n")
cat("================================================================================\n")
cat("10-K DOWNLOAD AND PARSING PIPELINE\n")
cat("================================================================================\n")
cat("\n")

# User confirmation
cat("This script will run the complete pipeline:\n")
cat("  1. Clean deals and remove missing CIKs\n")
cat("  2. Build EDGAR index (may take 30+ minutes)\n")
cat("  3. Match deals to filings\n")
cat("  4. Download 10-K filings (may take 1+ hours)\n")
cat("  5. Parse MD&A and Risk Factors\n")
cat("  6. Merge into final dataset\n")
cat("\n")

response <- readline(prompt = "Continue? (yes/no): ")

if (tolower(response) != "yes") {
  cat("Pipeline cancelled.\n")
  quit(save = "no")
}

# ==============================================================================
# Run pipeline
# ==============================================================================

start_time <- Sys.time()

cat("\n")
cat("================================================================================\n")
cat("Starting pipeline at:", format(start_time), "\n")
cat("================================================================================\n")

# Step 1
cat("\n--- Running Step 1: Clean Deals ---\n")
source("C:/Users/giuse/Documents/GitHub/mna-disclosure/scripts/01_ingest_clean_deals.R")

# Step 2
cat("\n--- Running Step 2: Build EDGAR Index ---\n")
source("C:/Users/giuse/Documents/GitHub/mna-disclosure/scripts/02_ingest_build_edgar_index.R")

# Step 3
cat("\n--- Running Step 3: Match Deals to Filings ---\n")
source("C:/Users/giuse/Documents/GitHub/mna-disclosure/scripts/03_merge_match_deals_to_filings.R")

# Step 4
cat("\n--- Running Step 4: Download 10-K Filings ---\n")
source("C:/Users/giuse/Documents/GitHub/mna-disclosure/scripts/04_ingest_download_10k_filings.R")

# Step 5
cat("\n--- Running Step 5: Parse Sections ---\n")
source("C:/Users/giuse/Documents/GitHub/mna-disclosure/scripts/05_nlp_parse_sections.R")

# Step 6
cat("\n--- Running Step 6: Merge Final Dataset ---\n")
source("C:/Users/giuse/Documents/GitHub/mna-disclosure/scripts/06_merge_final_dataset.R")

# ==============================================================================
# Completion
# ==============================================================================

end_time <- Sys.time()
duration <- difftime(end_time, start_time, units = "hours")

cat("\n")
cat("================================================================================\n")
cat("PIPELINE COMPLETE\n")
cat("================================================================================\n")
cat(glue("Started:  {format(start_time)}\n"))
cat(glue("Finished: {format(end_time)}\n"))
cat(glue("Duration: {round(duration, 2)} hours\n"))
cat("\n")
cat("Final dataset:\n")
cat("  C:/Users/giuse/Documents/GitHub/mna-disclosure/data/processed/deals_with_10k_text.rds\n")
cat("\n")
cat("Check logs in:\n")
cat("  C:/Users/giuse/Documents/GitHub/mna-disclosure/output/logs/\n")
cat("\n")
