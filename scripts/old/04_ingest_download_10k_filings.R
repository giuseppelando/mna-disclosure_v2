# ==============================================================================
# Step 4: Download 10-K Filings
# Download matched filings from EDGAR with rate limiting and caching
# ==============================================================================

library(tidyverse)
library(glue)

# Detect project root
if (!exists("PROJECT_ROOT")) {
  current_dir <- getwd()
  if (dir.exists(file.path(current_dir, "config")) && 
      dir.exists(file.path(current_dir, "src"))) {
    PROJECT_ROOT <- current_dir
  } else {
    PROJECT_ROOT <- dirname(current_dir)
  }
}

# Load configuration and utilities
source(file.path(PROJECT_ROOT, "config/pipeline_config.R"))
source(file.path(PROJECT_ROOT, "src/99_utils/utils.R"))
source(file.path(PROJECT_ROOT, "src/99_utils/edgar_utils.R"))
source(file.path(PROJECT_ROOT, "src/30_nlp/05_nlp_parse_sections.R"))

# Initialize logging
log_file <- init_logging(PATHS$log_dir, "04_ingest_download_10k_filings")
log_section("STEP 4: DOWNLOAD 10-K FILINGS", log_file)

# ==============================================================================
# Load matched deals
# ==============================================================================

log_message("Loading matched deals...", log_file)
deals_filing_matched <- readRDS(PATHS$deals_filing_matched)

matched_deals <- deals_filing_matched %>%
  filter(match_status == "matched")

log_message(glue("  → Total deals: {nrow(deals_filing_matched)}"), log_file)
log_message(glue("  → Matched deals: {nrow(matched_deals)}"), log_file)

# Get unique filings to download
filings_to_download <- matched_deals %>%
  select(accession_number, filing_url, matched_cik, filing_date) %>%
  distinct() %>%
  arrange(filing_date)

log_message(glue("  → Unique filings to download: {nrow(filings_to_download)}"), log_file)

# ==============================================================================
# Setup download directory
# ==============================================================================

log_message("\nSetting up download directory...", log_file)
ensure_dir(PATHS$raw_10k_dir, log_file)

# Check for already downloaded files
existing_files <- list.files(PATHS$raw_10k_dir, pattern = "\\.txt$", full.names = FALSE)
existing_accessions <- str_remove(existing_files, "\\.txt$")

filings_to_download <- filings_to_download %>%
  mutate(
    already_downloaded = accession_number %in% existing_accessions,
    file_path = file.path(PATHS$raw_10k_dir, paste0(accession_number, ".txt"))
  )

n_already <- sum(filings_to_download$already_downloaded)
n_to_download <- sum(!filings_to_download$already_downloaded)

log_message(glue("  → Already downloaded: {n_already}"), log_file)
log_message(glue("  → Need to download: {n_to_download}"), log_file)

# ==============================================================================
# Download filings
# ==============================================================================

if (n_to_download > 0) {
  log_message(glue("\nDownloading {n_to_download} filings..."), log_file)
  log_message(glue("Rate limit: {EDGAR$rate_limit_per_second} requests/second"), log_file)
  
  # Estimate time
  est_minutes <- (n_to_download / EDGAR$rate_limit_per_second) / 60
  log_message(glue("Estimated time: ~{round(est_minutes, 1)} minutes\n"), log_file)
  
  # Create rate limiter
  rate_limiter <- create_rate_limiter(EDGAR$rate_limit_per_second)
  
  # Create progress bar
  progress <- create_progress(n_to_download, "Downloading")
  
  # Download counter
  download_count <- 0
  failed_downloads <- c()
  
  # Download each filing
  for (i in 1:nrow(filings_to_download)) {
    
    # Skip if already downloaded
    if (filings_to_download$already_downloaded[i]) {
      next
    }
    
    accession <- filings_to_download$accession_number[i]
    url <- filings_to_download$filing_url[i]
    file_path <- filings_to_download$file_path[i]
    
    # Apply rate limit
    rate_limiter()
    
    # Download with retry
    content <- retry_on_error(
      download_filing(url, EDGAR$user_agent),
      max_retries = EDGAR$max_retries,
      delay_seconds = EDGAR$retry_delay_seconds,
      log_file = log_file
    )
    
    if (!is.null(content)) {
      # Save to file
      writeLines(content, file_path)
      download_count <- download_count + 1
    } else {
      failed_downloads <- c(failed_downloads, accession)
      log_message(glue("Failed to download: {accession}"), log_file, "WARNING")
    }
    
    # Update progress
    progress$update(download_count)
    
    # Periodic checkpoint
    if (download_count %% 100 == 0) {
      log_message(
        glue("\nCheckpoint: Downloaded {download_count}/{n_to_download} filings"),
        log_file
      )
    }
  }
  
  progress$close()
  
  log_message("\nDownload complete:", log_file)
  log_message(glue("  → Successfully downloaded: {download_count}"), log_file)
  log_message(glue("  → Failed: {length(failed_downloads)}"), log_file)
  
  if (length(failed_downloads) > 0) {
    failed_path <- file.path(PATHS$log_dir, "failed_downloads.txt")
    writeLines(failed_downloads, failed_path)
    log_message(glue("  → Failed accessions saved to: {failed_path}"), log_file, "WARNING")
  }
  
} else {
  log_message("\nNo new filings to download. All filings already cached.", log_file)
}

# ==============================================================================
# Verify downloads
# ==============================================================================

log_message("\nVerifying downloads...", log_file)

# Check which files exist and are non-empty
file_status <- filings_to_download %>%
  mutate(
    file_exists = file.exists(file_path),
    file_size = ifelse(file_exists, file.size(file_path), 0)
  )

n_exists <- sum(file_status$file_exists)
n_missing <- sum(!file_status$file_exists)
avg_size_mb <- mean(file_status$file_size[file_status$file_exists]) / 1024 / 1024

log_message(glue("  → Files exist: {n_exists} / {nrow(filings_to_download)}"), log_file)
log_message(glue("  → Missing: {n_missing}"), log_file)
log_message(glue("  → Average file size: {round(avg_size_mb, 2)} MB"), log_file)

# Save file status
file_status_path <- file.path(
  dirname(PATHS$deals_filing_matched),
  "download_status.rds"
)
saveRDS(file_status, file_status_path)
log_message(glue("  → Download status saved: {file_status_path}"), log_file)

# ==============================================================================
# Summary
# ==============================================================================

log_section("STEP 4 COMPLETE", log_file)
log_message(glue("Downloaded filings: {n_exists} / {nrow(filings_to_download)}"), log_file)
log_message(glue("Storage location: {PATHS$raw_10k_dir}"), log_file)
log_message("\nNext step: Run 05_nlp_parse_sections.R", log_file)
