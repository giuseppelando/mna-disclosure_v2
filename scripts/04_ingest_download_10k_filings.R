# ==============================================================================
# 04_ingest_download_10k_filings.R
# SEC-API mode: prepare a filings manifest (no EDGAR downloads)
# ==============================================================================

library(tidyverse)
library(glue)

# ------------------------------------------------------------------------------
# Project root discovery (portable)
# ------------------------------------------------------------------------------
get_start_dir <- function() {
  this_file <- tryCatch(
    normalizePath(sys.frames()[1]$ofile, winslash = "/", mustWork = TRUE),
    error = function(e) NA_character_
  )
  if (!is.na(this_file)) return(dirname(this_file))
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

find_project_root <- function(start_dir) {
  cur <- normalizePath(start_dir, winslash = "/", mustWork = FALSE)
  for (i in 1:30) {
    if (file.exists(file.path(cur, "config", "pipeline_config.R"))) return(cur)
    parent <- normalizePath(file.path(cur, ".."), winslash = "/", mustWork = FALSE)
    if (identical(parent, cur)) break
    cur <- parent
  }
  stop("Project root not found. Set env var MNA_DISCLOSURE_ROOT to repo root.")
}

ROOT <- Sys.getenv("MNA_DISCLOSURE_ROOT", unset = "")
if (nzchar(ROOT)) {
  ROOT <- normalizePath(ROOT, winslash = "/", mustWork = TRUE)
} else {
  ROOT <- find_project_root(get_start_dir())
}

# Load configuration and utilities
source(file.path(ROOT, "config", "pipeline_config.R"))
source(file.path(ROOT, "src", "99_utils", "utils.R"))

# Initialize logging
log_file <- init_logging(PATHS$log_dir, "04_ingest_download_10k_filings_sec_api")
log_section("STEP 4: PREPARE FILINGS MANIFEST (SEC-API)", log_file)

# ------------------------------------------------------------------------------
# Parameters
# ------------------------------------------------------------------------------
MANIFEST_PATH <- file.path(dirname(PATHS$deals_filing_matched), "filings_manifest_sec_api.rds")

# ------------------------------------------------------------------------------
# Load matched deals
# ------------------------------------------------------------------------------
log_message("Loading deals_filing_matched...", log_file)
deals_filing_matched <- readRDS(PATHS$deals_filing_matched)

matched_deals <- deals_filing_matched %>%
  filter(match_status == "matched", !is.na(accession_number), nzchar(accession_number),
         !is.na(filing_url), nzchar(filing_url))

log_message(glue("  → Total deals: {nrow(deals_filing_matched)}"), log_file)
log_message(glue("  → Matched deals with filing_url: {nrow(matched_deals)}"), log_file)

# Unique filings
filings_manifest <- matched_deals %>%
  select(accession_number, matched_cik, filing_date, filing_url) %>%
  distinct() %>%
  arrange(filing_date) %>%
  mutate(
    extraction_mode = "sec_api_extractor",
    prepared_at = Sys.time(),
    manifest_status = "prepared"
  )

log_message(glue("  → Unique filings prepared for SEC-API extraction: {nrow(filings_manifest)}"), log_file)

# ------------------------------------------------------------------------------
# Cache awareness (optional): mark which accessions already in parsed_sections
# ------------------------------------------------------------------------------
if (file.exists(PATHS$parsed_sections)) {
  log_message("Detected parsed_sections cache; computing cache coverage...", log_file)
  parsed_cache <- readRDS(PATHS$parsed_sections)
  cached_accessions <- unique(parsed_cache$accession_number)
  filings_manifest <- filings_manifest %>%
    mutate(is_cached_any = accession_number %in% cached_accessions)
  log_message(glue("  → Cached accessions present: {sum(filings_manifest$is_cached_any)}"), log_file)
} else {
  filings_manifest <- filings_manifest %>% mutate(is_cached_any = FALSE)
  log_message("No parsed_sections cache found.", log_file)
}

# ------------------------------------------------------------------------------
# Save manifest
# ------------------------------------------------------------------------------
log_message("Saving filings_manifest_sec_api...", log_file)
ensure_dir(dirname(MANIFEST_PATH), log_file)
saveRDS(filings_manifest, MANIFEST_PATH)

log_message(glue("  → Saved: {MANIFEST_PATH}"), log_file)
log_section("STEP 4 COMPLETE", log_file)
log_message("Next step: Run 05_nlp_parse_sections.R (SEC-API extraction).", log_file)
