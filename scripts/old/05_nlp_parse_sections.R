# ==============================================================================
# Step 5: Parse Sections from 10-K Filings
# Extract MD&A and Risk Factors from downloaded filings
# ==============================================================================

library(tidyverse)
library(glue)


library(tidyverse)
library(glue)

# ==============================================================================
# Project root discovery (portable)
# ==============================================================================

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

# Load configuration and utilities (relative, portable)
source(file.path(ROOT, "config", "pipeline_config.R"))
source(file.path(ROOT, "src", "99_utils", "utils.R"))
source(file.path(ROOT, "src", "30_nlp", "section_parsers.R"))

# ==============================================================================
# Flexible RDS resolver
# ==============================================================================

resolve_rds_path <- function(p, root, also_try = character()) {
  p0 <- p
  if (!is.null(p0) && nzchar(p0) && file.exists(p0)) {
    return(normalizePath(p0, winslash = "/", mustWork = TRUE))
  }
  
  bn <- if (!is.null(p0) && nzchar(p0)) basename(p0) else NA_character_
  
  candidates <- c(
    if (!is.na(bn)) file.path(root, "data", "interim", bn),
    if (!is.na(bn)) file.path(root, "data", "processed", bn),
    if (!is.na(bn)) file.path(root, "data", "raw", bn),
    if (!is.na(bn)) file.path(root, bn),
    also_try
  )
  candidates <- unique(candidates[nzchar(candidates)])
  
  hit <- candidates[file.exists(candidates)][1]
  if (length(hit) == 1 && !is.na(hit)) {
    return(normalizePath(hit, winslash = "/", mustWork = TRUE))
  }
  
  tried <- unique(c(p0, candidates))
  tried <- tried[nzchar(tried)]
  stop(glue(
    "Cannot find required RDS file.\nTried:\n - {paste(tried, collapse = '\n - ')}"
  ))
}

# Initialize logging
log_file <- init_logging(PATHS$log_dir, "05_nlp_parse_sections")
log_section("STEP 5: PARSE SECTIONS", log_file)

# ==============================================================================
# Load data
# ==============================================================================

log_message("Loading data...", log_file)
PATHS$deals_filing_matched <- resolve_rds_path(PATHS$deals_filing_matched, ROOT)
deals_filing_matched <- readRDS(PATHS$deals_filing_matched)

matched_deals <- deals_filing_matched %>%
  filter(match_status == "matched")

log_message(glue("  → Matched deals: {nrow(matched_deals)}"), log_file)

# Get unique filings
unique_filings <- matched_deals %>%
  select(accession_number, matched_cik, filing_date) %>%
  distinct() %>%
  mutate(
    # Files are named simply: {accession}.txt
    file_path = file.path(PATHS$raw_10k_dir, paste0(accession_number, ".txt"))
  )

log_message(glue("  → Unique filings to parse: {nrow(unique_filings)}"), log_file)

# ==============================================================================
# Check for already parsed sections
# ==============================================================================

log_message("\nChecking for cached parsed sections...", log_file)

if (file.exists(PATHS$parsed_sections)) {
  parsed_cache <- readRDS(PATHS$parsed_sections)
  log_message(glue("  → Found cached results: {nrow(parsed_cache)} filings"), log_file)
  
  # Filter to filings not yet parsed
  already_parsed <- unique(parsed_cache$accession_number)
  filings_to_parse <- unique_filings %>%
    filter(!accession_number %in% already_parsed)
  
  log_message(glue("  → Already parsed: {length(already_parsed)}"), log_file)
  log_message(glue("  → Need to parse: {nrow(filings_to_parse)}"), log_file)
} else {
  log_message("  → No cached results found. Parsing all filings.", log_file)
  filings_to_parse <- unique_filings
  parsed_cache <- NULL
}

# ==============================================================================
# Parse sections
# ==============================================================================

if (nrow(filings_to_parse) > 0) {
  log_message(glue("\nParsing {nrow(filings_to_parse)} filings..."), log_file)
  log_message("This may take several minutes.\n", log_file)
  
  # Create progress bar
  progress <- create_progress(nrow(filings_to_parse), "Parsing")
  
  # Initialize results list
  parsed_results <- vector("list", nrow(filings_to_parse))
  
  # Parse each filing
  for (i in 1:nrow(filings_to_parse)) {
    accession <- filings_to_parse$accession_number[i]
    file_path <- filings_to_parse$file_path[i]
    
    # Check if file exists
    if (!file.exists(file_path)) {
      parsed_results[[i]] <- list(
        accession_number = accession,
        mda_text = NA_character_,
        risk_factors_text = NA_character_,
        mda_word_count = 0,
        risk_word_count = 0,
        parse_status = "file_missing"
      )
      next
    }
    
    # Read file
    tryCatch({
      filing_text <- readLines(file_path, warn = FALSE) %>%
        paste(collapse = "\n")
      
      # Check file size
      file_chars <- nchar(filing_text)
      if (file_chars < 1000) {
        parsed_results[[i]] <- list(
          accession_number = accession,
          mda_text = NA_character_,
          risk_factors_text = NA_character_,
          mda_word_count = 0,
          risk_word_count = 0,
          parse_status = "file_too_small"
        )
        next
      }
      
      # Parse sections
      parsed <- parse_10k_sections(filing_text)
      
      # Add accession number and file info
      parsed$accession_number = accession
      parsed$file_size_chars = file_chars
      
      parsed_results[[i]] <- parsed
      
    }, error = function(e) {
      log_message(
        glue("Error parsing {accession}: {e$message}"),
        log_file,
        "WARNING"
      )
      parsed_results[[i]] <- list(
        accession_number = accession,
        mda_text = NA_character_,
        risk_factors_text = NA_character_,
        mda_word_count = 0,
        risk_word_count = 0,
        parse_status = "error",
        error_message = e$message
      )
    })
    
    progress$update(i)
    
    # Periodic save
    if (i %% 50 == 0) {
      temp_results <- bind_rows(parsed_results[1:i])
      saveRDS(temp_results, paste0(PATHS$parsed_sections, ".temp"))
    }
  }
  
  progress$close()
  
  # Combine new results with cache
  new_parsed <- bind_rows(parsed_results)
  
  if (!is.null(parsed_cache)) {
    all_parsed <- bind_rows(parsed_cache, new_parsed)
  } else {
    all_parsed <- new_parsed
  }
  
} else {
  log_message("\nNo new filings to parse. Using cached results.", log_file)
  all_parsed <- parsed_cache
}

# ==============================================================================
# Parsing statistics
# ==============================================================================

log_message("\nParsing statistics:", log_file)

parse_summary <- all_parsed %>%
  count(parse_status) %>%
  mutate(pct = round(100 * n / sum(n), 1))

for (i in 1:nrow(parse_summary)) {
  status <- parse_summary$parse_status[i]
  count <- parse_summary$n[i]
  pct <- parse_summary$pct[i]
  log_message(glue("  → {status}: {count} ({pct}%)"), log_file)
}

# Detailed breakdown of failures
failed_parses <- all_parsed %>%
  filter(parse_status != "success")

if (nrow(failed_parses) > 0) {
  log_message(glue("\nFailure breakdown ({nrow(failed_parses)} total failures):"), log_file)
  
  n_missing <- sum(failed_parses$parse_status == "file_missing", na.rm = TRUE)
  if (n_missing > 0) {
    log_message(glue("  → Files not found: {n_missing}"), log_file)
  }
  
  n_too_small <- sum(failed_parses$parse_status == "file_too_small", na.rm = TRUE)
  if (n_too_small > 0) {
    log_message(glue("  → Files too small: {n_too_small}"), log_file)
  }
  
  n_parse_failed <- sum(failed_parses$parse_status == "parse_failed", na.rm = TRUE)
  if (n_parse_failed > 0) {
    log_message(glue("  → Parse failed (sections too short): {n_parse_failed}"), log_file)
  }
  
  n_errors <- sum(failed_parses$parse_status == "error", na.rm = TRUE)
  if (n_errors > 0) {
    log_message(glue("  → Errors during parsing: {n_errors}"), log_file)
  }
}

# Statistics for successful parses
successful_parses <- all_parsed %>%
  filter(parse_status == "success")

if (nrow(successful_parses) > 0) {
  log_message("\nSuccessful parses statistics:", log_file)
  log_message(glue("  → Mean MD&A words: {round(mean(successful_parses$mda_word_count), 0)}"), log_file)
  log_message(glue("  → Median MD&A words: {median(successful_parses$mda_word_count)}"), log_file)
  log_message(glue("  → Mean Risk words: {round(mean(successful_parses$risk_word_count), 0)}"), log_file)
  log_message(glue("  → Median Risk words: {median(successful_parses$risk_word_count)}"), log_file)
}

# ==============================================================================
# Quality checks
# ==============================================================================

log_message("\nQuality checks:", log_file)

# Check for suspiciously short sections
short_mda <- successful_parses %>%
  filter(mda_word_count < VALIDATION$min_mda_words) %>%
  nrow()

short_risk <- successful_parses %>%
  filter(risk_word_count < VALIDATION$min_risk_words) %>%
  nrow()

if (short_mda > 0) {
  log_message(glue("  ⚠ {short_mda} filings have MD&A < {VALIDATION$min_mda_words} words"), log_file, "WARNING")
}

if (short_risk > 0) {
  log_message(glue("  ⚠ {short_risk} filings have Risk Factors < {VALIDATION$min_risk_words} words"), log_file, "WARNING")
}

# Check for suspiciously long sections
long_mda <- successful_parses %>%
  filter(mda_word_count > VALIDATION$max_mda_words) %>%
  nrow()

long_risk <- successful_parses %>%
  filter(risk_word_count > VALIDATION$max_risk_words) %>%
  nrow()

if (long_mda > 0) {
  log_message(glue("  ⚠ {long_mda} filings have MD&A > {VALIDATION$max_mda_words} words (possible parsing error)"), log_file, "WARNING")
}

if (long_risk > 0) {
  log_message(glue("  ⚠ {long_risk} filings have Risk Factors > {VALIDATION$max_risk_words} words (possible parsing error)"), log_file, "WARNING")
}

# ==============================================================================
# Save parsed sections
# ==============================================================================

log_message("\nSaving parsed sections...", log_file)
ensure_dir(dirname(PATHS$parsed_sections), log_file)
safe_save(all_parsed, PATHS$parsed_sections, log_file)

# Clean up temp file
temp_file <- paste0(PATHS$parsed_sections, ".temp")
if (file.exists(temp_file)) {
  file.remove(temp_file)
}

# ==============================================================================
# Create sample for manual inspection
# ==============================================================================

log_message("\nCreating sample for manual inspection...", log_file)

sample_size <- min(10, nrow(successful_parses))
sample_parsed <- successful_parses %>%
  slice_sample(n = sample_size)

sample_path <- file.path(
  dirname(PATHS$parsed_sections),
  "parsing_sample.rds"
)
saveRDS(sample_parsed, sample_path)
log_message(glue("  → Sample saved: {sample_path}"), log_file)
log_message("  → Review this sample to verify parsing quality", log_file)

# ==============================================================================
# Summary
# ==============================================================================

log_section("STEP 5 COMPLETE", log_file)
log_message(glue("Output: {PATHS$parsed_sections}"), log_file)
log_message(glue("Successfully parsed: {nrow(successful_parses)} / {nrow(all_parsed)} ({round(100 * nrow(successful_parses) / nrow(all_parsed), 1)}%)"), log_file)
log_message("\nNext step: Run 06_merge_final_dataset.R", log_file)
