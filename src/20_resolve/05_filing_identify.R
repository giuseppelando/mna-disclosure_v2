# =============================================================================
# src/20_resolve/05_filing_identify.R
# =============================================================================
# STEP 3: Identify Pre-Announcement 10-K Filings
#
# For each deal with CIK, identifies the most recent 10-K filed before
# the announcement date, subject to research design timing constraints.
#
# Research Design Constraints:
#   - Filing must be BEFORE announcement date
#   - Minimum lag (default 30 days) to avoid contamination
#   - Prefer 10-K over 10-K/A (amended)
#   - Fallback to prior fiscal year if no qualifying filing
#
# Input:  data/interim/deals_with_cik.rds
# Output: data/interim/deals_with_filing.rds
#         data/interim/filing_identify_log.txt
#         data/interim/filing_selection_details.csv
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(glue)
  library(jsonlite)
  library(httr)
  library(lubridate)
})

# =============================================================================
# CONFIGURATION
# =============================================================================

CONFIG <- list(
  # SEC API
  sec_user_agent = "Giuseppe Lando <giuseppe.lando@studbocconi.it> ; M&A Disclosure Thesis",
  sec_submissions_url = "https://data.sec.gov/submissions/CIK{cik}.json",
  rate_limit_delay = 0.15,
  
  # Filing selection parameters
  min_lag_days = 30,
  max_lookback_days = 450,
  
  # Form types (priority order)
  target_forms = c("10-K", "10-K/A"),
  
  # Prefer original over amended
 prefer_original = TRUE,
  
  # Cache settings
  cache_dir = "data/interim/submissions_cache",
  
  # Fallback behavior
  allow_prior_year_fallback = TRUE
)

# =============================================================================
# SETUP
# =============================================================================

cat("\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("STEP 3: IDENTIFY PRE-ANNOUNCEMENT FILINGS\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("\n")

dir.create(CONFIG$cache_dir, recursive = TRUE, showWarnings = FALSE)

log_file <- "data/interim/filing_identify_log.txt"
log_conn <- file(log_file, "w")

log_msg <- function(msg, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  full_msg <- glue("[{timestamp}] [{level}] {msg}")
  writeLines(full_msg, log_conn)
  cat(full_msg, "\n")
}

log_msg("Filing identification process started")
log_msg(glue("Min lag: {CONFIG$min_lag_days} days"))
log_msg(glue("Max lookback: {CONFIG$max_lookback_days} days"))
log_msg(glue("Target forms: {paste(CONFIG$target_forms, collapse = ', ')}"))

# =============================================================================
# LOAD INPUT DATA
# =============================================================================

input_file <- "data/interim/deals_with_cik.rds"

if (!file.exists(input_file)) {
  log_msg("ERROR: Input file not found!", "ERROR")
  close(log_conn)
  stop("Missing input: ", input_file)
}

deals <- readRDS(input_file)

# Ensure date_announced is Date type
if (!inherits(deals$date_announced, "Date")) {
  deals$date_announced <- as.Date(deals$date_announced)
  log_msg("Converted date_announced to Date type")
}

n_total <- nrow(deals)
n_with_cik <- sum(!is.na(deals$target_cik) & deals$target_cik != "")

log_msg(glue("Loaded {n_total} deals ({n_with_cik} with CIK)"))

# Debug: show sample dates
sample_dates <- deals %>% 
  filter(!is.na(target_cik)) %>% 
  slice_head(n = 5) %>% 
  select(target_name, date_announced, target_cik)
log_msg("Sample deals:")
for (i in 1:nrow(sample_dates)) {
  log_msg(glue("  {sample_dates$target_name[i]}: announced {sample_dates$date_announced[i]}, CIK {sample_dates$target_cik[i]}"))
}

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

format_cik_api <- function(cik) {
  if (is.na(cik) || cik == "") return(NA_character_)
  cik_num <- suppressWarnings(as.numeric(str_replace_all(as.character(cik), "^0+", "")))
  if (is.na(cik_num)) return(NA_character_)
  sprintf("%010d", cik_num)
}

get_sec_submissions <- function(cik) {
  cik_formatted <- format_cik_api(cik)
  if (is.na(cik_formatted)) return(NULL)
  
  cache_file <- file.path(CONFIG$cache_dir, glue("{cik_formatted}.json"))
  
  if (file.exists(cache_file)) {
    cache_age <- difftime(Sys.time(), file.info(cache_file)$mtime, units = "days")
    if (cache_age < 30) {
      tryCatch({
        return(fromJSON(cache_file, simplifyVector = TRUE))
      }, error = function(e) NULL)
    }
  }
  
  url <- str_replace(CONFIG$sec_submissions_url, "\\{cik\\}", cik_formatted)
  
  tryCatch({
    Sys.sleep(CONFIG$rate_limit_delay)
    
    resp <- GET(
      url,
      add_headers(`User-Agent` = CONFIG$sec_user_agent),
      timeout(30)
    )
    
    if (status_code(resp) == 200) {
      content_text <- content(resp, "text", encoding = "UTF-8")
      writeLines(content_text, cache_file)
      return(fromJSON(content_text, simplifyVector = TRUE))
    } else {
      return(NULL)
    }
  }, error = function(e) {
    return(NULL)
  })
}

extract_filings <- function(submissions_json) {
  if (is.null(submissions_json)) return(tibble())
  
  recent <- submissions_json$filings$recent
  if (is.null(recent) || is.null(recent$form) || length(recent$form) == 0) return(tibble())
  
  tibble(
    form = recent$form,
    filing_date = as.Date(recent$filingDate),
    accession = recent$accessionNumber,
    primary_doc = recent$primaryDocument
  )
}

select_filing <- function(filings, announce_date, min_lag, max_lookback, target_forms, prefer_original) {
  # Default return for no filings
  no_result <- list(
    accession = NA_character_,
    filing_date = as.Date(NA),
    form = NA_character_,
    primary_doc = NA_character_,
    days_before = NA_integer_,
    reason = "no_filings_found"
  )
  
  if (is.null(filings) || nrow(filings) == 0) {
    return(no_result)
  }
  
  # Ensure announce_date is Date
  if (!inherits(announce_date, "Date")) {
    announce_date <- as.Date(announce_date)
  }
  
  # Filter to target forms (10-K and 10-K/A)
  filings <- filings %>%
    filter(form %in% target_forms)
  
  if (nrow(filings) == 0) {
    return(list(
      accession = NA_character_,
      filing_date = as.Date(NA),
      form = NA_character_,
      primary_doc = NA_character_,
      days_before = NA_integer_,
      reason = "no_10K_forms"
    ))
  }
  
  # Calculate days before announcement
  filings <- filings %>%
    mutate(
      days_before = as.integer(announce_date - filing_date)
    )
  
  # Apply timing constraints: filing must be BEFORE announcement (days_before > 0)
  # and within the lookback window
  qualifying <- filings %>%
    filter(
      days_before >= min_lag,
      days_before <= max_lookback
    )
  
  if (nrow(qualifying) == 0) {
    # Check why: too recent or too old?
    closest <- filings %>% filter(days_before > 0) %>% arrange(days_before) %>% slice_head(n=1)
    if (nrow(closest) > 0) {
      if (closest$days_before < min_lag) {
        return(list(
          accession = NA_character_,
          filing_date = as.Date(NA),
          form = NA_character_,
          primary_doc = NA_character_,
          days_before = NA_integer_,
          reason = glue("too_recent_{closest$days_before}d")
        ))
      } else {
        return(list(
          accession = NA_character_,
          filing_date = as.Date(NA),
          form = NA_character_,
          primary_doc = NA_character_,
          days_before = NA_integer_,
          reason = glue("too_old_{closest$days_before}d")
        ))
      }
    }
    return(list(
      accession = NA_character_,
      filing_date = as.Date(NA),
      form = NA_character_,
      primary_doc = NA_character_,
      days_before = NA_integer_,
      reason = "no_pre_announcement_10K"
    ))
  }
  
  # Sort by preference
  if (prefer_original) {
    qualifying <- qualifying %>%
      mutate(
        is_amended = str_detect(form, "/A$"),
        priority = ifelse(is_amended, 2, 1)
      ) %>%
      arrange(priority, days_before)
  } else {
    qualifying <- qualifying %>%
      arrange(days_before)
  }
  
  best <- qualifying[1, ]
  
  list(
    accession = best$accession,
    filing_date = best$filing_date,
    form = best$form,
    primary_doc = best$primary_doc,
    days_before = best$days_before,
    reason = "selected"
  )
}

build_filing_url <- function(cik, accession, primary_doc) {
  if (any(is.na(c(cik, accession, primary_doc)))) return(NA_character_)
  
  cik_clean <- str_replace_all(as.character(cik), "^0+", "")
  accession_clean <- str_replace_all(accession, "-", "")
  
  glue("https://www.sec.gov/Archives/edgar/data/{cik_clean}/{accession_clean}/{primary_doc}")
}

# =============================================================================
# MAIN PROCESSING LOOP
# =============================================================================

log_msg("")
log_msg("Processing deals with CIK...")
log_msg(paste(rep("-", 50), collapse = ""))

deals_to_process <- deals %>%
  filter(!is.na(target_cik) & target_cik != "") %>%
  select(deal_id, target_cik, target_name, date_announced)

n_to_process <- nrow(deals_to_process)
log_msg(glue("Processing {n_to_process} deals"))

selection_details <- list()
n_found <- 0
n_not_found <- 0

progress_interval <- max(1, floor(n_to_process / 20))

for (i in seq_len(n_to_process)) {
  deal <- deals_to_process[i, ]
  
  if (i %% progress_interval == 0 || i == n_to_process) {
    pct <- round(100 * i / n_to_process)
    log_msg(glue("Progress: {i}/{n_to_process} ({pct}%) - Found: {n_found}, Missing: {n_not_found}"), "DEBUG")
  }
  
  submissions <- get_sec_submissions(deal$target_cik)
  
  if (is.null(submissions)) {
    selection_details[[i]] <- tibble(
      deal_id = deal$deal_id,
      target_cik = deal$target_cik,
      target_name = deal$target_name,
      announce_date = deal$date_announced,
      filing_accession = NA_character_,
      filing_date = as.Date(NA),
      filing_form = NA_character_,
      filing_primary_doc = NA_character_,
      filing_url = NA_character_,
      days_before = NA_integer_,
      selection_reason = "api_error_or_invalid_cik"
    )
    n_not_found <- n_not_found + 1
    next
  }
  
  filings <- extract_filings(submissions)
  
  result <- select_filing(
    filings,
    announce_date = deal$date_announced,
    min_lag = CONFIG$min_lag_days,
    max_lookback = CONFIG$max_lookback_days,
    target_forms = CONFIG$target_forms,
    prefer_original = CONFIG$prefer_original
  )
  
  filing_url <- build_filing_url(deal$target_cik, result$accession, result$primary_doc)
  
  selection_details[[i]] <- tibble(
    deal_id = deal$deal_id,
    target_cik = deal$target_cik,
    target_name = deal$target_name,
    announce_date = deal$date_announced,
    filing_accession = result$accession,
    filing_date = result$filing_date,
    filing_form = result$form,
    filing_primary_doc = result$primary_doc,
    filing_url = filing_url,
    days_before = result$days_before,
    selection_reason = result$reason
  )
  
  if (!is.na(result$accession)) {
    n_found <- n_found + 1
  } else {
    n_not_found <- n_not_found + 1
  }
}

selection_df <- bind_rows(selection_details)

# =============================================================================
# UPDATE MAIN DATASET
# =============================================================================

log_msg("")
log_msg("Updating main dataset...")

# Join filing info back to deals
deals_final <- deals %>%
  left_join(
    selection_df %>%
      select(deal_id, filing_accession, filing_date, filing_form,
             filing_primary_doc, filing_url, days_before, selection_reason),
    by = "deal_id"
  )

# =============================================================================
# SUMMARY
# =============================================================================

log_msg("")
log_msg(paste(rep("=", 70), collapse = ""))
log_msg("FILING IDENTIFICATION SUMMARY")
log_msg(paste(rep("=", 70), collapse = ""))

n_with_filing <- sum(!is.na(selection_df$filing_accession))
filing_rate <- round(100 * n_with_filing / n_to_process, 1)

log_msg(glue("Deals processed: {n_to_process}"))
log_msg(glue("Filings found: {n_with_filing} ({filing_rate}%)"))
log_msg(glue("Filings not found: {n_to_process - n_with_filing} ({round(100 - filing_rate, 1)}%)"))

reason_summary <- selection_df %>%
  count(selection_reason) %>%
  arrange(desc(n))

log_msg("")
log_msg("Selection reasons:")
for (i in seq_len(nrow(reason_summary))) {
  row <- reason_summary[i, ]
  pct <- round(100 * row$n / n_to_process, 1)
  log_msg(glue("  {row$selection_reason}: {row$n} ({pct}%)"))
}

if (n_with_filing > 0) {
  lag_stats <- selection_df %>%
    filter(!is.na(days_before)) %>%
    summarise(
      median_lag = median(days_before),
      mean_lag = round(mean(days_before), 1),
      min_lag = min(days_before),
      max_lag = max(days_before)
    )
  
  log_msg("")
  log_msg("Filing lag statistics (days before announcement):")
  log_msg(glue("  Median: {lag_stats$median_lag}"))
  log_msg(glue("  Mean: {lag_stats$mean_lag}"))
  log_msg(glue("  Range: {lag_stats$min_lag} - {lag_stats$max_lag}"))
}

# =============================================================================
# SAVE OUTPUTS
# =============================================================================

output_file <- "data/interim/deals_with_filing.rds"
saveRDS(deals_final, output_file)
log_msg(glue("Saved: {output_file}"))

write.csv(selection_df, "data/interim/filing_selection_details.csv", row.names = FALSE)
log_msg("Saved: data/interim/filing_selection_details.csv")

close(log_conn)

# Console summary
cat("\n")
cat("╔══════════════════════════════════════════════════════════════════╗\n")
cat("║              FILING IDENTIFICATION COMPLETE                      ║\n")
cat("╠══════════════════════════════════════════════════════════════════╣\n")
cat(sprintf("║ Deals processed:   %-46d ║\n", n_to_process))
cat(sprintf("║ Filings found:     %-46s ║\n", glue("{n_with_filing} ({filing_rate}%)")))
cat("╠══════════════════════════════════════════════════════════════════╣\n")
cat("║ Output: data/interim/deals_with_filing.rds                       ║\n")
cat("╚══════════════════════════════════════════════════════════════════╝\n")
cat("\n")
