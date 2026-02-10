# ==============================================================================
# 05_nlp_parse_sections.R
# SEC-API extraction of Item 7 (MD&A) and Item 1A (Risk Factors) for full dataset
# ==============================================================================

library(tidyverse)
library(glue)
library(httr)

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

# ------------------------------------------------------------------------------
# SEC-API settings
# ------------------------------------------------------------------------------
SEC_API_TOKEN <- Sys.getenv(
  "SEC_API_TOKEN",
  unset = "6fd95cc2f0046eca8561aa0d50eeadbb15a5171428ff913892d99e28cdad7ed2"
)

SEC_API <- list(
  endpoint = "https://api.sec-api.io/extractor",
  type = "text",
  rate_limit_per_second = as.numeric(Sys.getenv("SEC_API_RATE_LIMIT", unset = "5")),
  max_retries = as.integer(Sys.getenv("SEC_API_MAX_RETRIES", unset = "3")),
  retry_delay_seconds = as.numeric(Sys.getenv("SEC_API_RETRY_DELAY", unset = "2"))
)

# Caching behavior
RETRY_TRANSIENT_FAILURES <- TRUE   # re-try api_failed_* and error
RETRY_TOO_SHORT <- FALSE           # re-try too_short_* (usually structural)

`%||%` <- function(x, y) if (is.null(x)) y else x

# ------------------------------------------------------------------------------
# Paths
# ------------------------------------------------------------------------------
MANIFEST_PATH <- file.path(dirname(PATHS$deals_filing_matched), "filings_manifest_sec_api.rds")

# ------------------------------------------------------------------------------
# Logging
# ------------------------------------------------------------------------------
log_file <- init_logging(PATHS$log_dir, "05_nlp_parse_sections_sec_api")
log_section("STEP 5: PARSE SECTIONS (SEC-API)", log_file)

log_message("SEC-API configuration:", log_file)
log_message(glue("  → endpoint: {SEC_API$endpoint}"), log_file)
log_message(glue("  → rate_limit_per_second: {SEC_API$rate_limit_per_second}"), log_file)
log_message(glue("  → max_retries: {SEC_API$max_retries}"), log_file)

# ------------------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------------------
make_rate_limiter_safe <- function(rps) {
  if (exists("create_rate_limiter")) return(create_rate_limiter(rps))
  last_time <- Sys.time() - 1
  min_interval <- 1 / max(rps, 0.1)
  function() {
    now <- Sys.time()
    elapsed <- as.numeric(difftime(now, last_time, units = "secs"))
    if (elapsed < min_interval) Sys.sleep(min_interval - elapsed)
    last_time <<- Sys.time()
  }
}

create_progress_safe <- function(n, label) {
  if (exists("create_progress")) return(create_progress(n, label))
  pb <- utils::txtProgressBar(min = 0, max = n, style = 3)
  list(
    update = function(i) utils::setTxtProgressBar(pb, i),
    close = function() close(pb)
  )
}

sec_api_get_item <- function(filing_url, item, token, endpoint, type = "text",
                             max_retries = 3, retry_delay_seconds = 2, rate_limiter = NULL) {
  
  last_status <- NA_integer_
  last_body <- NA_character_
  
  for (attempt in seq_len(max_retries)) {
    
    if (!is.null(rate_limiter)) rate_limiter()
    
    resp <- httr::GET(
      endpoint,
      query = list(url = filing_url, item = item, type = type, token = token),
      httr::user_agent("mna-disclosure-sec-api/0.1")
    )
    
    status <- httr::status_code(resp)
    body <- httr::content(resp, as = "text", encoding = "UTF-8")
    
    # success
    if (status == 200) {
      return(list(ok = TRUE, status = status, body = body))
    }
    
    last_status <- status
    last_body <- body
    
    # retry only on transient statuses
    transient <- status %in% c(429, 500, 502, 503, 504)
    if (!transient) break
    
    Sys.sleep(retry_delay_seconds * attempt)
  }
  
  list(ok = FALSE, status = last_status, body = last_body)
}

should_skip_cached <- function(status_vec) {
  # status_vec is parse_status from cache for an accession; use first non-NA
  s <- status_vec[which(!is.na(status_vec))[1]] %||% NA_character_
  if (is.na(s)) return(FALSE)
  if (s == "success") return(TRUE)
  if (!RETRY_TOO_SHORT && s %in% c("too_short_mda", "too_short_risk")) return(TRUE)
  if (!RETRY_TRANSIENT_FAILURES && (str_detect(s, "^sec_api_failed") || s == "error")) return(TRUE)
  FALSE
}

# ------------------------------------------------------------------------------
# Load filings list (manifest if present; else build from deals_filing_matched)
# ------------------------------------------------------------------------------
if (file.exists(MANIFEST_PATH)) {
  log_message(glue("Loading filings manifest: {MANIFEST_PATH}"), log_file)
  filings_manifest <- readRDS(MANIFEST_PATH)
  filings_to_extract <- filings_manifest %>%
    filter(!is.na(accession_number), nzchar(accession_number),
           !is.na(filing_url), nzchar(filing_url)) %>%
    select(accession_number, filing_url, matched_cik, filing_date) %>%
    distinct()
} else {
  log_message("Manifest not found; building filings list from deals_filing_matched...", log_file)
  deals_filing_matched <- readRDS(PATHS$deals_filing_matched)
  filings_to_extract <- deals_filing_matched %>%
    filter(match_status == "matched",
           !is.na(accession_number), nzchar(accession_number),
           !is.na(filing_url), nzchar(filing_url)) %>%
    select(accession_number, filing_url, matched_cik, filing_date) %>%
    distinct()
}

log_message(glue("  → Unique filings to extract: {nrow(filings_to_extract)}"), log_file)

# ------------------------------------------------------------------------------
# Load cache and determine which filings still need extraction
# ------------------------------------------------------------------------------
parsed_cache <- NULL
filings_pending <- filings_to_extract

if (file.exists(PATHS$parsed_sections)) {
  log_message("Found parsed_sections cache; loading...", log_file)
  parsed_cache <- readRDS(PATHS$parsed_sections)
  
  # Map accession -> parse_status; skip per policy
  cache_status <- parsed_cache %>%
    select(accession_number, parse_status) %>%
    group_by(accession_number) %>%
    summarise(parse_status = dplyr::first(parse_status), .groups = "drop")
  
  filings_pending <- filings_to_extract %>%
    left_join(cache_status, by = "accession_number") %>%
    rowwise() %>%
    mutate(skip_cached = should_skip_cached(parse_status)) %>%
    ungroup() %>%
    filter(!skip_cached) %>%
    select(accession_number, filing_url, matched_cik, filing_date)
  
  log_message(glue("  → Cached rows: {nrow(parsed_cache)}"), log_file)
  log_message(glue("  → Filings skipped due to cache: {nrow(filings_to_extract) - nrow(filings_pending)}"), log_file)
  log_message(glue("  → Filings pending extraction: {nrow(filings_pending)}"), log_file)
} else {
  log_message("No parsed_sections cache found; extracting all filings.", log_file)
}

# ------------------------------------------------------------------------------
# Extract sections
# ------------------------------------------------------------------------------
rate_limiter <- make_rate_limiter_safe(SEC_API$rate_limit_per_second)

if (nrow(filings_pending) > 0) {
  log_message(glue("Extracting sections for {nrow(filings_pending)} filings via SEC-API..."), log_file)
  
  progress <- create_progress_safe(nrow(filings_pending), "SEC-API Extractor")
  parsed_results <- vector("list", nrow(filings_pending))
  
  checkpoint_every <- 50L
  
  for (i in seq_len(nrow(filings_pending))) {
    accession <- filings_pending$accession_number[i]
    f_url <- filings_pending$filing_url[i]
    
    res <- tryCatch({
      r7 <- sec_api_get_item(
        filing_url = f_url, item = "7",
        token = SEC_API_TOKEN, endpoint = SEC_API$endpoint, type = SEC_API$type,
        max_retries = SEC_API$max_retries, retry_delay_seconds = SEC_API$retry_delay_seconds,
        rate_limiter = rate_limiter
      )
      
      r1a <- sec_api_get_item(
        filing_url = f_url, item = "1A",
        token = SEC_API_TOKEN, endpoint = SEC_API$endpoint, type = SEC_API$type,
        max_retries = SEC_API$max_retries, retry_delay_seconds = SEC_API$retry_delay_seconds,
        rate_limiter = rate_limiter
      )
      
      mda_txt <- if (isTRUE(r7$ok)) r7$body else NA_character_
      risk_txt <- if (isTRUE(r1a$ok)) r1a$body else NA_character_
      
      mda_wc <- if (!is.na(mda_txt)) str_count(mda_txt, "\\S+") else 0L
      risk_wc <- if (!is.na(risk_txt)) str_count(risk_txt, "\\S+") else 0L
      
      parse_status <- case_when(
        !isTRUE(r7$ok) & !isTRUE(r1a$ok) ~ "sec_api_failed_both",
        !isTRUE(r7$ok) ~ "sec_api_failed_item_7",
        !isTRUE(r1a$ok) ~ "sec_api_failed_item_1A",
        mda_wc < VALIDATION$min_mda_words ~ "too_short_mda",
        risk_wc < VALIDATION$min_risk_words ~ "too_short_risk",
        TRUE ~ "success"
      )
      
      tibble(
        accession_number = accession,
        filing_url = f_url,
        source = "sec_api_extractor",
        extracted_at = Sys.time(),
        
        mda_text = mda_txt,
        risk_factors_text = risk_txt,
        mda_word_count = as.integer(mda_wc),
        risk_word_count = as.integer(risk_wc),
        
        parse_status = parse_status,
        
        api_http_7 = as.integer(r7$status %||% NA_integer_),
        api_http_1A = as.integer(r1a$status %||% NA_integer_),
        api_error_7 = if (isTRUE(r7$ok)) NA_character_ else str_sub(r7$body %||% "", 1, 800),
        api_error_1A = if (isTRUE(r1a$ok)) NA_character_ else str_sub(r1a$body %||% "", 1, 800)
      )
    }, error = function(e) {
      tibble(
        accession_number = accession,
        filing_url = f_url,
        source = "sec_api_extractor",
        extracted_at = Sys.time(),
        mda_text = NA_character_,
        risk_factors_text = NA_character_,
        mda_word_count = 0L,
        risk_word_count = 0L,
        parse_status = "error",
        api_http_7 = NA_integer_,
        api_http_1A = NA_integer_,
        api_error_7 = str_sub(e$message, 1, 800),
        api_error_1A = str_sub(e$message, 1, 800)
      )
    })
    
    parsed_results[[i]] <- res
    progress$update(i)
    
    if (i %% checkpoint_every == 0) {
      temp_new <- bind_rows(parsed_results[1:i])
      if (!is.null(parsed_cache)) {
        temp_all <- bind_rows(parsed_cache, temp_new)
      } else {
        temp_all <- temp_new
      }
      saveRDS(temp_all, paste0(PATHS$parsed_sections, ".temp"))
      log_message(glue("Checkpoint saved: {i}/{nrow(filings_pending)}"), log_file)
    }
  }
  
  progress$close()
  
  new_parsed <- bind_rows(parsed_results)
  
  if (!is.null(parsed_cache)) {
    all_parsed <- bind_rows(parsed_cache, new_parsed) %>%
      arrange(accession_number, extracted_at) %>%
      group_by(accession_number) %>%
      summarise(
        # keep the latest attempt
        filing_url = dplyr::last(filing_url),
        source = dplyr::last(source),
        extracted_at = dplyr::last(extracted_at),
        mda_text = dplyr::last(mda_text),
        risk_factors_text = dplyr::last(risk_factors_text),
        mda_word_count = dplyr::last(mda_word_count),
        risk_word_count = dplyr::last(risk_word_count),
        parse_status = dplyr::last(parse_status),
        api_http_7 = dplyr::last(api_http_7),
        api_http_1A = dplyr::last(api_http_1A),
        api_error_7 = dplyr::last(api_error_7),
        api_error_1A = dplyr::last(api_error_1A),
        .groups = "drop"
      )
  } else {
    all_parsed <- new_parsed
  }
  
} else {
  log_message("No filings pending extraction; using cached parsed_sections.", log_file)
  all_parsed <- parsed_cache
}

# ------------------------------------------------------------------------------
# Statistics + QA summaries
# ------------------------------------------------------------------------------
log_message("\nParse status distribution:", log_file)
parse_summary <- all_parsed %>%
  count(parse_status) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  arrange(desc(n))

for (i in seq_len(nrow(parse_summary))) {
  log_message(glue("  → {parse_summary$parse_status[i]}: {parse_summary$n[i]} ({parse_summary$pct[i]}%)"), log_file)
}

successful_parses <- all_parsed %>% filter(parse_status == "success")
log_message(glue("\nSuccess coverage: {nrow(successful_parses)} / {nrow(all_parsed)} ({round(100 * nrow(successful_parses) / max(nrow(all_parsed), 1), 1)}%)"), log_file)

if (nrow(successful_parses) > 0) {
  log_message("Successful parses stats:", log_file)
  log_message(glue("  → Mean MD&A words: {round(mean(successful_parses$mda_word_count), 0)}"), log_file)
  log_message(glue("  → Median MD&A words: {median(successful_parses$mda_word_count)}"), log_file)
  log_message(glue("  → Mean Risk words: {round(mean(successful_parses$risk_word_count), 0)}"), log_file)
  log_message(glue("  → Median Risk words: {median(successful_parses$risk_word_count)}"), log_file)
}

# ------------------------------------------------------------------------------
# Save parsed sections
# ------------------------------------------------------------------------------
log_message("\nSaving parsed sections...", log_file)
ensure_dir(dirname(PATHS$parsed_sections), log_file)
safe_save(all_parsed, PATHS$parsed_sections, log_file)

temp_file <- paste0(PATHS$parsed_sections, ".temp")
if (file.exists(temp_file)) file.remove(temp_file)

# Sample for inspection
log_message("\nCreating sample for manual inspection...", log_file)
sample_size <- min(10, nrow(all_parsed))
sample_parsed <- all_parsed %>% slice_sample(n = sample_size)

sample_path <- file.path(dirname(PATHS$parsed_sections), "parsing_sample_sec_api.rds")
saveRDS(sample_parsed, sample_path)
log_message(glue("  → Sample saved: {sample_path}"), log_file)

log_section("STEP 5 COMPLETE", log_file)
log_message(glue("Output: {PATHS$parsed_sections}"), log_file)
log_message("Next step: Run 06_merge_final_dataset.R", log_file)
