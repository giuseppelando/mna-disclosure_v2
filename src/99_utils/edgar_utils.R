# ==============================================================================
# EDGAR Utilities (Archives + index.json primary document resolution + download)
# Allowed dependencies only: tidyverse, httr, jsonlite, stringr/stringi, lubridate,
# glue, purrr
# ==============================================================================

library(tidyverse)
library(httr)
library(jsonlite)
library(stringr)
library(glue)
library(purrr)
library(lubridate)

# ------------------------------------------------------------------------------
# Small helpers
# ------------------------------------------------------------------------------

#' Convert a CIK (possibly 10-digit padded) into SEC Archives path form (no leading zeros)
#' @param cik CIK as character/numeric
#' @return character scalar (e.g. "320193"), or NA_character_
cik_to_int <- function(cik) {
  x <- as.character(cik)
  x <- if_else(is.na(x), NA_character_, x)
  x <- str_replace_all(x, "\\D+", "")
  x <- na_if(x, "")
  x <- str_replace(x, "^0+", "")
  if_else(is.na(x) | x == "", NA_character_, x)
}

#' Remove dashes from accession number
#' @param accession_number character
#' @return character scalar (e.g. "000032019323000066"), or NA_character_
accession_nodash <- function(accession_number) {
  x <- as.character(accession_number)
  x <- if_else(is.na(x), NA_character_, x)
  x <- str_replace_all(x, "-", "")
  x <- na_if(x, "")
  x
}

#' Optional helper: user agent from env without stopping
#' @param env_var environment variable name, default "SEC_USER_AGENT"
#' @return list(ok, user_agent, error_message)
safe_user_agent_from_env <- function(env_var = "SEC_USER_AGENT") {
  ua <- Sys.getenv(env_var, unset = "")
  if (identical(ua, "")) {
    return(list(ok = FALSE, user_agent = NA_character_, error_message = paste0("Missing env var: ", env_var)))
  }
  list(ok = TRUE, user_agent = ua, error_message = NA_character_)
}

#' SEC request headers (User-Agent required)
#' @param user_agent character scalar
#' @param accept optional Accept header
#' @return httr headers object
sec_headers <- function(user_agent, accept = NULL) {
  ua <- as.character(user_agent)
  if (is.na(ua) || ua == "") {
    # do not stop; caller functions should validate; still return something
    ua <- "MISSING_SEC_USER_AGENT"
  }
  
  hdrs <- c(
    `User-Agent` = ua,
    `Accept-Encoding` = "gzip, deflate",
    `Connection` = "keep-alive"
  )
  if (!is.null(accept) && !is.na(accept) && accept != "") {
    hdrs <- c(hdrs, `Accept` = as.character(accept))
  }
  
  do.call(add_headers, as.list(hdrs))
}

.ensure_trailing_slash <- function(x) {
  x <- as.character(x)
  if_else(is.na(x) | x == "", NA_character_, if_else(str_ends(x, "/"), x, paste0(x, "/")))
}

.is_valid_accession_dashed <- function(x) {
  x <- as.character(x)
  !is.na(x) && str_detect(x, "^\\d{10}-\\d{2}-\\d{6}$")
}

# ------------------------------------------------------------------------------
# API: Build Archives directory URL
# ------------------------------------------------------------------------------

#' Build EDGAR Archives directory URL for a filing accession
#' @param cik_int CIK in Archives path form (no leading zeros), character/numeric
#' @param accession_number accession in dashed form (##########-##-######)
#' @return archives_dir_url or NA_character_
build_archives_dir_url <- function(cik_int, accession_number) {
  cik_i <- cik_to_int(cik_int)
  acc <- as.character(accession_number)
  if (is.na(cik_i) || cik_i == "" || is.na(acc) || acc == "" || !.is_valid_accession_dashed(acc)) {
    return(NA_character_)
  }
  acc_nd <- accession_nodash(acc)
  glue("https://www.sec.gov/Archives/edgar/data/{cik_i}/{acc_nd}/")
}

# ------------------------------------------------------------------------------
# Internal: safe GET with retries (no stop)
# ------------------------------------------------------------------------------

.safe_get <- function(url,
                      user_agent,
                      rate_limiter = NULL,
                      max_retries = 3,
                      retry_delay_seconds = 2,
                      timeout_seconds = 30,
                      accept = NULL) {
  out <- list(ok = FALSE, response = NULL, error_message = NA_character_)
  
  u <- as.character(url)
  if (is.na(u) || u == "") {
    out$error_message <- "Missing URL"
    return(out)
  }
  
  ua <- as.character(user_agent)
  if (is.na(ua) || ua == "") {
    out$error_message <- "Missing user_agent"
    return(out)
  }
  
  attempts <- max(1L, as.integer(max_retries))
  
  for (i in seq_len(attempts)) {
    if (is.function(rate_limiter)) {
      try(rate_limiter(), silent = TRUE)
    }
    
    resp <- try(
      GET(
        u,
        sec_headers(ua, accept = accept),
        timeout(timeout_seconds)
      ),
      silent = TRUE
    )
    
    if (inherits(resp, "try-error") || is.null(resp)) {
      msg <- paste0("Request error: ", as.character(resp))
      if (i < attempts) {
        Sys.sleep(as.numeric(retry_delay_seconds))
        next
      }
      out$error_message <- msg
      return(out)
    }
    
    sc <- status_code(resp)
    
    if (identical(sc, 200L)) {
      out$ok <- TRUE
      out$response <- resp
      out$error_message <- NA_character_
      return(out)
    }
    
    # Retry on rate limit and transient server errors; otherwise fail fast
    retryable <- (sc %in% c(429L, 500L, 502L, 503L, 504L))
    msg <- paste0("HTTP ", sc)
    
    if (retryable && i < attempts) {
      Sys.sleep(as.numeric(retry_delay_seconds))
      next
    }
    
    out$error_message <- msg
    out$response <- resp
    return(out)
  }
  
  out$error_message <- "Unknown request failure"
  out
}

# ------------------------------------------------------------------------------
# API: Fetch and parse index.json
# ------------------------------------------------------------------------------

#' Fetch EDGAR Archives index.json and parse it into a file list
#' @param index_json_url URL to index.json
#' @param user_agent SEC-compliant user agent
#' @param rate_limiter function() for rate limiting (optional)
#' @param max_retries integer
#' @param retry_delay_seconds numeric
#' @return list(ok, files, index_json, error_message)
fetch_index_json <- function(index_json_url,
                             user_agent,
                             rate_limiter = NULL,
                             max_retries = 3,
                             retry_delay_seconds = 2) {
  out <- list(ok = FALSE,
              files = tibble(name = character(), size = numeric(), type = character()),
              index_json = NULL,
              error_message = NA_character_)
  
  req <- .safe_get(
    url = index_json_url,
    user_agent = user_agent,
    rate_limiter = rate_limiter,
    max_retries = max_retries,
    retry_delay_seconds = retry_delay_seconds,
    timeout_seconds = 30,
    accept = "application/json"
  )
  
  if (!isTRUE(req$ok) || is.null(req$response)) {
    out$error_message <- req$error_message
    return(out)
  }
  
  txt <- try(content(req$response, as = "text", encoding = "UTF-8"), silent = TRUE)
  if (inherits(txt, "try-error") || is.null(txt) || identical(txt, "")) {
    out$error_message <- "Failed to read response body"
    return(out)
  }
  
  parsed <- try(fromJSON(txt, simplifyVector = FALSE), silent = TRUE)
  if (inherits(parsed, "try-error") || is.null(parsed)) {
    out$error_message <- "Failed to parse JSON"
    return(out)
  }
  
  files <- tibble(name = character(), size = numeric(), type = character())
  if (!is.null(parsed$directory) && !is.null(parsed$directory$item)) {
    items <- parsed$directory$item
    # items is typically a list of lists
    files <- map_dfr(items, function(it) {
      tibble(
        name = as.character(it$name %||% NA_character_),
        size = suppressWarnings(as.numeric(it$size %||% NA_real_)),
        type = as.character(it$type %||% NA_character_)
      )
    })
  } else if (!is.null(parsed$item)) {
    items <- parsed$item
    files <- map_dfr(items, function(it) {
      tibble(
        name = as.character(it$name %||% NA_character_),
        size = suppressWarnings(as.numeric(it$size %||% NA_real_)),
        type = as.character(it$type %||% NA_character_)
      )
    })
  }
  
  files <- files %>%
    mutate(
      name = na_if(name, ""),
      type = na_if(type, "")
    )
  
  out$ok <- TRUE
  out$files <- files
  out$index_json <- parsed
  out$error_message <- NA_character_
  out
}

# ------------------------------------------------------------------------------
# API: Resolve primary document (HTML/HTM) from index.json
# ------------------------------------------------------------------------------

.extract_files_from_index <- function(index_json) {
  if (is.null(index_json)) {
    return(tibble(name = character(), size = numeric(), type = character()))
  }
  if (is.list(index_json) && !is.null(index_json$files) && is.data.frame(index_json$files)) {
    return(as_tibble(index_json$files))
  }
  if (is.list(index_json) && !is.null(index_json$directory) && !is.null(index_json$directory$item)) {
    items <- index_json$directory$item
    return(map_dfr(items, function(it) {
      tibble(
        name = as.character(it$name %||% NA_character_),
        size = suppressWarnings(as.numeric(it$size %||% NA_real_)),
        type = as.character(it$type %||% NA_character_)
      )
    }))
  }
  if (is.list(index_json) && !is.null(index_json$item)) {
    items <- index_json$item
    return(map_dfr(items, function(it) {
      tibble(
        name = as.character(it$name %||% NA_character_),
        size = suppressWarnings(as.numeric(it$size %||% NA_real_)),
        type = as.character(it$type %||% NA_character_)
      )
    }))
  }
  tibble(name = character(), size = numeric(), type = character())
}

#' Resolve the primary 10-K HTML document URL using Archives index.json
#' @param archives_dir_url directory URL (ending with "/")
#' @param index_json parsed JSON (from fetch_index_json$index_json) OR list(files=...)
#' @param primary_document_expected optional expected filename (from submissions API)
#' @return list(resolved_filename, resolved_url, strategy, error_message)
resolve_primary_document_url <- function(archives_dir_url,
                                         index_json,
                                         primary_document_expected = NULL) {
  out <- list(
    resolved_filename = NA_character_,
    resolved_url = NA_character_,
    strategy = NA_character_,
    error_message = NA_character_
  )
  
  base <- .ensure_trailing_slash(archives_dir_url)
  if (is.na(base) || base == "") {
    out$error_message <- "Missing archives_dir_url"
    return(out)
  }
  
  files <- .extract_files_from_index(index_json) %>%
    mutate(
      name_lc = str_to_lower(as.character(name)),
      ext = str_extract(name_lc, "\\.[a-z0-9]+$"),
      size = suppressWarnings(as.numeric(size))
    )
  
  if (nrow(files) == 0) {
    out$error_message <- "index.json has no file entries"
    return(out)
  }
  
  # 1) Use expected filename if provided and present
  if (!is.null(primary_document_expected) &&
      !is.na(primary_document_expected) &&
      as.character(primary_document_expected) != "") {
    exp_name <- as.character(primary_document_expected)
    hit <- files %>%
      filter(!is.na(name)) %>%
      filter(str_to_lower(name) == str_to_lower(exp_name))
    
    if (nrow(hit) >= 1) {
      fname <- hit$name[1]
      out$resolved_filename <- fname
      out$resolved_url <- paste0(base, fname)
      out$strategy <- "expected_filename_match"
      out$error_message <- NA_character_
      return(out)
    }
  }
  
  # 2) Otherwise, choose among HTML/HTM candidates excluding common exhibits/non-primary artifacts
  html_candidates <- files %>%
    filter(ext %in% c(".htm", ".html")) %>%
    filter(!is.na(name_lc)) %>%
    # Exclusions per requirements + common noise
    filter(!str_detect(name_lc, "^ex")) %>%
    filter(!str_detect(name_lc, "exhibit")) %>%
    filter(!str_detect(name_lc, "graphics")) %>%
    filter(!str_detect(name_lc, "\\.xsl$")) %>%
    filter(!str_detect(name_lc, "\\.xml$")) %>%
    filter(!str_detect(name_lc, "calx")) %>%
    filter(!str_detect(name_lc, "defn")) %>%
    filter(!str_detect(name_lc, "lab")) %>%
    filter(!str_detect(name_lc, "pre")) %>%
    filter(!str_detect(name_lc, "xbrl")) %>%
    # Avoid common directory landing pages where possible
    filter(!(name_lc %in% c("index.htm", "index.html")))
  
  if (nrow(html_candidates) == 0) {
    # fallback: allow index.html/index.htm if that's all we have
    html_candidates <- files %>%
      filter(ext %in% c(".htm", ".html")) %>%
      filter(!is.na(name_lc)) %>%
      filter(!str_detect(name_lc, "^ex")) %>%
      filter(!str_detect(name_lc, "exhibit")) %>%
      filter(!str_detect(name_lc, "graphics")) %>%
      filter(!str_detect(name_lc, "\\.xsl$")) %>%
      filter(!str_detect(name_lc, "\\.xml$")) %>%
      filter(!str_detect(name_lc, "xbrl"))
  }
  
  if (nrow(html_candidates) == 0) {
    out$error_message <- "No eligible .htm/.html candidates found in index.json"
    return(out)
  }
  
  # Choose the largest by size (explainable, deterministic tie-break on name)
  chosen <- html_candidates %>%
    mutate(size = replace_na(size, -Inf)) %>%
    arrange(desc(size), name_lc) %>%
    slice(1)
  
  fname <- as.character(chosen$name[1])
  if (is.na(fname) || fname == "") {
    out$error_message <- "Resolved candidate filename is missing"
    return(out)
  }
  
  out$resolved_filename <- fname
  out$resolved_url <- paste0(base, fname)
  out$strategy <- "largest_html_candidate"
  out$error_message <- NA_character_
  out
}

# ------------------------------------------------------------------------------
# API: Download file with SEC headers + retries (no stop)
# ------------------------------------------------------------------------------

#' Download a file from resolved URL to disk with SEC headers and retry logic
#' @param resolved_url URL to download
#' @param dest_path destination file path
#' @param user_agent SEC user agent
#' @param rate_limiter function() rate limiter (optional)
#' @param max_retries integer
#' @param retry_delay_seconds numeric
#' @return list(ok, file_bytes, error_message)
download_file_with_headers <- function(resolved_url,
                                       dest_path,
                                       user_agent,
                                       rate_limiter = NULL,
                                       max_retries = 3,
                                       retry_delay_seconds = 2) {
  out <- list(ok = FALSE, file_bytes = NA_real_, error_message = NA_character_)
  
  url <- as.character(resolved_url)
  if (is.na(url) || url == "") {
    out$error_message <- "Missing resolved_url"
    return(out)
  }
  
  ua <- as.character(user_agent)
  if (is.na(ua) || ua == "") {
    out$error_message <- "Missing user_agent"
    return(out)
  }
  
  dp <- as.character(dest_path)
  if (is.na(dp) || dp == "") {
    out$error_message <- "Missing dest_path"
    return(out)
  }
  
  dir.create(dirname(dp), recursive = TRUE, showWarnings = FALSE)
  
  tmp <- paste0(dp, ".part")
  if (file.exists(tmp)) {
    try(file.remove(tmp), silent = TRUE)
  }
  
  attempts <- max(1L, as.integer(max_retries))
  
  for (i in seq_len(attempts)) {
    if (is.function(rate_limiter)) {
      try(rate_limiter(), silent = TRUE)
    }
    
    resp <- try(
      GET(
        url,
        sec_headers(ua, accept = "*/*"),
        timeout(60),
        write_disk(tmp, overwrite = TRUE)
      ),
      silent = TRUE
    )
    
    if (inherits(resp, "try-error") || is.null(resp)) {
      msg <- paste0("Request error: ", as.character(resp))
      if (i < attempts) {
        Sys.sleep(as.numeric(retry_delay_seconds))
        next
      }
      out$error_message <- msg
      try(file.remove(tmp), silent = TRUE)
      return(out)
    }
    
    sc <- status_code(resp)
    if (identical(sc, 200L)) {
      info <- try(file.info(tmp), silent = TRUE)
      if (inherits(info, "try-error") || is.null(info) || is.na(info$size)) {
        out$error_message <- "Downloaded file but failed to read file size"
        try(file.remove(tmp), silent = TRUE)
        return(out)
      }
      
      # Atomic-ish move into place
      if (file.exists(dp)) {
        try(file.remove(dp), silent = TRUE)
      }
      ok_move <- try(file.rename(tmp, dp), silent = TRUE)
      if (inherits(ok_move, "try-error") || !isTRUE(ok_move)) {
        # fallback copy+remove
        ok_copy <- try(file.copy(tmp, dp, overwrite = TRUE), silent = TRUE)
        if (inherits(ok_copy, "try-error") || !isTRUE(ok_copy)) {
          out$error_message <- "Failed to move downloaded file into destination"
          try(file.remove(tmp), silent = TRUE)
          return(out)
        }
        try(file.remove(tmp), silent = TRUE)
      }
      
      out$ok <- TRUE
      out$file_bytes <- as.numeric(info$size)
      out$error_message <- NA_character_
      return(out)
    }
    
    retryable <- (sc %in% c(429L, 500L, 502L, 503L, 504L))
    msg <- paste0("HTTP ", sc)
    
    if (retryable && i < attempts) {
      Sys.sleep(as.numeric(retry_delay_seconds))
      next
    }
    
    out$error_message <- msg
    try(file.remove(tmp), silent = TRUE)
    return(out)
  }
  
  out$error_message <- "Unknown download failure"
  try(file.remove(tmp), silent = TRUE)
  out
}

# ------------------------------------------------------------------------------
# Compatibility: submissions API query (used in step 02)
# ------------------------------------------------------------------------------

#' Query SEC submissions API for recent filings for a given CIK
#' Returns a tibble suitable for downstream coercion: cik, accession_number,
#' filing_date, form, primary_document
#' @param cik CIK as character/numeric (any format; will be padded to 10 for API)
#' @param user_agent SEC-compliant user agent
#' @param form_type form type filter, default "10-K"
#' @return tibble (possibly empty); never stops
query_edgar_filings <- function(cik, user_agent, form_type = "10-K") {
  ua <- as.character(user_agent)
  if (is.na(ua) || ua == "") {
    return(tibble(
      cik = character(),
      accession_number = character(),
      filing_date = as.Date(character()),
      form = character(),
      primary_document = character()
    ))
  }
  
  cik_digits <- as.character(cik)
  cik_digits <- if_else(is.na(cik_digits), NA_character_, cik_digits)
  cik_digits <- str_replace_all(cik_digits, "\\D+", "")
  cik_digits <- na_if(cik_digits, "")
  if (is.na(cik_digits)) {
    return(tibble(
      cik = character(),
      accession_number = character(),
      filing_date = as.Date(character()),
      form = character(),
      primary_document = character()
    ))
  }
  
  cik_padded <- str_pad(cik_digits, width = 10, side = "left", pad = "0")
  url <- glue("https://data.sec.gov/submissions/CIK{cik_padded}.json")
  
  req <- .safe_get(
    url = url,
    user_agent = ua,
    rate_limiter = NULL,
    max_retries = 3,
    retry_delay_seconds = 2,
    timeout_seconds = 30,
    accept = "application/json"
  )
  
  if (!isTRUE(req$ok) || is.null(req$response)) {
    return(tibble(
      cik = character(),
      accession_number = character(),
      filing_date = as.Date(character()),
      form = character(),
      primary_document = character()
    ))
  }
  
  txt <- try(content(req$response, as = "text", encoding = "UTF-8"), silent = TRUE)
  if (inherits(txt, "try-error") || is.null(txt) || identical(txt, "")) {
    return(tibble(
      cik = character(),
      accession_number = character(),
      filing_date = as.Date(character()),
      form = character(),
      primary_document = character()
    ))
  }
  
  dat <- try(fromJSON(txt, flatten = TRUE), silent = TRUE)
  if (inherits(dat, "try-error") || is.null(dat)) {
    return(tibble(
      cik = character(),
      accession_number = character(),
      filing_date = as.Date(character()),
      form = character(),
      primary_document = character()
    ))
  }
  
  if (is.null(dat$filings) || is.null(dat$filings$recent)) {
    return(tibble(
      cik = character(),
      accession_number = character(),
      filing_date = as.Date(character()),
      form = character(),
      primary_document = character()
    ))
  }
  
  rec <- as_tibble(dat$filings$recent)
  
  # Required columns in submissions payload are typically:
  # form, filingDate, accessionNumber, primaryDocument
  needed <- c("form", "filingDate", "accessionNumber", "primaryDocument")
  if (!all(needed %in% names(rec)) || nrow(rec) == 0) {
    return(tibble(
      cik = character(),
      accession_number = character(),
      filing_date = as.Date(character()),
      form = character(),
      primary_document = character()
    ))
  }
  
  rec %>%
    filter(.data$form == form_type) %>%
    transmute(
      cik = str_pad(cik_digits, width = 10, side = "left", pad = "0"),
      accession_number = as.character(.data$accessionNumber),
      filing_date = as.Date(.data$filingDate),
      form = as.character(.data$form),
      primary_document = as.character(.data$primaryDocument)
    ) %>%
    filter(!is.na(accession_number), accession_number != "") %>%
    filter(!is.na(filing_date))
}
