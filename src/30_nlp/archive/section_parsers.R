# ==============================================================================
# Section Parsing Utilities - Robust TOC-Aware Version
# ==============================================================================

suppressPackageStartupMessages({
  library(stringr)
})

clean_html <- function(text) {
  text <- str_replace_all(text, "<[^>]+>", " ")
  text <- str_replace_all(text, "</?(ix|us-gaap|dei):[^>]+>", " ")
  text <- str_replace_all(text, "&nbsp;", " ")
  text <- str_replace_all(text, "&amp;", "&")
  text <- str_replace_all(text, "&lt;", "<")
  text <- str_replace_all(text, "&gt;", ">")
  text <- str_replace_all(text, "&quot;", '"')
  text <- str_replace_all(text, "&#[0-9]+;", " ")
  text <- str_replace_all(text, "\\t+", " ")
  str_squish(text)
}

remove_edgar_headers_fast <- function(text) {
  start <- regexpr("<DOCUMENT>", text, fixed = TRUE)[1]
  if (start > 0) {
    text <- substr(text, start + nchar("<DOCUMENT>"), nchar(text))
  }
  end <- regexpr("</DOCUMENT>", text, fixed = TRUE)[1]
  if (end > 0) {
    text <- substr(text, 1, end - 1)
  }
  text
}

extract_document_by_type <- function(text, type = "10-K") {
  parts <- strsplit(text, "<DOCUMENT>", fixed = TRUE)[[1]]
  if (length(parts) <= 1) return(text)
  
  docs <- parts[-1]
  
  for (d in docs) {
    end <- regexpr("</DOCUMENT>", d, fixed = TRUE)[1]
    d1 <- if (end > 0) substr(d, 1, end - 1) else d
    
    head <- substr(d1, 1, min(4000, nchar(d1)))
    if (str_detect(head, regex(paste0("<TYPE>\\s*", type, "\\b"), ignore_case = TRUE))) {
      return(d1)
    }
  }
  
  d <- docs[1]
  end <- regexpr("</DOCUMENT>", d, fixed = TRUE)[1]
  if (end > 0) substr(d, 1, end - 1) else d
}

# NEW: Find ALL occurrences and pick the longest (skips TOC)
extract_item_robust <- function(text,
                                start_patterns,
                                end_patterns,
                                search_max,
                                end_window,
                                min_section_words) {
  n <- nchar(text)
  if (n == 0) return(NA_character_)
  
  search_text <- substr(text, 1, min(search_max, n))
  
  # Find ALL candidate starts
  all_starts <- integer(0)
  for (p in start_patterns) {
    locs <- gregexpr(p, search_text, ignore.case = TRUE, perl = TRUE)[[1]]
    if (locs[1] > 0) {
      all_starts <- c(all_starts, locs)
    }
  }
  
  if (length(all_starts) == 0) return(NA_character_)
  all_starts <- sort(unique(all_starts))
  
  # Combined end regex
  end_regex <- paste0("(", paste(end_patterns, collapse = ")|("), ")")
  
  # Extract ALL candidate sections and pick the LONGEST
  candidates <- list()
  
  for (i in seq_along(all_starts)) {
    s <- all_starts[i]
    
    # Find end position
    end_text <- substr(text, s, min(s + end_window, n))
    e <- regexpr(end_regex, end_text, ignore.case = TRUE, perl = TRUE)[1]
    
    if (e > 0) {
      section_end <- s + e - 2
    } else {
      # No end found, use window
      section_end <- min(s + end_window, n)
    }
    
    section_raw <- substr(text, s, section_end)
    section_clean <- clean_html(section_raw)
    word_count <- str_count(section_clean, "\\S+")
    
    # Only keep sections with substantial content (not TOC)
    if (word_count >= min_section_words) {
      candidates[[length(candidates) + 1]] <- list(
        text = section_clean,
        words = word_count,
        start_pos = s
      )
    }
  }
  
  # Return the LONGEST section (real content, not TOC)
  if (length(candidates) == 0) return(NA_character_)
  
  word_counts <- sapply(candidates, function(x) x$words)
  longest_idx <- which.max(word_counts)
  
  return(candidates[[longest_idx]]$text)
}

extract_mda <- function(filing_text) {
  extract_item_robust(
    text = filing_text,
    start_patterns = c(
      "item[[:space:]]+7[[:space:][:punct:]]+management",
      "item[[:space:]]+7[[:space:][:punct:]]+md",
      "item[[:space:]]+7[[:space:][:punct:]]+"
    ),
    end_patterns = c(
      "item[[:space:]]+7a[[:space:][:punct:]]",
      "item[[:space:]]+8[[:space:][:punct:]]"
    ),
    search_max = 1500000,
    end_window = 1200000,
    min_section_words = 1000  # Real MD&A sections are at least 1000 words
  )
}

extract_risk_factors <- function(filing_text) {
  extract_item_robust(
    text = filing_text,
    start_patterns = c(
      "item[[:space:]]+1a[[:space:][:punct:]]+risk[[:space:]]+factors",
      "item[[:space:]]+1a[[:space:][:punct:]]+"
    ),
    end_patterns = c(
      "item[[:space:]]+1b[[:space:][:punct:]]",
      "item[[:space:]]+2[[:space:][:punct:]]"
    ),
    search_max = 1200000,
    end_window = 800000,
    min_section_words = 500  # Real Risk Factors are at least 500 words
  )
}

parse_10k_sections <- function(filing_text) {
  filing_text <- extract_document_by_type(filing_text, type = "10-K")
  filing_text <- remove_edgar_headers_fast(filing_text)
  
  mda_text  <- extract_mda(filing_text)
  risk_text <- extract_risk_factors(filing_text)
  
  mda_words  <- if (!is.na(mda_text))  str_count(mda_text, "\\S+") else 0L
  risk_words <- if (!is.na(risk_text)) str_count(risk_text, "\\S+") else 0L
  
  list(
    mda_text = mda_text,
    risk_factors_text = risk_text,
    mda_word_count = mda_words,
    risk_word_count = risk_words,
    parse_status = if (!is.na(mda_text) && !is.na(risk_text) && mda_words > 100 && risk_words > 100)
      "success" else "parse_failed"
  )
}

validate_parsed_sections <- function(parsed_result, min_words = 100, max_words = 200000) {
  mda_valid <- !is.na(parsed_result$mda_text) &&
    parsed_result$mda_word_count >= min_words &&
    parsed_result$mda_word_count <= max_words
  
  risk_valid <- !is.na(parsed_result$risk_factors_text) &&
    parsed_result$risk_word_count >= min_words &&
    parsed_result$risk_word_count <= max_words
  
  mda_valid && risk_valid
}
