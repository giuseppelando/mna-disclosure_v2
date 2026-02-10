# ==============================================================================
# Section Parsing Utilities (10-K only)
# Fast extraction of MD&A (Item 7) and Risk Factors (Item 1A)
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

# Fast fixed-string trim to the first <DOCUMENT> ... </DOCUMENT> block
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

# Prefer the <DOCUMENT> block with <TYPE>10-K (avoids exhibits and speeds up parsing)
extract_document_by_type <- function(text, type = "10-K") {
  parts <- strsplit(text, "<DOCUMENT>", fixed = TRUE)[[1]]
  if (length(parts) <= 1) return(text)
  
  docs <- parts[-1]  # drop header before first <DOCUMENT>
  
  for (d in docs) {
    end <- regexpr("</DOCUMENT>", d, fixed = TRUE)[1]
    d1 <- if (end > 0) substr(d, 1, end - 1) else d
    
    head <- substr(d1, 1, min(4000, nchar(d1)))
    if (str_detect(head, regex(paste0("<TYPE>\\s*", type, "\\b"), ignore_case = TRUE))) {
      return(d1)
    }
  }
  
  # fallback: first document block
  d <- docs[1]
  end <- regexpr("</DOCUMENT>", d, fixed = TRUE)[1]
  if (end > 0) substr(d, 1, end - 1) else d
}

# Generic extractor with more flexible patterns
extract_item_section <- function(text,
                                 start_patterns,
                                 end_patterns,
                                 search_max,
                                 end_window,
                                 min_chars_after_start,
                                 fallback_chars) {
  n <- nchar(text)
  if (n == 0) return(NA_character_)
  
  search_text <- substr(text, 1, min(search_max, n))
  
  # Find candidate starts (prefer the first pattern that yields matches)
  starts <- integer(0)
  for (p in start_patterns) {
    locs <- gregexpr(p, search_text, ignore.case = TRUE, perl = TRUE)[[1]]
    if (locs[1] > 0) {
      starts <- sort(unique(locs))
      break
    }
  }
  if (length(starts) == 0) return(NA_character_)
  
  # One combined end regex
  end_regex <- paste0("(", paste(end_patterns, collapse = ")|("), ")")
  
  # Pick the first start that yields a "long enough" section (skips TOC hits)
  for (s in starts) {
    end_text <- substr(text, s, min(s + end_window, n))
    e <- regexpr(end_regex, end_text, ignore.case = TRUE, perl = TRUE)[1]
    if (e > min_chars_after_start) {
      section <- substr(text, s, s + e - 2)
      return(clean_html(section))
    }
  }
  
  # Fallback window after the first start
  s <- starts[1]
  section <- substr(text, s, min(s + fallback_chars, n))
  clean_html(section)
}

extract_mda <- function(filing_text) {
  extract_item_section(
    text = filing_text,
    # FLEXIBLE patterns - match any punctuation after Item 7
    # Removed line-start anchors to catch more formatting variations
    start_patterns = c(
      "item[[:space:]]+7[[:space:][:punct:]]+management",  # Item 7. Management / Item 7, Management / Item 7: Management
      "item[[:space:]]+7[[:space:][:punct:]]+md",          # Item 7. MD&A / Item 7, MD&A
      "item[[:space:]]+7[[:space:][:punct:]]+"             # Item 7. / Item 7, / Item 7:
    ),
    end_patterns = c(
      "item[[:space:]]+7a[[:space:][:punct:]]",  # Item 7A. / Item 7A, / Item 7A:
      "item[[:space:]]+8[[:space:][:punct:]]"    # Item 8. / Item 8, / Item 8:
    ),
    search_max = 1500000,
    end_window = 1200000,
    min_chars_after_start = 2000,   # Lowered from 3000 to catch shorter sections
    fallback_chars = 250000
  )
}

extract_risk_factors <- function(filing_text) {
  extract_item_section(
    text = filing_text,
    start_patterns = c(
      "item[[:space:]]+1a[[:space:][:punct:]]+risk[[:space:]]+factors",  # Item 1A. Risk Factors / Item 1A, Risk Factors
      "item[[:space:]]+1a[[:space:][:punct:]]+"                          # Item 1A. / Item 1A, / Item 1A:
    ),
    end_patterns = c(
      "item[[:space:]]+1b[[:space:][:punct:]]",  # Item 1B. / Item 1B, / Item 1B:
      "item[[:space:]]+2[[:space:][:punct:]]"    # Item 2. / Item 2, / Item 2:
    ),
    search_max = 1200000,
    end_window = 800000,
    min_chars_after_start = 1500,   # Lowered from 2000 to catch shorter sections
    fallback_chars = 200000
  )
}

parse_10k_sections <- function(filing_text) {
  # 1) Choose the 10-K document block (major speed + correctness improvement)
  filing_text <- extract_document_by_type(filing_text, type = "10-K")
  
  # 2) Fast trimming of EDGAR headers
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
