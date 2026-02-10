# ==============================================================================
# EDGAR API Utilities
# Functions for querying SEC EDGAR database
# ==============================================================================

library(httr)
library(jsonlite)
library(xml2)
library(dplyr)
library(glue)

#' Query EDGAR for company filings
#'
#' @param cik Company CIK (Central Index Key)
#' @param user_agent User agent string (required by SEC)
#' @param form_type Type of form (e.g., "10-K")
#' @return Data frame of filings or NULL on error
query_edgar_filings <- function(cik, user_agent, form_type = "10-K") {
  # Pad CIK to 10 digits
  cik_padded <- str_pad(cik, width = 10, side = "left", pad = "0")
  
  # Construct API URL (using SEC's new API)
  url <- glue("https://data.sec.gov/submissions/CIK{cik_padded}.json")
  
  # Make request
  response <- GET(
    url,
    user_agent(user_agent),
    timeout(30)
  )
  
  if (status_code(response) != 200) {
    return(NULL)
  }
  
  # Parse JSON
  content <- content(response, as = "text", encoding = "UTF-8")
  data <- fromJSON(content, flatten = TRUE)
  
  # Extract filings
  if (!"filings" %in% names(data) || !"recent" %in% names(data$filings)) {
    return(NULL)
  }
  
  filings <- as.data.frame(data$filings$recent, stringsAsFactors = FALSE)
  
  # Check if filings dataframe has required columns
  if (nrow(filings) == 0 || !"form" %in% names(filings)) {
    return(NULL)
  }
  
  # Filter for form type (using .data pronoun to avoid NSE issues)
  filings_filtered <- filings[filings$form == form_type, ]
  
  if (nrow(filings_filtered) == 0) {
    return(NULL)
  }
  
  # Check if required columns exist
  required_cols <- c("filingDate", "accessionNumber", "primaryDocument")
  if (!all(required_cols %in% names(filings_filtered))) {
    return(NULL)
  }
  
  # Transform to standard format
  result <- filings_filtered %>%
    transmute(
      cik = cik,
      company_name = data$name,
      filing_date = as.Date(filingDate),
      accession_number = accessionNumber,
      primary_document = primaryDocument,
      form_type = form
    ) %>%
    # Filter out filings with missing primary_document
    filter(!is.na(primary_document), primary_document != "") %>%
    # Filter out very old filings (before Item 1A Risk Factors was mandatory)
    filter(filing_date >= as.Date("2005-01-01"))
  
  if (nrow(result) == 0) {
    return(NULL)
  }
  
  return(result)
}

#' Build complete filing URL
#'
#' @param cik Company CIK (must be padded to 10 digits)
#' @param accession_number EDGAR accession number
#' @param primary_document Primary document filename
#' @return Full URL to filing
build_filing_url <- function(cik, accession_number, primary_document) {
  # Pad CIK to 10 digits (SEC URL format requirement)
  cik_padded <- str_pad(as.character(cik), width = 10, side = "left", pad = "0")
  
  # Remove dashes from accession number for URL path
  accession_clean <- gsub("-", "", accession_number)
  
  # Construct URL
  url <- glue(
    "https://www.sec.gov/Archives/edgar/data/",
    "{cik_padded}/",
    "{accession_clean}/",
    "{primary_document}"
  )
  
  return(url)
}

#' Download filing content
#'
#' @param url Filing URL
#' @param user_agent User agent string
#' @return Raw text content or NULL on error
download_filing <- function(url, user_agent) {
  response <- GET(
    url,
    user_agent(user_agent),
    timeout(60)
  )
  
  if (status_code(response) != 200) {
    return(NULL)
  }
  
  content <- content(response, as = "text", encoding = "UTF-8")
  return(content)
}

#' Extract CIK from various formats
#'
#' @param cik_string CIK as string (may have leading zeros or whitespace)
#' @return Cleaned CIK string
clean_cik <- function(cik_string) {
  cik_string %>%
    as.character() %>%
    str_trim() %>%
    str_remove_all("[^0-9]") %>%
    str_remove("^0+") %>%  # Remove leading zeros
    as.character()
}
