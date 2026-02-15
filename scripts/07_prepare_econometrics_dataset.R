# ==============================================================================
# 07_prepare_econometrics_dataset.R
# 
# PURPOSE: Transform the "final" M&A dataset (with computed textual indices)
#          into an econometrics-ready analysis dataset.
#
# DESIGN PRINCIPLES:
#   - Pattern-based column detection (avoid hardcoding exact names)
#   - Layered, explainable exclusion rules with reason codes
#   - Full audit trail: counts before/after each step
#   - No silent data loss: all transformations logged
#   - Produce sample flags for premium, completion, duration analyses
#
# INPUTS:  data/final/analysis_dataset.rds (matched to pre-announcement 10-K)
# OUTPUTS: In-memory objects ready for econometric estimation
#
# DEPENDENCIES: tidyverse, lubridate, janitor
#
# VERSION: 1.1 - Fixed column detection patterns
# ==============================================================================

library(tidyverse)
library(lubridate)
library(janitor)

# ==============================================================================
# CONFIGURATION
# ==============================================================================

# --- File path (modify for your system) ---
INPUT_PATH <- "data/final/analysis_dataset.rds"

# --- Exclusion patterns for non-M&A transactions (case-insensitive regex) ---
# These identify transaction types that are NOT control-transfer M&A deals
NON_MA_PATTERNS <- paste0(
  "repurchase|buyback|self[- ]?tender|issuer[- ]?tender|",
  "recap(?:italizat)?|restructur|spin[- ]?off|split[- ]?off|",
  "minority|stake[- ]?purchase|investment|exchange[- ]?offer|",
  "open[- ]?market|privately[- ]?negotiated"
)

# --- Premium sanity bounds (for flagging, not auto-winsorizing) ---
PREMIUM_BOUNDS <- list(
  lower = -50,   # Flag premiums below -50%
  upper = 300    # Flag premiums above 300%
)

# --- Duration sanity check ---
MIN_DURATION_DAYS <- 0  # Minimum acceptable duration (negative = error)

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

#' Find columns matching any of the given patterns (case-insensitive)
#' @param col_names Character vector of column names
#' @param patterns Character vector of regex patterns to match
#' @return Character vector of matching column names
find_cols <- function(col_names, patterns) {
  pattern_combined <- paste0("(", paste(patterns, collapse = "|"), ")")
  matches <- col_names[str_detect(col_names, regex(pattern_combined, ignore_case = TRUE))]
  return(matches)
}

#' Find a single column matching patterns; return NA if none or multiple
#' Uses preference order and exclusion patterns for disambiguation
#' @param col_names Character vector of column names
#' @param patterns Character vector of regex patterns (in preference order)
#' @param exclude Optional regex patterns to exclude from matches
#' @return Single column name or NA
find_col_single <- function(col_names, patterns, exclude = NULL) {
  # Try each pattern in order of preference

for (pat in patterns) {
    matches <- col_names[str_detect(col_names, regex(pat, ignore_case = TRUE))]
    
    # Apply exclusion filter if provided
    if (!is.null(exclude) && length(matches) > 0) {
      exclude_combined <- paste0("(", paste(exclude, collapse = "|"), ")")
      matches <- matches[!str_detect(matches, regex(exclude_combined, ignore_case = TRUE))]
    }
    
    if (length(matches) == 1) return(matches)
    if (length(matches) > 1) {
      # Prefer exact match if available
      exact <- matches[str_to_lower(matches) == str_to_lower(str_remove_all(pat, "\\^|\\$"))]
      if (length(exact) == 1) return(exact)
      # Return first match
      return(matches[1])
    }
  }
  return(NA_character_)
}

#' Safely parse dates from various formats
#' @param x Character, Date, or numeric vector
#' @return Date vector with NA for unparseable values
safe_parse_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  
  # Check if numeric (not a date)
  if (is.numeric(x)) {
    warning("Attempted to parse numeric column as date; returning NA")
    return(rep(NA_Date_, length(x)))
  }
  
  # Try common formats
  parsed <- suppressWarnings(as.Date(x))  # ISO format first
  
  # Try alternative formats for remaining NAs
  na_idx <- is.na(parsed) & !is.na(x)
  if (any(na_idx)) {
    # Try Y-m-d with time
    alt1 <- suppressWarnings(as.Date(x[na_idx], format = "%Y-%m-%dT%H:%M:%SZ"))
    parsed[na_idx] <- coalesce(alt1, parsed[na_idx])
  }
  
  return(parsed)
}

#' Standardize deal status to a minimal set
#' @param status Character vector of raw status values
#' @return Character vector: "Completed", "Withdrawn", or "Pending/Unknown"
standardize_status <- function(status) {
  status_lower <- str_to_lower(status)
  
  case_when(
    str_detect(status_lower, "complet|effect|clos|success") ~ "Completed",
    str_detect(status_lower, "withdraw|terminat|cancel|fail|reject") ~ "Withdrawn",
    str_detect(status_lower, "pending|rumor|intent|propos") ~ "Pending/Unknown",
    is.na(status) ~ "Pending/Unknown",
    TRUE ~ "Pending/Unknown"
  )
}

#' Detect textual index columns by pattern
#' Excludes raw text columns and word counts
#' @param col_names Character vector of column names
#' @return Character vector of detected index column names
detect_indices <- function(col_names) {
  # Patterns for textual indices
  index_patterns <- c(
    "tone", "forward", "specificity", "uncertainty", 
    "tfidf", "lm_", "_lm$", "sentiment", "readab", "fog",
    "operational", "disclosure.*norm"
  )
  
  candidates <- find_cols(col_names, index_patterns)
  
  # Exclude raw text fields and word counts
  exclude_patterns <- "_text$|_raw_text|word_count|_count$"
  candidates <- candidates[!str_detect(candidates, regex(exclude_patterns, ignore_case = TRUE))]
  
  return(candidates)
}

#' Detect risk-related index columns (separate from text)
#' @param col_names Character vector of column names
#' @return Character vector of risk index column names
detect_risk_indices <- function(col_names) {
  # Risk disclosure indices (not raw text)
  risk_patterns <- c("risk_disclosure", "risk.*tfidf", "risk.*norm")
  candidates <- find_cols(col_names, risk_patterns)
  
  # Exclude text fields
  exclude_patterns <- "_text$|word_count"
  candidates <- candidates[!str_detect(candidates, regex(exclude_patterns, ignore_case = TRUE))]
  
  return(candidates)
}

#' Detect deal control columns by pattern
#' @param col_names Character vector of column names
#' @return Character vector of detected control column names
detect_controls <- function(col_names) {
  control_patterns <- c(
    "payment|cash|stock", "tender", "hostil|attitude", "toehold|prior",
    "deal_value|rank_value", "cross_border", "competing", "related",
    "financial_advisor", "sic", "industry", "nation", "state",
    "stake", "technique", "form_of"
  )
  candidates <- find_cols(col_names, control_patterns)
  
  # Exclude price columns (not controls)
  exclude_patterns <- "share_price|price_per"
  candidates <- candidates[!str_detect(candidates, regex(exclude_patterns, ignore_case = TRUE))]
  
  return(candidates)
}

#' Build the keep-set of columns dynamically
#' @param df Data frame
#' @param key_col Name of the key column
#' @return Character vector of columns to keep
build_keep_set <- function(df, key_col) {
  all_cols <- names(df)
  
  # Core identifiers
  keep <- c(key_col)
  
  # Target/acquirer identifiers (patterns)
  id_patterns <- c("^target_name", "^acqui.*name", "ticker", "cusip", "^.*_cik$")
  keep <- c(keep, find_cols(all_cols, id_patterns))
  
  # Date and timing (be specific to avoid number_of_days...)
  date_patterns <- c("^date_", "^year_", "^month_", "^days_lag$")
  keep <- c(keep, find_cols(all_cols, date_patterns))
  
  # Status and outcomes
  outcome_patterns <- c("^status", "deal_status", "complet", "^premium", "^time_to", "duration", "^event_")
  keep <- c(keep, find_cols(all_cols, outcome_patterns))
  
  # Textual indices (not text columns)
  keep <- c(keep, detect_indices(all_cols))
  keep <- c(keep, detect_risk_indices(all_cols))
  
  # Deal controls
  keep <- c(keep, detect_controls(all_cols))
  
  # Industry codes
  sic_patterns <- c("^.*sic.*$", "^.*industry$", "macro_industry", "mid_industry")
  keep <- c(keep, find_cols(all_cols, sic_patterns))
  
  # Sample flags (important for analysis)
  flag_patterns <- c("^sample_", "_outlier$", "^fe$", "_fe$")
  keep <- c(keep, find_cols(all_cols, flag_patterns))
  
  # Word counts (useful for robustness, but not text)
  keep <- c(keep, find_cols(all_cols, c("word_count")))
  
  # Accession number (for traceability)
  keep <- c(keep, find_cols(all_cols, c("accession")))
  
  # Unique and sort
  keep <- unique(keep)
  keep <- keep[keep %in% all_cols]  # Safety: only existing columns
  
  # Explicitly EXCLUDE large text columns even if they matched
  text_cols <- all_cols[str_detect(all_cols, regex("_text$|^mda_|^risk_factors_text", ignore_case = TRUE))]
  keep <- setdiff(keep, text_cols)
  
  return(keep)
}

#' Print a summary line with counts
#' @param message Description of the step
#' @param n_before Count before
#' @param n_after Count after (optional)
print_count <- function(message, n_before, n_after = NULL) {
  if (is.null(n_after)) {
    cat(sprintf("  → %s: %d\n", message, n_before))
  } else {
    cat(sprintf("  → %s: %d → %d (Δ = %d)\n", message, n_before, n_after, n_after - n_before))
  }
}

# ==============================================================================
# MAIN SCRIPT
# ==============================================================================

cat("\n")
cat("=" %>% rep(78) %>% paste(collapse = ""), "\n")
cat("ECONOMETRICS DATASET PREPARATION\n")
cat("=" %>% rep(78) %>% paste(collapse = ""), "\n")
cat(sprintf("Input: %s\n", INPUT_PATH))
cat(sprintf("Timestamp: %s\n", Sys.time()))
cat("\n")

# ==============================================================================
# STEP 1: Load and Profile
# ==============================================================================

cat("--- STEP 1: LOAD AND PROFILE ---\n\n")

# Load dataset
if (!file.exists(INPUT_PATH)) {
  stop(sprintf("Input file not found: %s", INPUT_PATH))
}

df_raw <- readRDS(INPUT_PATH)

cat(sprintf("Dimensions: %d rows × %d columns\n\n", nrow(df_raw), ncol(df_raw)))

# Column types summary
type_summary <- df_raw %>%
  summarise(across(everything(), ~class(.x)[1])) %>%
  pivot_longer(everything(), names_to = "column", values_to = "type") %>%
  count(type, name = "n_cols")

cat("Column types:\n")
for (i in 1:nrow(type_summary)) {
  cat(sprintf("  %s: %d\n", type_summary$type[i], type_summary$n_cols[i]))
}
cat("\n")

# Missingness summary (top 15 most missing)
missing_summary <- df_raw %>%
  summarise(across(everything(), ~sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "n_missing") %>%
  mutate(pct_missing = round(100 * n_missing / nrow(df_raw), 1)) %>%
  filter(n_missing > 0) %>%
  arrange(desc(n_missing)) %>%
  head(15)

if (nrow(missing_summary) > 0) {
  cat("Top columns by missingness:\n")
  for (i in 1:nrow(missing_summary)) {
    cat(sprintf("  %s: %d (%.1f%%)\n", 
                missing_summary$column[i], 
                missing_summary$n_missing[i],
                missing_summary$pct_missing[i]))
  }
  cat("\n")
}

# Candidate ID columns
id_candidates <- find_cols(names(df_raw), c("^deal_id$", "^id$", "logical_deal"))
cat(sprintf("Candidate ID columns: %s\n", paste(id_candidates, collapse = ", ")))

# Candidate date columns (specific patterns)
date_candidates <- find_cols(names(df_raw), c("^date_"))
cat(sprintf("Candidate date columns: %s\n", paste(date_candidates, collapse = ", ")))

# Candidate status columns
status_candidates <- find_cols(names(df_raw), c("deal_status", "^status$"))
cat(sprintf("Candidate status columns: %s\n", paste(status_candidates, collapse = ", ")))

# Candidate type columns
type_candidates <- find_cols(names(df_raw), c("deal_type", "acquisition_technique", "form_of"))
cat(sprintf("Candidate type columns: %s\n", paste(type_candidates, collapse = ", ")))

# Candidate index columns
index_candidates <- detect_indices(names(df_raw))
risk_indices <- detect_risk_indices(names(df_raw))
all_indices <- unique(c(index_candidates, risk_indices))
cat(sprintf("Detected textual indices (%d): %s\n", 
            length(all_indices), 
            paste(head(all_indices, 10), collapse = ", ")))
if (length(all_indices) > 10) cat("  ... and more\n")
cat("\n")

# ==============================================================================
# STEP 2: Identify Unique Key
# ==============================================================================

cat("--- STEP 2: IDENTIFY UNIQUE KEY ---\n\n")

# Prefer deal_id if present (exact match)
key_col <- NA_character_
if ("deal_id" %in% names(df_raw)) {
  key_col <- "deal_id"
} else if ("logical_deal_id" %in% names(df_raw)) {
  key_col <- "logical_deal_id"
} else if ("id" %in% names(df_raw)) {
  key_col <- "id"
}

if (is.na(key_col)) {
  # Create a row-number based key
  warning("No ID column found; creating row-based key 'row_id'")
  df_raw <- df_raw %>% mutate(row_id = row_number(), .before = 1)
  key_col <- "row_id"
}

cat(sprintf("Selected key column: %s\n", key_col))

# Check uniqueness
n_rows <- nrow(df_raw)
n_unique <- n_distinct(df_raw[[key_col]])

if (n_unique != n_rows) {
  stop(sprintf("Key column '%s' is not unique: %d unique values for %d rows. Cannot proceed.", 
               key_col, n_unique, n_rows))
}

cat(sprintf("Uniqueness verified: %d unique keys for %d rows ✓\n\n", n_unique, n_rows))

# ==============================================================================
# STEP 3: Remove Non-M&A Transactions
# ==============================================================================

cat("--- STEP 3: REMOVE NON-M&A TRANSACTIONS ---\n\n")

# Initialize exclusion log
exclusion_log <- tibble(
  key = character(),
  reason = character()
)

n_initial <- nrow(df_raw)

# Find type/category columns (explicit names)
type_col <- if ("deal_type" %in% names(df_raw)) "deal_type" else NA_character_
technique_col <- if ("acquisition_techniques" %in% names(df_raw)) "acquisition_techniques" else NA_character_

if (!is.na(type_col)) {
  cat(sprintf("Using '%s' for transaction type filtering\n", type_col))
  
  # Identify non-M&A rows
  non_ma_rows <- df_raw %>%
    filter(str_detect(.data[[type_col]], regex(NON_MA_PATTERNS, ignore_case = TRUE)))
  
  if (nrow(non_ma_rows) > 0) {
    # Log exclusions
    exclusion_log <- bind_rows(
      exclusion_log,
      tibble(
        key = as.character(non_ma_rows[[key_col]]),
        reason = "EX_NON_MA_TYPE"
      )
    )
    
    # Show breakdown of excluded types
    type_breakdown <- non_ma_rows %>%
      count(.data[[type_col]], name = "n") %>%
      arrange(desc(n))
    
    cat("\nExcluded transaction types:\n")
    for (i in 1:min(nrow(type_breakdown), 10)) {
      cat(sprintf("  %s: %d\n", type_breakdown[[type_col]][i], type_breakdown$n[i]))
    }
    
    # Apply filter
    df_work <- df_raw %>%
      filter(!str_detect(.data[[type_col]], regex(NON_MA_PATTERNS, ignore_case = TRUE)))
  } else {
    df_work <- df_raw
  }
} else {
  cat("No deal_type column found; skipping type-based exclusion\n")
  df_work <- df_raw
}

# Also check techniques column if present
if (!is.na(technique_col)) {
  cat(sprintf("\nAlso checking '%s' for non-M&A techniques\n", technique_col))
  
  technique_patterns <- "repurchase|buyback|open[- ]?market|self[- ]?tender"
  tech_non_ma <- df_work %>%
    filter(str_detect(.data[[technique_col]], regex(technique_patterns, ignore_case = TRUE)))
  
  if (nrow(tech_non_ma) > 0) {
    # Only exclude if not already excluded
    already_excluded <- tech_non_ma[[key_col]] %in% exclusion_log$key
    new_exclusions <- tech_non_ma %>% filter(!already_excluded)
    
    if (nrow(new_exclusions) > 0) {
      exclusion_log <- bind_rows(
        exclusion_log,
        tibble(
          key = as.character(new_exclusions[[key_col]]),
          reason = "EX_NON_MA_TECHNIQUE"
        )
      )
      
      cat(sprintf("  Additional exclusions from techniques: %d\n", nrow(new_exclusions)))
      
      df_work <- df_work %>%
        filter(!(.data[[key_col]] %in% new_exclusions[[key_col]]))
    }
  }
}

n_after_type <- nrow(df_work)
print_count("After type/technique filtering", n_initial, n_after_type)
cat(sprintf("  Excluded: %d rows (reason: EX_NON_MA_TYPE/TECHNIQUE)\n\n", n_initial - n_after_type))

# ==============================================================================
# STEP 4: Status Standardization and Completion Outcome
# ==============================================================================

cat("--- STEP 4: STATUS STANDARDIZATION ---\n\n")

# Find status column (explicit)
status_col <- if ("deal_status" %in% names(df_work)) "deal_status" else NA_character_

if (!is.na(status_col)) {
  cat(sprintf("Using '%s' for deal status\n", status_col))
  
  # Show raw status distribution
  raw_status_dist <- df_work %>%
    count(.data[[status_col]], name = "n") %>%
    arrange(desc(n))
  
  cat("\nRaw status distribution:\n")
  for (i in 1:nrow(raw_status_dist)) {
    cat(sprintf("  %s: %d\n", raw_status_dist[[status_col]][i], raw_status_dist$n[i]))
  }
  
  # Standardize
  df_work <- df_work %>%
    mutate(status_std = standardize_status(.data[[status_col]]))
  
  # Show standardized distribution
  std_status_dist <- df_work %>%
    count(status_std, name = "n") %>%
    arrange(desc(n))
  
  cat("\nStandardized status distribution:\n")
  for (i in 1:nrow(std_status_dist)) {
    cat(sprintf("  %s: %d\n", std_status_dist$status_std[i], std_status_dist$n[i]))
  }
  
  # Create completion_dummy
  # 1 = Completed, 0 = Withdrawn, NA = Pending/Unknown
  df_work <- df_work %>%
    mutate(
      completion_dummy = case_when(
        status_std == "Completed" ~ 1L,
        status_std == "Withdrawn" ~ 0L,
        TRUE ~ NA_integer_
      )
    )
  
  # Check if there's already a 'completion' column and compare
  if ("completion" %in% names(df_work)) {
    agree <- sum(df_work$completion == df_work$completion_dummy, na.rm = TRUE)
    disagree <- sum(df_work$completion != df_work$completion_dummy, na.rm = TRUE)
    cat(sprintf("\nComparison with existing 'completion': agree=%d, disagree=%d\n",
                agree, disagree))
    if (disagree > 0) {
      cat("  ⚠ Discrepancies exist; using newly computed completion_dummy\n")
    }
  }
  
  cat(sprintf("\ncompletion_dummy created: 1=%d, 0=%d, NA=%d\n",
              sum(df_work$completion_dummy == 1, na.rm = TRUE),
              sum(df_work$completion_dummy == 0, na.rm = TRUE),
              sum(is.na(df_work$completion_dummy))))
} else {
  cat("No deal_status column found; cannot create completion_dummy\n")
  df_work <- df_work %>% mutate(completion_dummy = NA_integer_)
}

cat("\n")

# ==============================================================================
# STEP 5: Dates and Duration Variables
# ==============================================================================

cat("--- STEP 5: DATES AND DURATION VARIABLES ---\n\n")

# Find date columns (EXPLICIT names to avoid mismatches)
announce_col <- if ("date_announced" %in% names(df_work)) "date_announced" else NA_character_
effective_col <- if ("date_effective" %in% names(df_work)) "date_effective" else NA_character_
withdrawn_col <- if ("date_withdrawn" %in% names(df_work)) "date_withdrawn" else NA_character_

# Also check for existing time_to_close (computed from SDC)
existing_ttc_col <- if ("time_to_close" %in% names(df_work)) "time_to_close" else NA_character_

cat(sprintf("Announcement date column: %s\n", ifelse(is.na(announce_col), "NOT FOUND", announce_col)))
cat(sprintf("Effective/close date column: %s\n", ifelse(is.na(effective_col), "NOT FOUND", effective_col)))
cat(sprintf("Withdrawal date column: %s\n", ifelse(is.na(withdrawn_col), "NOT FOUND", withdrawn_col)))
cat(sprintf("Existing time_to_close column: %s\n", ifelse(is.na(existing_ttc_col), "NOT FOUND", existing_ttc_col)))

# Parse dates
if (!is.na(announce_col)) {
  df_work <- df_work %>%
    mutate(announce_date_parsed = safe_parse_date(.data[[announce_col]]))
  cat(sprintf("  Parsed announcement dates: %d valid, %d NA\n",
              sum(!is.na(df_work$announce_date_parsed)),
              sum(is.na(df_work$announce_date_parsed))))
}

if (!is.na(effective_col)) {
  df_work <- df_work %>%
    mutate(effective_date_parsed = safe_parse_date(.data[[effective_col]]))
  cat(sprintf("  Parsed effective dates: %d valid, %d NA\n",
              sum(!is.na(df_work$effective_date_parsed)),
              sum(is.na(df_work$effective_date_parsed))))
}

if (!is.na(withdrawn_col)) {
  df_work <- df_work %>%
    mutate(withdrawn_date_parsed = safe_parse_date(.data[[withdrawn_col]]))
  cat(sprintf("  Parsed withdrawal dates: %d valid, %d NA\n",
              sum(!is.na(df_work$withdrawn_date_parsed)),
              sum(is.na(df_work$withdrawn_date_parsed))))
}

# Compute duration variables
# STRATEGY: Use existing time_to_close if available and valid, else compute from dates

if (!is.na(existing_ttc_col)) {
  # Use existing time_to_close for completed deals
  cat("\nUsing existing 'time_to_close' variable for completed deals\n")
  
  df_work <- df_work %>%
    mutate(
      time_to_close_days = case_when(
        completion_dummy == 1 & !is.na(.data[[existing_ttc_col]]) & .data[[existing_ttc_col]] >= 0 ~
          as.numeric(.data[[existing_ttc_col]]),
        TRUE ~ NA_real_
      )
    )
  
  close_stats <- df_work %>%
    filter(!is.na(time_to_close_days)) %>%
    summarise(
      n = n(),
      min = min(time_to_close_days),
      q25 = quantile(time_to_close_days, 0.25),
      median = median(time_to_close_days),
      q75 = quantile(time_to_close_days, 0.75),
      max = max(time_to_close_days)
    )
  
  cat(sprintf("\ntime_to_close_days (completed deals from existing variable):\n"))
  cat(sprintf("  N=%d, Min=%.0f, Q1=%.0f, Median=%.0f, Q3=%.0f, Max=%.0f\n",
              close_stats$n, close_stats$min, close_stats$q25, 
              close_stats$median, close_stats$q75, close_stats$max))
  
} else if (!is.na(announce_col) && !is.na(effective_col)) {
  # Compute from dates
  df_work <- df_work %>%
    mutate(
      time_to_close_days = case_when(
        completion_dummy == 1 & !is.na(effective_date_parsed) & !is.na(announce_date_parsed) ~
          as.numeric(effective_date_parsed - announce_date_parsed),
        TRUE ~ NA_real_
      )
    )
  
  # Check for negative durations
  n_negative_close <- sum(df_work$time_to_close_days < 0, na.rm = TRUE)
  if (n_negative_close > 0) {
    cat(sprintf("\n  ⚠ %d completed deals have negative time_to_close_days (set to NA)\n", n_negative_close))
    df_work <- df_work %>%
      mutate(time_to_close_days = if_else(time_to_close_days < 0, NA_real_, time_to_close_days))
  }
  
  close_stats <- df_work %>%
    filter(!is.na(time_to_close_days)) %>%
    summarise(
      n = n(),
      min = min(time_to_close_days),
      q25 = quantile(time_to_close_days, 0.25),
      median = median(time_to_close_days),
      q75 = quantile(time_to_close_days, 0.75),
      max = max(time_to_close_days)
    )
  
  if (close_stats$n > 0) {
    cat(sprintf("\ntime_to_close_days (computed from dates):\n"))
    cat(sprintf("  N=%d, Min=%.0f, Q1=%.0f, Median=%.0f, Q3=%.0f, Max=%.0f\n",
                close_stats$n, close_stats$min, close_stats$q25, 
                close_stats$median, close_stats$q75, close_stats$max))
  } else {
    cat("\n  ⚠ Could not compute time_to_close_days from dates\n")
  }
} else {
  cat("\n  ⚠ Cannot compute time_to_close_days: missing required date columns\n")
  df_work <- df_work %>% mutate(time_to_close_days = NA_real_)
}

# Survival-ready variables: time_to_event_days, event_completed
# For completed: use time_to_close_days
# For withdrawn: compute from announcement to withdrawal date

if (!is.na(announce_col)) {
  df_work <- df_work %>%
    mutate(
      # Duration for withdrawn deals (from announcement to withdrawal)
      time_to_withdrawn_days = case_when(
        completion_dummy == 0 & !is.na(withdrawn_date_parsed) & !is.na(announce_date_parsed) ~
          as.numeric(withdrawn_date_parsed - announce_date_parsed),
        TRUE ~ NA_real_
      ),
      # Combined time-to-event (close for completed, withdrawn for withdrawn)
      time_to_event_days = case_when(
        completion_dummy == 1 ~ time_to_close_days,
        completion_dummy == 0 ~ time_to_withdrawn_days,
        TRUE ~ NA_real_
      ),
      # Event indicator (1 = completed, 0 = censored/withdrawn for Cox)
      event_completed = case_when(
        completion_dummy == 1 & !is.na(time_to_event_days) ~ 1L,
        completion_dummy == 0 & !is.na(time_to_event_days) ~ 0L,  # Withdrawn = censored
        TRUE ~ NA_integer_
      )
    )
  
  # Check for negative durations
  n_negative_event <- sum(df_work$time_to_event_days < 0, na.rm = TRUE)
  if (n_negative_event > 0) {
    cat(sprintf("\n  ⚠ %d deals have negative time_to_event_days (set to NA)\n", n_negative_event))
    df_work <- df_work %>%
      mutate(
        time_to_event_days = if_else(time_to_event_days < 0, NA_real_, time_to_event_days),
        event_completed = if_else(is.na(time_to_event_days), NA_integer_, event_completed)
      )
  }
  
  event_stats <- df_work %>%
    filter(!is.na(time_to_event_days)) %>%
    summarise(
      n = n(),
      n_completed = sum(event_completed == 1, na.rm = TRUE),
      n_withdrawn = sum(event_completed == 0, na.rm = TRUE),
      median_days = median(time_to_event_days)
    )
  
  cat(sprintf("\nSurvival variables created:\n"))
  cat(sprintf("  N with valid duration: %d (completed=%d, withdrawn=%d)\n",
              event_stats$n, event_stats$n_completed, event_stats$n_withdrawn))
  cat(sprintf("  Median time to event: %.0f days\n", event_stats$median_days))
}

cat("\n")

# ==============================================================================
# STEP 6: Premium Outcome
# ==============================================================================

cat("--- STEP 6: PREMIUM OUTCOME ---\n\n")

# Find existing premium variables (be specific)
premium_cols <- c("premium", "premium_1w", "premium_4w") %>%
  intersect(names(df_work))
cat(sprintf("Found premium columns: %s\n", paste(premium_cols, collapse = ", ")))

# Use 'premium' as primary (this is typically the standard 1-week premium)
# Prefer exact 'premium' column first
main_premium_col <- if ("premium" %in% names(df_work)) "premium" else 
                    if ("premium_1w" %in% names(df_work)) "premium_1w" else NA_character_

if (!is.na(main_premium_col)) {
  cat(sprintf("\nUsing '%s' as primary premium variable\n", main_premium_col))
  
  # Rename to standardized name
  df_work <- df_work %>%
    mutate(premium_pct = .data[[main_premium_col]])
  
  # Premium statistics
  premium_stats <- df_work %>%
    filter(!is.na(premium_pct)) %>%
    summarise(
      n = n(),
      min = min(premium_pct),
      p01 = quantile(premium_pct, 0.01),
      p05 = quantile(premium_pct, 0.05),
      p25 = quantile(premium_pct, 0.25),
      median = median(premium_pct),
      mean = mean(premium_pct),
      p75 = quantile(premium_pct, 0.75),
      p95 = quantile(premium_pct, 0.95),
      p99 = quantile(premium_pct, 0.99),
      max = max(premium_pct)
    )
  
  cat(sprintf("\nPremium distribution (N=%d):\n", premium_stats$n))
  cat(sprintf("  Min: %.2f%%\n", premium_stats$min))
  cat(sprintf("  P1/P5: %.2f%% / %.2f%%\n", premium_stats$p01, premium_stats$p05))
  cat(sprintf("  Q1/Median/Q3: %.2f%% / %.2f%% / %.2f%%\n", 
              premium_stats$p25, premium_stats$median, premium_stats$p75))
  cat(sprintf("  Mean: %.2f%%\n", premium_stats$mean))
  cat(sprintf("  P95/P99: %.2f%% / %.2f%%\n", premium_stats$p95, premium_stats$p99))
  cat(sprintf("  Max: %.2f%%\n", premium_stats$max))
  
  # Flag extremes (informational only, no winsorizing)
  n_low <- sum(df_work$premium_pct < PREMIUM_BOUNDS$lower, na.rm = TRUE)
  n_high <- sum(df_work$premium_pct > PREMIUM_BOUNDS$upper, na.rm = TRUE)
  
  if (n_low > 0 || n_high > 0) {
    cat(sprintf("\n  ⚠ Extreme premiums flagged (not excluded):\n"))
    cat(sprintf("    Below %.0f%%: %d deals\n", PREMIUM_BOUNDS$lower, n_low))
    cat(sprintf("    Above %.0f%%: %d deals\n", PREMIUM_BOUNDS$upper, n_high))
  }
  
} else {
  cat("\n  ⚠ No premium column found\n")
  df_work <- df_work %>% mutate(premium_pct = NA_real_)
}

cat("\n")

# ==============================================================================
# STEP 7: Column Reduction
# ==============================================================================

cat("--- STEP 7: COLUMN REDUCTION ---\n\n")

n_cols_initial <- ncol(df_work)

# Build keep set dynamically
keep_cols <- build_keep_set(df_work, key_col)

# Add our newly created columns
new_cols <- c("status_std", "completion_dummy", "announce_date_parsed", "effective_date_parsed",
              "withdrawn_date_parsed", "time_to_close_days", "time_to_event_days", 
              "event_completed", "time_to_withdrawn_days", "premium_pct")
keep_cols <- unique(c(keep_cols, intersect(new_cols, names(df_work))))

cat(sprintf("Keep set: %d columns identified\n", length(keep_cols)))

# Identify columns to drop
all_cols <- names(df_work)
drop_candidates <- setdiff(all_cols, keep_cols)

# Categorize drops
drop_reasons <- tibble(column = drop_candidates) %>%
  mutate(
    reason = case_when(
      # Large text fields
      str_detect(column, regex("_text$|mda_text|risk_factors_text", ignore_case = TRUE)) ~ "large_text",
      # Merge artifacts
      str_detect(column, "\\.(x|y)$") ~ "merge_duplicate",
      # Likely internal/temp columns
      str_detect(column, "^(tmp_|temp_|_)") ~ "temp_column",
      TRUE ~ "not_needed"
    )
  )

# Report drops by reason
drop_summary <- drop_reasons %>% count(reason, name = "n_cols")
cat("\nColumns to drop:\n")
for (i in 1:nrow(drop_summary)) {
  cat(sprintf("  %s: %d\n", drop_summary$reason[i], drop_summary$n_cols[i]))
}

# Show large text columns being dropped
text_dropped <- drop_reasons %>% filter(reason == "large_text") %>% pull(column)
if (length(text_dropped) > 0) {
  cat(sprintf("\n  Large text columns dropped: %s\n", paste(text_dropped, collapse = ", ")))
}

# Check for all-NA or single-unique columns in keep set
all_na_cols <- keep_cols[sapply(df_work[keep_cols], function(x) all(is.na(x)))]
single_val_cols <- keep_cols[sapply(df_work[keep_cols], function(x) n_distinct(x, na.rm = TRUE) <= 1)]

if (length(all_na_cols) > 0) {
  cat(sprintf("\n  ⚠ All-NA columns in keep set (will drop): %s\n", paste(all_na_cols, collapse = ", ")))
  keep_cols <- setdiff(keep_cols, all_na_cols)
}

if (length(single_val_cols) > 0) {
  cat(sprintf("  ⚠ Single-value columns in keep set (will drop): %s\n", paste(single_val_cols, collapse = ", ")))
  keep_cols <- setdiff(keep_cols, single_val_cols)
}

# Apply column reduction
df_clean <- df_work %>% select(all_of(keep_cols))

cat(sprintf("\nColumn reduction: %d → %d columns\n", n_cols_initial, ncol(df_clean)))

cat("\n")

# ==============================================================================
# STEP 8: Create Sample Flags and Final Datasets
# ==============================================================================

cat("--- STEP 8: SAMPLE FLAGS AND FINAL OBJECTS ---\n\n")

# Create sample flags based on outcome definability
df_final <- df_clean %>%
  mutate(
    # Premium analysis: need non-missing premium
    premium_defined = !is.na(premium_pct),
    
    # Completion analysis: need definite outcome (not pending)
    completion_defined = !is.na(completion_dummy),
    
    # Duration analysis: need time_to_event and event indicator
    duration_defined = !is.na(time_to_event_days) & !is.na(event_completed) & time_to_event_days >= 0
  )

# Summary counts
cat("Sample flags created:\n")
cat(sprintf("  premium_defined: %d (%.1f%%)\n", 
            sum(df_final$premium_defined), 
            100 * mean(df_final$premium_defined)))
cat(sprintf("  completion_defined: %d (%.1f%%)\n", 
            sum(df_final$completion_defined), 
            100 * mean(df_final$completion_defined)))
cat(sprintf("  duration_defined: %d (%.1f%%)\n", 
            sum(df_final$duration_defined), 
            100 * mean(df_final$duration_defined)))

# Completion breakdown
cat("\nCompletion breakdown (for completion-defined sample):\n")
completion_breakdown <- df_final %>%
  filter(completion_defined) %>%
  count(completion_dummy, name = "n") %>%
  mutate(pct = round(100 * n / sum(n), 1))

for (i in 1:nrow(completion_breakdown)) {
  status_label <- ifelse(completion_breakdown$completion_dummy[i] == 1, "Completed", "Withdrawn")
  cat(sprintf("  %s: %d (%.1f%%)\n", status_label, 
              completion_breakdown$n[i], completion_breakdown$pct[i]))
}

# Duration breakdown
if (sum(df_final$duration_defined) > 0) {
  cat("\nDuration breakdown (for duration-defined sample):\n")
  duration_breakdown <- df_final %>%
    filter(duration_defined) %>%
    count(event_completed, name = "n") %>%
    mutate(pct = round(100 * n / sum(n), 1))
  
  for (i in 1:nrow(duration_breakdown)) {
    status_label <- ifelse(duration_breakdown$event_completed[i] == 1, "Completed (event)", "Withdrawn (censored)")
    cat(sprintf("  %s: %d (%.1f%%)\n", status_label, 
                duration_breakdown$n[i], duration_breakdown$pct[i]))
  }
}

# Summary statistics for indices and outcomes
cat("\nSummary statistics for key variables:\n")

# Textual indices
index_cols <- detect_indices(names(df_final))
risk_idx <- detect_risk_indices(names(df_final))
all_idx <- unique(c(index_cols, risk_idx))

if (length(all_idx) > 0) {
  cat("\n  Textual indices:\n")
  for (col in head(all_idx, 8)) {
    if (is.numeric(df_final[[col]])) {
      stats <- df_final %>%
        filter(!is.na(.data[[col]])) %>%
        summarise(
          n = n(),
          mean = round(mean(.data[[col]]), 3),
          sd = round(sd(.data[[col]]), 3)
        )
      cat(sprintf("    %s: N=%d, Mean=%.3f, SD=%.3f\n", col, stats$n, stats$mean, stats$sd))
    }
  }
}

# Outcomes
cat("\n  Outcomes:\n")
if ("premium_pct" %in% names(df_final)) {
  prem_stats <- df_final %>%
    filter(premium_defined) %>%
    summarise(n = n(), mean = round(mean(premium_pct), 2), sd = round(sd(premium_pct), 2))
  cat(sprintf("    premium_pct: N=%d, Mean=%.2f, SD=%.2f\n", prem_stats$n, prem_stats$mean, prem_stats$sd))
}

if ("time_to_close_days" %in% names(df_final)) {
  ttc_stats <- df_final %>%
    filter(!is.na(time_to_close_days)) %>%
    summarise(n = n(), mean = round(mean(time_to_close_days), 1), sd = round(sd(time_to_close_days), 1))
  cat(sprintf("    time_to_close_days: N=%d, Mean=%.1f, SD=%.1f\n", ttc_stats$n, ttc_stats$mean, ttc_stats$sd))
}

if ("time_to_event_days" %in% names(df_final)) {
  tte_stats <- df_final %>%
    filter(duration_defined) %>%
    summarise(n = n(), mean = round(mean(time_to_event_days), 1), sd = round(sd(time_to_event_days), 1))
  cat(sprintf("    time_to_event_days: N=%d, Mean=%.1f, SD=%.1f\n", tte_stats$n, tte_stats$mean, tte_stats$sd))
}

cat("\n")

# ==============================================================================
# FINAL SUMMARY
# ==============================================================================

cat("=" %>% rep(78) %>% paste(collapse = ""), "\n")
cat("FINAL SUMMARY\n")
cat("=" %>% rep(78) %>% paste(collapse = ""), "\n\n")

cat(sprintf("Initial dataset: %d rows × %d columns\n", nrow(df_raw), ncol(df_raw)))
cat(sprintf("After exclusions: %d rows\n", nrow(df_final)))
cat(sprintf("Final dataset: %d rows × %d columns\n", nrow(df_final), ncol(df_final)))
cat(sprintf("Rows excluded: %d (%.1f%%)\n", 
            nrow(df_raw) - nrow(df_final), 
            100 * (nrow(df_raw) - nrow(df_final)) / nrow(df_raw)))

cat("\nExclusion log summary:\n")
excl_summary <- exclusion_log %>% count(reason, name = "n")
if (nrow(excl_summary) > 0) {
  for (i in 1:nrow(excl_summary)) {
    cat(sprintf("  %s: %d\n", excl_summary$reason[i], excl_summary$n[i]))
  }
} else {
  cat("  (No explicit exclusions logged)\n")
}

cat("\nSample sizes by analysis type:\n")
cat(sprintf("  Premium analyses: %d deals\n", sum(df_final$premium_defined)))
cat(sprintf("  Completion analyses: %d deals\n", sum(df_final$completion_defined)))
cat(sprintf("  Duration analyses: %d deals\n", sum(df_final$duration_defined)))

# ==============================================================================
# OUTPUT OBJECTS (in-memory)
# ==============================================================================

# Main dataset
econ_dataset <- df_final

# Subsets for specific analyses
econ_premium <- df_final %>% filter(premium_defined)
econ_completion <- df_final %>% filter(completion_defined)
econ_duration <- df_final %>% filter(duration_defined)

# Exclusion log (for audit)
exclusion_audit <- exclusion_log

cat("\n")
cat("=" %>% rep(78) %>% paste(collapse = ""), "\n")
cat("IN-MEMORY OBJECTS CREATED\n")
cat("=" %>% rep(78) %>% paste(collapse = ""), "\n\n")

cat(sprintf("  econ_dataset      : Full cleaned dataset (%d × %d)\n", nrow(econ_dataset), ncol(econ_dataset)))
cat(sprintf("  econ_premium      : Premium analysis sample (%d × %d)\n", nrow(econ_premium), ncol(econ_premium)))
cat(sprintf("  econ_completion   : Completion analysis sample (%d × %d)\n", nrow(econ_completion), ncol(econ_completion)))
cat(sprintf("  econ_duration     : Duration analysis sample (%d × %d)\n", nrow(econ_duration), ncol(econ_duration)))
cat(sprintf("  exclusion_audit   : Exclusion log (%d rows)\n", nrow(exclusion_audit)))

cat("\n")
cat("Script completed successfully.\n")
cat(sprintf("Timestamp: %s\n", Sys.time()))
cat("\n")

# Save to data/processed/
saveRDS(econ_dataset, "data/processed/econ_dataset.rds")
saveRDS(econ_premium, "data/processed/econ_premium.rds")
saveRDS(econ_completion, "data/processed/econ_completion.rds")
saveRDS(econ_duration, "data/processed/econ_duration.rds")
saveRDS(exclusion_audit, "data/processed/exclusion_audit.rds")

# Or save all in one list
saveRDS(
  list(
    full = econ_dataset,
    premium = econ_premium,
    completion = econ_completion,
    duration = econ_duration,
    audit = exclusion_audit
  ),
  "data/processed/econ_analysis_datasets.rds"
)

# ==============================================================================
# END OF SCRIPT
# ==============================================================================
