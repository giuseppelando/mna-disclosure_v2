# =============================================================================
# src/10_ingest/02_ingest_deals_final.R (FIXED VERSION)
# =============================================================================
# FIXES APPLIED:
# 1. Explicit handling of number_of_days column to preserve integer type
# 2. SDC missing value (-999) properly converted to NA
# 3. Added withdrawn flag for logit analysis
# =============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(lubridate)
  library(tidyr)
  library(janitor)
  library(glue)
  library(jsonlite)
})

# =============================================================================
# SETUP LOGGING
# =============================================================================

log_file <- "data/interim/ingestion_log.txt"
dir.create("data/interim", recursive = TRUE, showWarnings = FALSE)
log_conn <- file(log_file, "w")

log_msg <- function(msg, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  full_msg <- glue("[{timestamp}] [{level}] {msg}")
  writeLines(full_msg, log_conn)
  message(full_msg)
}

log_msg(paste(rep("=", 70), collapse = ""))
log_msg("M&A DATA INGESTION - FIXED VERSION")
log_msg(paste(rep("=", 70), collapse = ""))

# =============================================================================
# STEP 1: LOAD MAPPING
# =============================================================================

log_msg("", "INFO")
log_msg("STEP 1: Loading column mapping...", "INFO")
log_msg(paste(rep("-", 70), collapse = ""))

mapping_file <- "data/interim/research_mapping.csv"

if (!file.exists(mapping_file)) {
  log_msg("ERROR: Mapping file not found!", "ERROR")
  log_msg("Please run: source('src/10_ingest/analyze_columns.R')", "ERROR")
  stop("Run analyze_columns.R first to generate column mapping")
}

research_mapping <- read.csv(mapping_file, stringsAsFactors = FALSE)
log_msg(glue("Loaded mapping for {nrow(research_mapping)} columns"))

# Keep all mapped columns + unmapped with <50% missing
columns_to_keep <- research_mapping %>%
  filter(construct != "unmapped" | (construct == "unmapped" & pct_missing < 50)) %>%
  arrange(desc(priority == "CRITICAL"), desc(priority == "HIGH"), pct_missing)

log_msg(glue("Will keep {nrow(columns_to_keep)} columns"))

# =============================================================================
# STEP 2: LOAD RAW DATA
# =============================================================================

log_msg("", "INFO")
log_msg("STEP 2: Loading raw data...", "INFO")
log_msg(paste(rep("-", 70), collapse = ""))

raw_files <- list.files("data/raw", pattern = "\\.xlsx$", full.names = TRUE)
# Exclude temp files (Excel lock files)
raw_files <- raw_files[!str_detect(basename(raw_files), "^~\\$")]
if (length(raw_files) == 0) stop("No .xlsx files in data/raw/")

raw_path <- raw_files[1]
log_msg(glue("Input: {basename(raw_path)}"))

df_raw <- read_excel(raw_path, sheet = 1, guess_max = 10000)
log_msg(glue("Loaded: {nrow(df_raw)} rows × {ncol(df_raw)} columns"))

df_clean <- clean_names(df_raw)

# =============================================================================
# STEP 3: RENAME COLUMNS
# =============================================================================

log_msg("", "INFO")
log_msg("STEP 3: Renaming columns...", "INFO")
log_msg(paste(rep("-", 70), collapse = ""))

# Simple renaming: only core identifiers
rename_column <- function(col_name, construct) {
  if (str_detect(construct, "identifier_deal")) return("deal_id")
  if (str_detect(construct, "identifier_target") && str_detect(col_name, "full.*name")) return("target_name")
  if (str_detect(construct, "identifier_target") && str_detect(col_name, "ticker")) return("target_ticker")
  if (str_detect(construct, "identifier_target") && str_detect(col_name, "cusip")) return("target_cusip")
  if (str_detect(construct, "identifier_acquirer") && str_detect(col_name, "full.*name")) return("acquirer_name")
  if (str_detect(construct, "outcome_completion") && str_detect(col_name, "^deal.*status")) return("deal_status")
  if (str_detect(construct, "control_deal_type") && str_detect(col_name, "^deal.*type")) return("deal_type")
  return(col_name)  # Keep original for everything else
}

name_mapping <- columns_to_keep %>%
  mutate(
    standard_name = map2_chr(column_name, construct, rename_column),
    original_name = column_name
  ) %>%
  select(original_name, standard_name, construct, priority)

# Handle duplicates: keep best one
dup_check <- name_mapping %>% count(standard_name) %>% filter(n > 1)

if (nrow(dup_check) > 0) {
  for (dup_name in dup_check$standard_name) {
    dup_rows <- name_mapping %>%
      filter(standard_name == dup_name) %>%
      arrange(desc(priority == "CRITICAL"), desc(priority == "HIGH"))
    
    best_original <- dup_rows$original_name[1]
    log_msg(glue("Duplicate '{dup_name}': keeping {best_original}"), "DEBUG")
    
    name_mapping <- name_mapping %>%
      filter(!(standard_name == dup_name & original_name != best_original))
  }
}

# Apply
df_selected <- df_clean %>% select(all_of(name_mapping$original_name))
names(df_selected) <- name_mapping$standard_name

log_msg(glue("Selected {ncol(df_selected)} columns"))
log_msg(glue("Renamed {sum(name_mapping$original_name != name_mapping$standard_name)} columns"))

# =============================================================================
# STEP 4: DATA TYPE CONVERSION (FIXED VERSION)
# =============================================================================

log_msg("", "INFO")
log_msg("STEP 4: Converting data types (FIXED VERSION)...", "INFO")
log_msg(paste(rep("-", 70), collapse = ""))

# -------- SDC Missing Value Code --------
SDC_MISSING_VALUE <- -999L

# -------- helpers --------
excel_origin <- as.Date("1899-12-30")

coerce_to_date <- function(v) {
  if (inherits(v, "Date")) return(v)
  if (inherits(v, "POSIXt")) return(as.Date(v))
  
  # numeric: interpret as Excel date serial
  if (is.numeric(v)) return(as.Date(v, origin = excel_origin))
  
  # character: try strict ISO first
  ch <- as.character(v)
  out <- suppressWarnings(as.Date(ch, format = "%Y-%m-%d"))
  if (sum(!is.na(out)) > 0.5 * length(out)) return(out)
  
  formats <- c("%d/%m/%Y", "%m/%d/%Y", "%Y%m%d", "%d-%b-%Y", "%Y/%m/%d")
  for (fmt in formats) {
    tmp <- suppressWarnings(as.Date(ch, format = fmt))
    if (sum(!is.na(tmp)) > 0.5 * length(tmp)) return(tmp)
  }
  
  suppressWarnings(lubridate::as_date(v))
}

# FIX: Simpler, more robust integer conversion for duration columns
coerce_to_days_integer <- function(v) {
  # If already numeric (which includes integer), just convert to integer
  if (is.numeric(v)) {
    out <- as.integer(round(v))
    # Convert SDC missing value to NA
    out[out == SDC_MISSING_VALUE] <- NA_integer_
    return(out)
  }
  
  # If character, try numeric parse
  if (is.character(v) || is.factor(v)) {
    ch <- as.character(v)
    num <- suppressWarnings(as.numeric(str_replace_all(ch, ",", ".")))
    out <- as.integer(round(num))
    out[out == SDC_MISSING_VALUE] <- NA_integer_
    return(out)
  }
  
  # Fallback: all NA
  rep(NA_integer_, length(v))
}

coerce_to_numeric <- function(v) {
  if (inherits(v, c("Date", "POSIXt"))) return(v) # never coerce dates here
  if (is.numeric(v)) return(v)
  ch <- as.character(v)
  suppressWarnings(as.numeric(str_replace_all(ch, ",", ".")))
}

# -------- column classification rules --------

# 1) duration columns: explicitly identify by name pattern
# FIX: More precise pattern to avoid false matches
duration_cols <- names(df_selected)[
  str_detect(names(df_selected), 
             "^number_of_days|days_between|time_to_close")
]

# 2) date columns: contain "date" but are NOT duration columns
date_cols <- setdiff(
  names(df_selected)[str_detect(names(df_selected), "^date|_date$")],
  duration_cols
)

# 3) numeric columns (financial values)
numeric_patterns <- c("value", "price", "percent", "percentage", "stake", 
                      "shares", "outstanding", "premium", "ratio")
numeric_cols <- setdiff(
  names(df_selected)[str_detect(str_to_lower(names(df_selected)), 
                                str_c(numeric_patterns, collapse="|"))],
  c(date_cols, duration_cols)
)

log_msg(glue("Duration columns: {paste(duration_cols, collapse = ', ')}"))
log_msg(glue("Date columns: {length(date_cols)}"))
log_msg(glue("Numeric columns: {length(numeric_cols)}"))

# -------- apply conversions --------

# IMPORTANT: Process duration columns FIRST before any date conversion
# This prevents integer columns from being incorrectly interpreted as dates
for (col in duration_cols) {
  before_class <- paste(class(df_selected[[col]]), collapse = "/")
  before_sample <- paste(head(df_selected[[col]], 3), collapse = ", ")
  
  df_selected[[col]] <- coerce_to_days_integer(df_selected[[col]])
  
  after_class <- paste(class(df_selected[[col]]), collapse = "/")
  after_sample <- paste(head(df_selected[[col]], 3), collapse = ", ")
  n_nonmiss <- sum(!is.na(df_selected[[col]]))
  
  log_msg(glue("  {col}: {before_class} -> {after_class}"), "DEBUG")
  log_msg(glue("    Before: {before_sample}"), "DEBUG")
  log_msg(glue("    After: {after_sample} (nonmiss={n_nonmiss})"), "DEBUG")
}

# Dates second
for (col in date_cols) {
  before_class <- paste(class(df_selected[[col]]), collapse = "/")
  df_selected[[col]] <- coerce_to_date(df_selected[[col]])
  after_class <- paste(class(df_selected[[col]]), collapse = "/")
  n_parsed <- sum(!is.na(df_selected[[col]]))
  log_msg(glue("  {col}: {before_class} -> {after_class}, parsed={n_parsed}"), "DEBUG")
}

# Other numerics
for (col in numeric_cols) {
  if (!inherits(df_selected[[col]], c("Date", "POSIXt", "numeric", "integer"))) {
    before_class <- paste(class(df_selected[[col]]), collapse = "/")
    df_selected[[col]] <- coerce_to_numeric(df_selected[[col]])
    after_class <- paste(class(df_selected[[col]]), collapse = "/")
    n_numeric <- sum(!is.na(df_selected[[col]]))
    if (n_numeric > 0) log_msg(glue("  {col}: {before_class} -> {after_class}"), "DEBUG")
  }
}

# Trim character columns
char_cols <- names(df_selected)[sapply(df_selected, is.character)]
for (col in char_cols) df_selected[[col]] <- str_trim(df_selected[[col]])

log_msg("Type conversion complete")

# =============================================================================
# STEP 5: DEDUPLICATION
# =============================================================================

log_msg("", "INFO")
log_msg("STEP 5: Removing duplicates...", "INFO")
log_msg(paste(rep("-", 70), collapse = ""))

n_before <- nrow(df_selected)
if ("deal_id" %in% names(df_selected)) {
  df_selected <- df_selected %>% distinct(deal_id, .keep_all = TRUE)
} else {
  df_selected <- df_selected %>% distinct()
}
n_dupes <- n_before - nrow(df_selected)

log_msg(glue("Removed {n_dupes} duplicates"))
log_msg(glue("Final: {nrow(df_selected)} unique deals"))

# =============================================================================
# STEP 6: CREATE DERIVED VARIABLES (FIXED)
# =============================================================================

log_msg("", "INFO")
log_msg("STEP 6: Creating derived variables...", "INFO")
log_msg(paste(rep("-", 70), collapse = ""))

# Find relevant columns dynamically
announce_col <- names(df_selected)[str_detect(names(df_selected), "date.*announced")][1]
complete_col <- names(df_selected)[str_detect(names(df_selected), "date.*effective")][1]
withdrawn_col <- names(df_selected)[str_detect(names(df_selected), "date.*withdrawn")][1]
status_col <- names(df_selected)[str_detect(names(df_selected), "deal.*status")][1]
sic_col <- names(df_selected)[str_detect(names(df_selected), "sic.*code|primary.*sic")][1]

# Deal outcomes - FIXED: Added explicit withdrawn flag
if (!is.na(status_col)) {
  df_selected <- df_selected %>%
    mutate(
      deal_status_clean = str_to_lower(str_trim(.data[[status_col]])),
      
      # Binary: 1 = completed, 0 = withdrawn
      deal_completed = case_when(
        deal_status_clean == "completed" ~ 1L,
        deal_status_clean == "withdrawn" ~ 0L,
        TRUE ~ NA_integer_
      ),
      
      # ADDED: Explicit withdrawn flag for logit analysis
      withdrawn = case_when(
        deal_status_clean == "withdrawn" ~ 1L,
        deal_status_clean == "completed" ~ 0L,
        TRUE ~ NA_integer_
      ),
      
      deal_outcome_terminal = deal_status_clean %in% c("completed", "withdrawn")
    ) %>%
    select(-deal_status_clean)
  
  log_msg("Created: deal_completed, withdrawn, deal_outcome_terminal", "DEBUG")
  
  # Summary
  n_completed <- sum(df_selected$deal_completed == 1, na.rm = TRUE)
  n_withdrawn <- sum(df_selected$withdrawn == 1, na.rm = TRUE)
  log_msg(glue("  Completed: {n_completed}"))
  log_msg(glue("  Withdrawn: {n_withdrawn}"))
}

# Time to close
if (!is.na(announce_col) && !is.na(complete_col)) {
  df_selected <- df_selected %>%
    mutate(
      time_to_close = as.integer(difftime(.data[[complete_col]], .data[[announce_col]], units = 'days'))
    )
  log_msg("Created: time_to_close", "DEBUG")
}

# Industry classifications
if (!is.na(sic_col)) {
  df_selected <- df_selected %>%
    mutate(
      target_sic_primary = str_extract(.data[[sic_col]], "^\\d{4}"),
      target_sic_2digit = str_sub(target_sic_primary, 1, 2)
    )
  log_msg("Created: target_sic_primary, target_sic_2digit", "DEBUG")
}

# Time indicators
if (!is.na(announce_col)) {
  df_selected <- df_selected %>%
    mutate(
      year_announced = year(.data[[announce_col]]),
      month_announced = month(.data[[announce_col]])
    )
  log_msg("Created: year_announced, month_announced", "DEBUG")
}

# Industry × Year FE
if (all(c("target_sic_2digit", "year_announced") %in% names(df_selected))) {
  df_selected <- df_selected %>%
    mutate(industry_year = paste0(target_sic_2digit, "_", year_announced))
  log_msg("Created: industry_year", "DEBUG")
}

# Payment method
if (all(c("percentage_of_cash", "percentage_of_stock") %in% names(df_selected))) {
  df_selected <- df_selected %>%
    mutate(
      payment_method_clean = case_when(
        percentage_of_cash == 100 ~ "Cash",
        percentage_of_stock == 100 ~ "Stock",
        percentage_of_cash > 0 & percentage_of_stock > 0 ~ "Mixed",
        percentage_of_cash + percentage_of_stock > 0 ~ "Other",
        TRUE ~ NA_character_
      )
    )
  log_msg("Created: payment_method_clean", "DEBUG")
}

# =============================================================================
# STEP 7: CREATE SAMPLE FILTER FLAGS
# =============================================================================

log_msg("", "INFO")
log_msg("STEP 7: Creating sample filter flags...", "INFO")
log_msg(paste(rep("-", 70), collapse = ""))

df_selected <- df_selected %>%
  mutate(
    flag_has_deal_id = !is.na(deal_id),
    flag_has_target_name = if("target_name" %in% names(.)) !is.na(target_name) else FALSE,
    flag_has_announce_date = if(!is.na(announce_col)) !is.na(.data[[announce_col]]) else FALSE,
    flag_terminal_outcome = if("deal_outcome_terminal" %in% names(.)) deal_outcome_terminal else FALSE,
    flag_has_premium_inputs = {
      price_cols <- names(.)[str_detect(names(.), "price")]
      has_offer <- any(str_detect(price_cols, "paid|acquir"))
      has_target <- any(str_detect(price_cols, "target.*prior"))
      has_offer & has_target
    }
  )

flag_cols <- names(df_selected)[str_starts(names(df_selected), "flag_")]
flag_summary <- df_selected %>%
  summarise(across(all_of(flag_cols), ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "flag", values_to = "count") %>%
  mutate(pct = round(100 * count / nrow(df_selected), 1))

log_msg("Flags created:")
for (i in 1:nrow(flag_summary)) {
  log_msg(glue("  {flag_summary$flag[i]}: {flag_summary$count[i]} ({flag_summary$pct[i]}%)"), "DEBUG")
}

log_msg(glue("Final dataset: {nrow(df_selected)} rows × {ncol(df_selected)} columns"))

# =============================================================================
# STEP 8: VALIDATION
# =============================================================================

log_msg("", "INFO")
log_msg("STEP 8: Validation checks...", "INFO")
log_msg(paste(rep("-", 70), collapse = ""))

# Check number_of_days column is integer
days_col <- names(df_selected)[str_detect(names(df_selected), "number_of_days")][1]
if (!is.na(days_col)) {
  log_msg(glue("Checking {days_col}:"))
  log_msg(glue("  Class: {paste(class(df_selected[[days_col]]), collapse = '/')}"))
  log_msg(glue("  Sample: {paste(head(df_selected[[days_col]], 5), collapse = ', ')}"))
  
  if (!is.integer(df_selected[[days_col]])) {
    log_msg("  WARNING: Column is not integer type!", "WARN")
  } else {
    log_msg("  ✓ Column is correctly typed as integer")
  }
}

# Check withdrawn flag
if ("withdrawn" %in% names(df_selected)) {
  log_msg("Checking withdrawn flag:")
  log_msg(glue("  Class: {class(df_selected$withdrawn)}"))
  tbl <- table(df_selected$withdrawn, useNA = "always")
  log_msg(glue("  Completed (0): {tbl['0']}"))
  log_msg(glue("  Withdrawn (1): {tbl['1']}"))
}

# =============================================================================
# STEP 9: SAVE OUTPUTS
# =============================================================================

log_msg("", "INFO")
log_msg("STEP 9: Saving outputs...", "INFO")
log_msg(paste(rep("-", 70), collapse = ""))

saveRDS(df_selected, "data/interim/deals_ingested.rds")
log_msg("Saved: data/interim/deals_ingested.rds")

write.csv(name_mapping, "data/interim/kept_columns.csv", row.names = FALSE)
log_msg("Saved: data/interim/kept_columns.csv")

summary_stats <- list(
  date = as.character(Sys.Date()),
  input = basename(raw_path),
  n_rows = nrow(df_selected),
  n_cols = ncol(df_selected),
  n_completed = sum(df_selected$deal_completed == 1, na.rm = TRUE),
  n_withdrawn = sum(df_selected$withdrawn == 1, na.rm = TRUE)
)

write_json(summary_stats, "data/interim/ingestion_summary.json", pretty = TRUE, auto_unbox = TRUE)
log_msg("Saved: data/interim/ingestion_summary.json")

# =============================================================================
# FINALIZE
# =============================================================================

log_msg("")
log_msg(paste(rep("=", 70), collapse = ""))
log_msg("INGESTION COMPLETE (FIXED VERSION)")
log_msg(glue("Dataset: {nrow(df_selected)} deals × {ncol(df_selected)} columns"))
log_msg(paste(rep("=", 70), collapse = ""))

close(log_conn)

# Summary
cat("\n")
cat("╔══════════════════════════════════════════════════════════════════╗\n")
cat("║              INGESTION COMPLETE (FIXED VERSION)                  ║\n")
cat("╠══════════════════════════════════════════════════════════════════╣\n")
cat(sprintf("║ File:       %-52s ║\n", basename(raw_path)))
cat(sprintf("║ Deals:      %-52d ║\n", nrow(df_selected)))
cat(sprintf("║ Columns:    %-52d ║\n", ncol(df_selected)))
cat("╠══════════════════════════════════════════════════════════════════╣\n")
cat(sprintf("║ Completed:  %-52d ║\n", sum(df_selected$deal_completed == 1, na.rm = TRUE)))
cat(sprintf("║ Withdrawn:  %-52d ║\n", sum(df_selected$withdrawn == 1, na.rm = TRUE)))
cat("╠══════════════════════════════════════════════════════════════════╣\n")
cat("║ FIXES APPLIED:                                                   ║\n")
cat("║   ✓ number_of_days column preserved as integer                   ║\n")
cat("║   ✓ SDC missing value (-999) converted to NA                     ║\n")
cat("║   ✓ withdrawn flag added for logit analysis                      ║\n")
cat("╠══════════════════════════════════════════════════════════════════╣\n")
cat("║ Output: data/interim/deals_ingested.rds                          ║\n")
cat("╚══════════════════════════════════════════════════════════════════╝\n")
cat("\n")
