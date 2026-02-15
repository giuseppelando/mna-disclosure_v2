# =============================================================================
# src/20_clean/03_apply_sample_restrictions.R
# =============================================================================
# SAMPLE RESTRICTIONS FOR M&A RESEARCH - BLUEPRINT ALIGNED
#
# DESIGN LOGIC:
# -------------
# The research has TWO distinct analyses:
#   1. PREMIUM ANALYSIS: requires SDC pre-calculated premium (1-week window)
#   2. COMPLETION ANALYSIS: requires only terminal outcome (completed/withdrawn)
#
# PREMIUM DATA DECISION:
# ----------------------
# Analysis showed that SDC pre-calculated premium and manually calculated
# premium (offer_price / target_price - 1) match within ±0.1% for 99.9% of
# deals. Therefore, we use SDC pre-calculated premium for:
#   - Methodological consistency (SDC's documented approach)
#   - Comparability with literature
#   - Avoidance of edge cases (stock splits, data entry errors)
#
# Premium availability is a DATA QUALITY FLAG, not a sample restriction.
# All deals meeting base criteria enter the sample; has_premium flags
# eligibility for premium analysis.
#
# Input:  data/interim/deals_ingested.rds
# Output: data/processed/deals_restricted.rds
#         data/processed/deals_restricted.xlsx
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(glue)
  library(openxlsx)
})

# =============================================================================
# CONFIGURATION
# =============================================================================

config <- list(
  # Period bounds (Item 1A mandatory from Dec 2005 → 2006 filings onward)
  year_min = 2006,
  year_max = 2023,
  
  # Control transfer: no arbitrary threshold per blueprint
  minority_stake_threshold = NA,
  
  # Deal value: no minimum threshold per blueprint section 2.4
  min_deal_value = NA,
  
  # Premium window: 1-week is standard in M&A literature
  # (Alternatives: 1-day, 4-weeks - available for robustness)
  premium_window = "1_week"
)

# =============================================================================
# SETUP LOGGING
# =============================================================================

log_file <- "data/interim/sample_restrictions_log.txt"
dir.create("data/interim", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

log_conn <- file(log_file, "w")

log_msg <- function(msg, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  full_msg <- glue("[{timestamp}] [{level}] {msg}")
  writeLines(full_msg, log_conn)
  message(full_msg)
}

log_msg(paste(rep("=", 70), collapse = ""))
log_msg("SAMPLE RESTRICTION CASCADE - BLUEPRINT ALIGNED")
log_msg("Design: Base sample for COMPLETION + SDC premium flag for PREMIUM")
log_msg(paste(rep("=", 70), collapse = ""))

# =============================================================================
# STEP 1: LOAD DATA
# =============================================================================

log_msg("")
log_msg("STEP 1: Loading data...")
log_msg(paste(rep("-", 70), collapse = ""))

if (file.exists("data/interim/deals_ingested.rds")) {
  df <- readRDS("data/interim/deals_ingested.rds")
  log_msg("Loaded: deals_ingested.rds")
} else {
  stop("No input file found. Run ingestion first.")
}

n_start <- nrow(df)
log_msg(glue("Starting sample: {n_start} deals"))
log_msg(glue("Columns: {ncol(df)}"))

# =============================================================================
# STEP 2: IDENTIFY AND STANDARDIZE KEY COLUMNS
# =============================================================================

log_msg("")
log_msg("STEP 2: Identifying key columns...")
log_msg(paste(rep("-", 70), collapse = ""))

# --- Stake columns ---
final_stake_col <- names(df)[str_detect(names(df), regex("percentage_of_shares_acquired_in_transaction|percent.*acquired|final.*stake", ignore_case = TRUE))][1]

if (!is.na(final_stake_col)) {
  log_msg(glue("Found final stake: {final_stake_col}"))
  if (final_stake_col != "final_stake_pct") {
    df <- df %>% rename(final_stake_pct = !!sym(final_stake_col))
  }
} else {
  log_msg("No final stake column found", "WARN")
  df$final_stake_pct <- NA_real_
}

# --- Deal value ---
deal_value_col <- names(df)[str_detect(names(df), regex("deal_value_usd_millions|deal.*value.*usd.*million", ignore_case = TRUE))][1]
if (!is.na(deal_value_col) && deal_value_col != "deal_value_usd_millions") {
  df <- df %>% rename(deal_value_usd_millions = !!sym(deal_value_col))
  log_msg(glue("Renamed {deal_value_col} → deal_value_usd_millions"))
}

# --- Year announced ---
if ("date_announced" %in% names(df) && !("year_announced" %in% names(df))) {
  df <- df %>%
    mutate(year_announced = as.integer(format(date_announced, "%Y")))
  log_msg("Created year_announced from date_announced")
}

# --- SDC Premium columns ---
premium_col_map <- list(
  "1_day" = "premium_paid_1_day_prior_to_announcement",
  "1_week" = "premium_paid_1_week_prior_to_announcement",
  "4_weeks" = "premium_paid_4_weeks_prior_to_announcement"
)

# Check which premium columns exist
premium_cols_present <- list()
for (window in names(premium_col_map)) {
  col <- premium_col_map[[window]]
  if (col %in% names(df)) {
    premium_cols_present[[window]] <- col
    n_avail <- sum(!is.na(df[[col]]))
    log_msg(glue("SDC premium ({window}): {n_avail} deals ({round(100*n_avail/nrow(df),1)}%)"))
  }
}

# Primary premium column (configurable)
primary_premium_col <- premium_col_map[[config$premium_window]]
if (!primary_premium_col %in% names(df)) {
  stop(glue("Primary premium column not found: {primary_premium_col}"))
}
log_msg(glue("Primary premium window: {config$premium_window} → {primary_premium_col}"))

# =============================================================================
# STEP 3: RESTRICTION CASCADE (BASE SAMPLE)
# =============================================================================

log_msg("")
log_msg("STEP 3: Applying restriction cascade (BASE SAMPLE)...")
log_msg("Note: Premium is a DATA QUALITY FLAG, not a restriction")
log_msg(paste(rep("-", 70), collapse = ""))

cascade <- data.frame(
  step = character(),
  rule = character(),
  description = character(),
  n_before = integer(),
  n_after = integer(),
  n_removed = integer(),
  pct_of_initial = numeric(),
  stringsAsFactors = FALSE
)

add_step <- function(step_num, rule_ref, desc, n_b, n_a) {
  cascade <<- rbind(cascade, data.frame(
    step = as.character(step_num),
    rule = rule_ref,
    description = desc,
    n_before = n_b,
    n_after = n_a,
    n_removed = n_b - n_a,
    pct_of_initial = round(100 * n_a / n_start, 1),
    stringsAsFactors = FALSE
  ))
}

df_working <- df
add_step(0, "-", "Initial sample", n_start, n_start)

# --------------------------------------------------------------------------
# FILTER 1: Terminal outcomes only (DL-03)
# --------------------------------------------------------------------------
log_msg("")
log_msg("FILTER 1: Terminal outcomes only (DL-03)...")

n_before <- nrow(df_working)

if ("deal_completed" %in% names(df_working)) {
  df_working <- df_working %>% filter(!is.na(deal_completed))
} else if ("deal_status" %in% names(df_working)) {
  df_working <- df_working %>%
    filter(str_to_lower(str_trim(deal_status)) %in% c("completed", "withdrawn"))
}

n_after <- nrow(df_working)
add_step(1, "DL-03", "Terminal outcomes only", n_before, n_after)
log_msg(glue("  Removed: {n_before - n_after}  |  Remaining: {n_after}"))

# --------------------------------------------------------------------------
# FILTER 2: Control transfer screen (DL-09)
# --------------------------------------------------------------------------
log_msg("")
log_msg("FILTER 2: Control transfer screen (DL-09)...")

n_before <- nrow(df_working)

if (!is.na(config$minority_stake_threshold)) {
  df_working <- df_working %>%
    filter(is.na(final_stake_pct) | final_stake_pct >= config$minority_stake_threshold)
}

n_after <- nrow(df_working)
add_step(2, "DL-09", "Control transfer screen", n_before, n_after)
log_msg(glue("  Removed: {n_before - n_after}  |  Remaining: {n_after}"))

# --------------------------------------------------------------------------
# FILTER 3: US public targets (DL-01)
# --------------------------------------------------------------------------
log_msg("")
log_msg("FILTER 3: US public targets (DL-01)...")

n_before <- nrow(df_working)

if ("target_nation" %in% names(df_working)) {
  df_working <- df_working %>%
    filter(str_detect(str_to_upper(str_trim(target_nation)), "^US$|^USA$|UNITED STATES"))
}

n_after <- nrow(df_working)
add_step(3, "DL-01", "US public targets", n_before, n_after)
log_msg(glue("  Removed: {n_before - n_after}  |  Remaining: {n_after}"))

# --------------------------------------------------------------------------
# FILTER 4: Announcement date required (DL-04)
# --------------------------------------------------------------------------
log_msg("")
log_msg("FILTER 4: Announcement date required (DL-04)...")

n_before <- nrow(df_working)

if ("date_announced" %in% names(df_working)) {
  df_working <- df_working %>% filter(!is.na(date_announced))
}

n_after <- nrow(df_working)
add_step(4, "DL-04", "Announcement date required", n_before, n_after)
log_msg(glue("  Removed: {n_before - n_after}  |  Remaining: {n_after}"))

# --------------------------------------------------------------------------
# FILTER 5: Payment method required (DL-10)
# --------------------------------------------------------------------------
log_msg("")
log_msg("FILTER 5: Payment method required (DL-10)...")

n_before <- nrow(df_working)

if ("payment_method_clean" %in% names(df_working)) {
  df_working <- df_working %>% filter(!is.na(payment_method_clean))
} else if (all(c("percentage_of_cash", "percentage_of_stock") %in% names(df_working))) {
  df_working <- df_working %>%
    filter(!is.na(percentage_of_cash) | !is.na(percentage_of_stock))
}

n_after <- nrow(df_working)
add_step(5, "DL-10", "Payment method required", n_before, n_after)
log_msg(glue("  Removed: {n_before - n_after}  |  Remaining: {n_after}"))

# --------------------------------------------------------------------------
# FILTER 6: Industry identifiers required (DL-11)
# --------------------------------------------------------------------------
log_msg("")
log_msg("FILTER 6: Industry identifiers required (DL-11)...")

n_before <- nrow(df_working)

sic_cols <- names(df_working)[str_detect(names(df_working), regex("target.*sic|sic.*target", ignore_case = TRUE))]
if (length(sic_cols) > 0) {
  df_working <- df_working %>% filter(!is.na(.data[[sic_cols[1]]]))
  log_msg(glue("  Using: {sic_cols[1]}"))
}

n_after <- nrow(df_working)
add_step(6, "DL-11", "Industry identifiers required", n_before, n_after)
log_msg(glue("  Removed: {n_before - n_after}  |  Remaining: {n_after}"))

# --------------------------------------------------------------------------
# FILTER 7: Period 2006-2023 (DL-18)
# --------------------------------------------------------------------------
log_msg("")
log_msg(glue("FILTER 7: Period {config$year_min}-{config$year_max} (DL-18)..."))

n_before <- nrow(df_working)

if ("year_announced" %in% names(df_working)) {
  df_working <- df_working %>%
    filter(year_announced >= config$year_min, year_announced <= config$year_max)
}

n_after <- nrow(df_working)
add_step(7, "DL-18", glue("Period {config$year_min}-{config$year_max}"), n_before, n_after)
log_msg(glue("  Removed: {n_before - n_after}  |  Remaining: {n_after}"))

# --------------------------------------------------------------------------
# FILTER 8: Deal value available (Section 2.4 - no minimum)
# --------------------------------------------------------------------------
log_msg("")
log_msg("FILTER 8: Deal value available (Section 2.4)...")

n_before <- nrow(df_working)

if ("deal_value_usd_millions" %in% names(df_working)) {
  df_working <- df_working %>% filter(!is.na(deal_value_usd_millions))
}

n_after <- nrow(df_working)
add_step(8, "2.4", "Deal value available", n_before, n_after)
log_msg(glue("  Removed: {n_before - n_after}  |  Remaining: {n_after}"))

# =============================================================================
# STEP 4: CREATE ANALYSIS ELIGIBILITY FLAGS
# =============================================================================

log_msg("")
log_msg("STEP 4: Creating analysis eligibility flags...")
log_msg(paste(rep("-", 70), collapse = ""))

# --- Premium flags (SDC pre-calculated) ---
df_working <- df_working %>%
  mutate(
    # Primary premium (1-week, standard in literature)
    has_premium = !is.na(.data[[primary_premium_col]]),
    premium_1w = .data[[primary_premium_col]],
    
    # Alternative windows (for robustness)
    has_premium_1d = if ("premium_paid_1_day_prior_to_announcement" %in% names(.)) 
      !is.na(premium_paid_1_day_prior_to_announcement) else FALSE,
    has_premium_4w = if ("premium_paid_4_weeks_prior_to_announcement" %in% names(.)) 
      !is.na(premium_paid_4_weeks_prior_to_announcement) else FALSE,
    
    # Completion (all base sample deals are eligible)
    has_completion_outcome = TRUE,
    
    # Identifiers (for CIK matching)
    has_ticker = if ("target_ticker" %in% names(.)) !is.na(target_ticker) else FALSE,
    has_cusip = if ("target_cusip" %in% names(.)) !is.na(target_cusip) else FALSE
  )

# Analysis eligibility summary
eligibility <- df_working %>%
  summarise(
    n_total = n(),
    n_completion = sum(has_completion_outcome),
    n_premium = sum(has_premium),
    n_premium_1d = sum(has_premium_1d),
    n_premium_4w = sum(has_premium_4w),
    n_ticker = sum(has_ticker),
    n_cusip = sum(has_cusip)
  )

log_msg("")
log_msg("Analysis eligibility:")
log_msg(glue("  COMPLETION analysis: {eligibility$n_completion} deals (100% of base sample)"))
log_msg(glue("  PREMIUM analysis:    {eligibility$n_premium} deals ({round(100*eligibility$n_premium/eligibility$n_total,1)}%)"))
log_msg(glue("    └─ 1-week window (primary)"))
log_msg(glue("  Premium robustness windows:"))
log_msg(glue("    └─ 1-day:   {eligibility$n_premium_1d} deals"))
log_msg(glue("    └─ 4-weeks: {eligibility$n_premium_4w} deals"))

# =============================================================================
# STEP 5: PREPARE FOR CIK MATCHING
# =============================================================================

log_msg("")
log_msg("STEP 5: Preparing for CIK matching...")
log_msg(paste(rep("-", 70), collapse = ""))

if (!("target_cik" %in% names(df_working))) {
  df_working <- df_working %>% mutate(target_cik = NA_character_)
  log_msg("Added target_cik column (empty)")
}

df_working <- df_working %>% arrange(target_name)
log_msg("Sorted by target_name")

log_msg(glue("  Ticker available: {round(100*eligibility$n_ticker/eligibility$n_total,1)}%"))
log_msg(glue("  CUSIP available:  {round(100*eligibility$n_cusip/eligibility$n_total,1)}%"))

# =============================================================================
# STEP 6: SAVE OUTPUTS
# =============================================================================

log_msg("")
log_msg("STEP 6: Saving outputs...")
log_msg(paste(rep("-", 70), collapse = ""))

output_rds <- "data/processed/deals_restricted.rds"
saveRDS(df_working, output_rds)
log_msg(glue("✓ Saved: {output_rds}"))
log_msg(glue("  Rows: {nrow(df_working)}  |  Columns: {ncol(df_working)}"))

output_xlsx <- "data/processed/deals_restricted.xlsx"
write.xlsx(df_working, output_xlsx, overwrite = TRUE)
log_msg(glue("✓ Saved: {output_xlsx}"))

cascade_csv <- "data/interim/sample_cascade_blueprint.csv"
write.csv(cascade, cascade_csv, row.names = FALSE)
log_msg(glue("✓ Saved: {cascade_csv}"))

# =============================================================================
# STEP 7: FINAL SUMMARY
# =============================================================================

log_msg("")
log_msg(paste(rep("=", 70), collapse = ""))
log_msg("SAMPLE RESTRICTION CASCADE COMPLETE")
log_msg(paste(rep("=", 70), collapse = ""))

log_msg("")
log_msg("Cascade summary:")
for (i in 2:nrow(cascade)) {
  log_msg(glue("  [{cascade$rule[i]}] {cascade$description[i]}"))
  log_msg(glue("       Removed: {cascade$n_removed[i]}  |  Retained: {cascade$pct_of_initial[i]}%"))
}

log_msg("")
log_msg(glue("BASE SAMPLE: {nrow(df_working)} deals ({round(100*nrow(df_working)/n_start,1)}% retention)"))
log_msg(glue("  → COMPLETION analysis: {eligibility$n_completion} deals"))
log_msg(glue("  → PREMIUM analysis:    {eligibility$n_premium} deals (SDC 1-week)"))

# Sample characteristics
log_msg("")
log_msg("Sample characteristics:")
if ("deal_value_usd_millions" %in% names(df_working)) {
  log_msg(glue("  Deal value - Median: ${round(median(df_working$deal_value_usd_millions, na.rm=T),1)}M"))
}
if ("year_announced" %in% names(df_working)) {
  log_msg(glue("  Year range: {min(df_working$year_announced, na.rm=T)} - {max(df_working$year_announced, na.rm=T)}"))
}
if ("deal_completed" %in% names(df_working)) {
  n_completed <- sum(df_working$deal_completed == TRUE, na.rm = TRUE)
  n_withdrawn <- sum(df_working$deal_completed == FALSE, na.rm = TRUE)
  log_msg(glue("  Completed: {n_completed} ({round(100*n_completed/nrow(df_working),1)}%)"))
  log_msg(glue("  Withdrawn: {n_withdrawn} ({round(100*n_withdrawn/nrow(df_working),1)}%)"))
}

# Premium characteristics (for eligible deals only)
df_premium <- df_working %>% filter(has_premium)
if (nrow(df_premium) > 0) {
  log_msg("")
  log_msg("Premium characteristics (eligible deals only):")
  log_msg(glue("  Premium - Median: {round(median(df_premium$premium_1w, na.rm=T),1)}%"))
  log_msg(glue("  Premium - IQR: [{round(quantile(df_premium$premium_1w, 0.25, na.rm=T),1)}%, {round(quantile(df_premium$premium_1w, 0.75, na.rm=T),1)}%]"))
}

close(log_conn)

# =============================================================================
# CONSOLE SUMMARY
# =============================================================================

cat("\n")
cat("╔═══════════════════════════════════════════════════════════════════════╗\n")
cat("║        SAMPLE RESTRICTIONS - BLUEPRINT ALIGNED - COMPLETE            ║\n")
cat("╠═══════════════════════════════════════════════════════════════════════╣\n")
cat(sprintf("║ Initial sample:        %-47d ║\n", n_start))
cat(sprintf("║ BASE SAMPLE:           %-47d ║\n", nrow(df_working)))
cat(sprintf("║ Retention:             %.1f%%%46s ║\n", 100*nrow(df_working)/n_start, ""))
cat("╠═══════════════════════════════════════════════════════════════════════╣\n")
cat("║ Analysis eligibility:                                                 ║\n")
cat(sprintf("║   COMPLETION analysis: %-5d deals (100%%)                          ║\n", 
            eligibility$n_completion))
cat(sprintf("║   PREMIUM analysis:    %-5d deals (%.1f%%) [SDC 1-week]             ║\n", 
            eligibility$n_premium,
            100*eligibility$n_premium/eligibility$n_total))
cat("╠═══════════════════════════════════════════════════════════════════════╣\n")
cat("║ Filters applied (BASE SAMPLE):                                        ║\n")
for (i in 2:nrow(cascade)) {
  cat(sprintf("║ [%-5s] %-44s -%6d ║\n",
              cascade$rule[i],
              substr(cascade$description[i], 1, 44),
              cascade$n_removed[i]))
}
cat("╠═══════════════════════════════════════════════════════════════════════╣\n")
cat("║ Premium methodology:                                                  ║\n")
cat("║   • Using SDC pre-calculated premium (1-week window)                 ║\n")
cat("║   • Consistent with literature; avoids edge cases                    ║\n")
cat("║   • Alternative windows available for robustness                     ║\n")
cat("╠═══════════════════════════════════════════════════════════════════════╣\n")
cat("║ Outputs:                                                              ║\n")
cat("║   • deals_restricted.rds   (base sample + eligibility flags)         ║\n")
cat("║   • deals_restricted.xlsx  (for manual CIK matching)                 ║\n")
cat("╚═══════════════════════════════════════════════════════════════════════╝\n")
cat("\n")
