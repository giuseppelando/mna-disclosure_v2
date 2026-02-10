# ==============================================================================
# Pipeline Configuration
# All parameters for 10-K download and parsing pipeline
# ==============================================================================

# Detect project root reliably
# Look for the directory containing 'config', 'src', 'scripts' folders
if (!exists("PROJECT_ROOT")) {
  # Start from current working directory
  current_dir <- getwd()
  
  # Check if we're already in project root (has config/, src/, scripts/)
  if (dir.exists(file.path(current_dir, "config")) && 
      dir.exists(file.path(current_dir, "src")) && 
      dir.exists(file.path(current_dir, "scripts"))) {
    PROJECT_ROOT <- current_dir
  } else {
    # If not, assume we need to go up or this is being sourced from scripts/
    # Try parent directory
    parent_dir <- dirname(current_dir)
    if (dir.exists(file.path(parent_dir, "config")) && 
        dir.exists(file.path(parent_dir, "src")) && 
        dir.exists(file.path(parent_dir, "scripts"))) {
      PROJECT_ROOT <- parent_dir
    } else {
      # Fallback: use current directory
      PROJECT_ROOT <- current_dir
    }
  }
}

# Paths (all relative to PROJECT_ROOT)
PATHS <- list(
  # Input
  deals_matched = file.path(PROJECT_ROOT, "data/interim/deals_with_cik_matches.xlsx"),
  
  # Outputs
  deals_clean = file.path(PROJECT_ROOT, "data/interim/deals_clean.rds"),
  edgar_index = file.path(PROJECT_ROOT, "data/interim/edgar_10k_index.rds"),
  deals_filing_matched = file.path(PROJECT_ROOT, "data/interim/deals_filing_matched.rds"),
  raw_10k_dir = file.path(PROJECT_ROOT, "data/raw/10k_filings"),
  parsed_sections = file.path(PROJECT_ROOT, "data/interim/parsed_sections.rds"),
  final_dataset = file.path(PROJECT_ROOT, "data/processed/deals_with_10k_text.rds"),
  
  # Logs
  log_dir = file.path(PROJECT_ROOT, "output/logs")
)

# Timing parameters
TIMING <- list(
  lag_min = 90,              # Minimum days between filing and announcement
  lag_robustness = c(60, 120) # Alternative lags for robustness checks
)

# EDGAR API settings
EDGAR <- list(
  user_agent = "Your Name your.email@example.com",  # REQUIRED by SEC
  rate_limit_per_second = 10,   # SEC allows 10 requests/second
  max_retries = 3,
  retry_delay_seconds = 2
)

# Sample period for EDGAR index
SAMPLE_PERIOD <- list(
  start_date = "2005-01-01",  # Item 1A Risk Factors became mandatory in 2005
  end_date = "2024-12-31"
)

# Section parsing settings
PARSING <- list(
  sections = c("MD&A", "Risk Factors"),
  save_raw_files = TRUE,         # Keep original 10-K files
  max_section_chars = 500000     # Safety limit for section length
)

# Validation thresholds
VALIDATION <- list(
  min_mda_words = 100,           # Minimum words to consider section valid
  min_risk_words = 100,
  max_mda_words = 200000,        # Maximum words (detect parsing errors)
  max_risk_words = 100000
)
