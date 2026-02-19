# ==============================================================================
# create_lm_dictionaries_v3.R
# ==============================================================================
# Builds analysis_config.rds in the exact structure expected by
# 03_construct_indices_UPDATED_v2.R
#
# Expected by Module 3:
#   config$lm_positive            : character vector
#   config$lm_negative            : character vector
#   config$lm_uncertainty         : character vector
#   config$lm_litigious           : character vector
#   config$lm_optional            : named list (possibly empty)
#   config$forward_looking_extended : character vector
#   config$operational_phrases    : character vector
#   config$negation_words         : character vector
#   config$params                 : list(weight_numeric, weight_compounds,
#                                       min_cell_size, outlier_threshold)
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
})

cat("=== BUILDING analysis_config.rds (LM + project dictionaries) ===\n")

# ---------------------------------------------------------------------------
# PROJECT ROOT AND PATHS
# ---------------------------------------------------------------------------

project_root <- "C:/Users/giuse/Documents/GitHub/mna-disclosure"  # adjust if needed

lm_csv_path <- file.path(project_root, "config", "lm", "LM_MasterDictionary.csv")
if (!file.exists(lm_csv_path)) {
  stop("LM CSV not found at: ", lm_csv_path)
}

config_dir  <- file.path(project_root, "config")
config_path <- file.path(config_dir, "analysis_config.rds")
dir.create(config_dir, recursive = TRUE, showWarnings = FALSE)

cat("Project root: ", project_root, "\n", sep = "")
cat("LM CSV path:  ", lm_csv_path, "\n", sep = "")

# ---------------------------------------------------------------------------
# LOAD LM MASTER DICTIONARY
# ---------------------------------------------------------------------------

cat("Loading LM Master Dictionary...\n")
lm_raw <- read.csv(lm_csv_path, stringsAsFactors = FALSE)
names(lm_raw) <- tolower(names(lm_raw))

# Identify word column
word_col <- which(names(lm_raw) %in% c("word", "term"))
if (length(word_col) == 0) {
  stop("Could not find a 'word' or 'term' column in the LM CSV.")
}
if (!"word" %in% names(lm_raw)) {
  names(lm_raw)[word_col[1]] <- "word"
}
lm_raw$word <- tolower(lm_raw$word)

get_lm_terms <- function(df, flag_candidates) {
  col_id <- which(names(df) %in% flag_candidates)
  if (length(col_id) == 0) {
    warning("No column found for flags: ", paste(flag_candidates, collapse = ", "))
    return(character(0))
  }
  col_id <- col_id[1]
  vals <- suppressWarnings(as.numeric(df[[col_id]]))
  vals[is.na(vals)] <- 0
  terms <- df$word[vals > 0]
  terms <- unique(terms[nchar(terms) > 0])
  terms
}

lm_positive    <- get_lm_terms(lm_raw, c("positive", "positiv"))
lm_negative    <- get_lm_terms(lm_raw, c("negative", "negativ"))
lm_uncertainty <- get_lm_terms(lm_raw, c("uncertainty", "uncert"))
lm_litigious   <- get_lm_terms(lm_raw, c("litigious", "litig"))

cat("LM Positive terms:     ", length(lm_positive), "\n", sep = "")
cat("LM Negative terms:     ", length(lm_negative), "\n", sep = "")
cat("LM Uncertainty terms:  ", length(lm_uncertainty), "\n", sep = "")
cat("LM Litigious terms:    ", length(lm_litigious), "\n", sep = "")

if (all(c(length(lm_positive),
          length(lm_negative),
          length(lm_uncertainty),
          length(lm_litigious)) == 0)) {
  stop("All LM dictionaries are empty. Check LM CSV columns (positive, negative, uncertainty, litigious).")
}

# Optional LM categories if you want later (empty for now)
lm_optional <- list()

# ---------------------------------------------------------------------------
# PROJECT-SPECIFIC DICTIONARIES
# ---------------------------------------------------------------------------

# Forward-looking dictionary (token-level, not regex; can extend later)
forward_looking_extended <- c(
  "anticipate", "anticipated", "anticipates",
  "forecast", "forecasted", "forecasting", "forecasts",
  "expect", "expects", "expected",
  "project", "projects", "projected", "projecting",
  "plan", "plans", "planned", "planning",
  "intend", "intends", "intended", "intending",
  "aim", "aims", "aimed", "aiming",
  "target", "targets", "targeted", "targeting",
  "outlook", "guidance",
  "will", "would", "shall",
  "future", "forthcoming", "upcoming",
  "next"
)
forward_looking_extended <- unique(tolower(forward_looking_extended))

# Operational phrases for compounding (lowercase)
operational_phrases <- c(
  "operating income",
  "net income",
  "gross margin",
  "gross profit",
  "operating margin",
  "operating profit",
  "operating loss",
  "operating cash flow",
  "cash flow",
  "free cash flow",
  "capital expenditure",
  "capital expenditures",
  "capital spending",
  "capital investment",
  "return on assets",
  "return on equity",
  "return on capital",
  "return on invested capital",
  "interest expense",
  "interest income",
  "net interest income",
  "provision for loan losses",
  "loan loss provision",
  "nonperforming loans",
  "non-performing loans",
  "total assets",
  "total liabilities",
  "net sales",
  "net revenue",
  "operating expenses",
  "research and development",
  "selling general and administrative",
  "cost of goods sold"
)
operational_phrases <- unique(tolower(operational_phrases))

# Negation words for diagnostics
negation_words <- c(
  "not", "no", "never", "neither", "nor",
  "cannot", "can't", "won't", "don't", "didn't",
  "isn't", "aren't", "wasn't", "weren't",
  "without", "less", "lack", "lacks", "lacking"
)
negation_words <- unique(tolower(negation_words))

# ---------------------------------------------------------------------------
# PARAMETERS FOR MODULE 3
# ---------------------------------------------------------------------------
params <- list(
  weight_numeric    = 0.5,  # weight for numeric density in operational specificity
  weight_compounds  = 0.5,  # weight for operational compound density
  min_cell_size     = 10,   # minimum n for sic2×year cell to be "non-small"
  outlier_threshold = 3     # |z| > 3 used for outlier diagnostics
)

# ---------------------------------------------------------------------------
# FORWARD-LOOKING: SPLIT INTO MODAL (DFM) + REGEX (TEXT) CHANNELS
# Module 3 reads config$forward_looking_modals and config$forward_looking_regex.
# The "modals" key feeds the DFM-based token channel (all single-word FL terms).
# The "regex" key feeds the text-based regex channel (multi-word constructions).
# ---------------------------------------------------------------------------

forward_looking_modals <- forward_looking_extended   # all 34 single-word FL tokens

forward_looking_regex <- c(
  "\\bgoing\\s+forward\\b",
  "\\bin\\s+the\\s+future\\b",
  "\\bexpects?\\s+to\\b",
  "\\bexpected\\s+to\\b",
  "\\bplans?\\s+to\\b",
  "\\bwe\\s+believe\\b",
  "\\bintend(s|ed)?\\s+to\\b",
  "\\bnext\\s+(year|quarter|fiscal)\\b",
  "\\bquarters?\\s+ahead\\b",
  "\\blong[- ]?term\\b",
  "\\bshort[- ]?term\\b",
  "\\bnear[- ]?term\\b"
)

cat("Forward-looking modals (DFM channel): ", length(forward_looking_modals), "\n", sep = "")
cat("Forward-looking regex (text channel):  ", length(forward_looking_regex), "\n", sep = "")

# ---------------------------------------------------------------------------
# BUILD AND SAVE CONFIG
# ---------------------------------------------------------------------------

analysis_config <- list(
  lm_positive            = lm_positive,
  lm_negative            = lm_negative,
  lm_uncertainty         = lm_uncertainty,
  lm_litigious           = lm_litigious,
  lm_optional            = lm_optional,
  forward_looking_modals  = forward_looking_modals,
  forward_looking_regex   = forward_looking_regex,
  forward_looking_extended = forward_looking_extended,  # retained for documentation
  operational_phrases    = operational_phrases,
  negation_words         = negation_words,
  params                 = params,
  meta                   = list(
    lm_csv_path = lm_csv_path,
    created     = Sys.time()
  )
)

saveRDS(analysis_config, config_path)

cat("Saved analysis_config.rds to: ", config_path, "\n", sep = "")
cat("=== analysis_config.rds BUILD COMPLETE ===\n")
