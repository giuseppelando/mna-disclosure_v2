# ==============================================================================
# CONFIG: LOUGHRAN-MCDONALD MASTER DICTIONARY + PROJECT PARAMETERS
# ==============================================================================
# Purpose: Build LM category word lists from the official CSV and bundle all
#          analysis parameters into config/analysis_config.rds.
# Input : data/raw/Loughran-McDonald_MasterDictionary_*.csv
# Output: config/analysis_config.rds
# ==============================================================================

suppressPackageStartupMessages({
  library(stringr)
})

# ---- Paths (project-root relative; override via env vars) --------------------
project_root <- Sys.getenv("PROJECT_ROOT", unset = ".")
lm_csv_path <- Sys.getenv(
  "PATH_LM_CSV",
  unset = file.path(project_root, "data", "raw", "Loughran-McDonald_MasterDictionary_1993-2024.csv")
)
path_config_dir <- Sys.getenv("PATH_CONFIG_DIR", unset = file.path(project_root, "config"))
path_output <- Sys.getenv("PATH_ANALYSIS_CONFIG", unset = file.path(path_config_dir, "analysis_config.rds"))

dir.create(path_config_dir, showWarnings = FALSE, recursive = TRUE)

cat("=== LM DICTIONARY CONFIGURATION ===\n")
cat(sprintf("Input CSV: %s\n\n", normalizePath(lm_csv_path, winslash = "/", mustWork = FALSE)))

if (!file.exists(lm_csv_path)) {
  stop("LM CSV not found: ", lm_csv_path)
}

# ---- Load + standardize ------------------------------------------------------
lm_raw <- read.csv(lm_csv_path, stringsAsFactors = FALSE, check.names = FALSE)
names(lm_raw) <- tolower(names(lm_raw))

word_col <- intersect(names(lm_raw), c("word", "term", "token"))
if (length(word_col) != 1) {
  stop("Could not uniquely identify token column. Found: ", paste(word_col, collapse = ", "))
}
word_col <- word_col[[1]]

lm_word <- tolower(str_trim(lm_raw[[word_col]]))
keep <- !is.na(lm_word) & nzchar(lm_word)
lm_raw <- lm_raw[keep, , drop = FALSE]
lm_word <- lm_word[keep]

flag_on <- function(x) {
  x_num <- suppressWarnings(as.numeric(x))
  !is.na(x_num) & x_num > 0
}

get_col <- function(df_names, colname) {
  hit <- intersect(df_names, tolower(colname))
  if (length(hit) != 1) stop("Missing or ambiguous LM column: '", colname, "'.")
  hit[[1]]
}

# ---- Core LM categories used in this project --------------------------------
col_positive <- get_col(names(lm_raw), "positive")
col_negative <- get_col(names(lm_raw), "negative")
col_uncertainty <- get_col(names(lm_raw), "uncertainty")

lm_positive <- unique(lm_word[flag_on(lm_raw[[col_positive]])])
lm_negative <- unique(lm_word[flag_on(lm_raw[[col_negative]])])
lm_uncertainty <- unique(lm_word[flag_on(lm_raw[[col_uncertainty]])])

# Some CSV versions include litigious; keep if present but do not force.
lm_litigious <- character(0)
if ("litigious" %in% names(lm_raw)) {
  lm_litigious <- unique(lm_word[flag_on(lm_raw[["litigious"]])])
}

# ---- Project (non-LM) dictionary: forward-looking terms ---------------------
modal_all <- c("will", "shall", "may", "might", "could", "would", "should", "can", "must")
forward_looking_terms <- unique(c(
  modal_all,
  "expect","expects","expected","expecting","expectation","expectations",
  "anticipate","anticipates","anticipated","anticipating","anticipation",
  "believe","believes","believed","believing","belief",
  "intend","intends","intended","intending","intention","intentions",
  "plan","plans","planned","planning",
  "forecast","forecasts","forecasted","forecasting",
  "project","projects","projected","projecting","projection","projections",
  "estimate","estimates","estimated","estimating",
  "outlook","guidance",
  "future","forward-looking",
  "next","upcoming","forthcoming",
  "potential","potentially",
  "possible","possibly","possibility","possibilities",
  "likely","unlikely"
))

# ---- Project parameters ------------------------------------------------------
params <- list(
  # Normalization
  min_cell_size = 10L,

  # Operational Specificity weighting
  op_spec_w_numeric = 0.6,
  op_spec_w_operational = 0.4,

  # Denominator convention
  density_per = 1000
)

config_list <- list(
  created = Sys.time(),
  lm_source_file = basename(lm_csv_path),
  lm = list(
    positive = lm_positive,
    negative = lm_negative,
    uncertainty = lm_uncertainty,
    litigious = lm_litigious
  ),
  project = list(
    forward_looking_terms = forward_looking_terms
  ),
  params = params
)

saveRDS(config_list, path_output)
cat(sprintf("Saved config: %s\n", normalizePath(path_output, winslash = "/", mustWork = FALSE)))
