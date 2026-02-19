# ==============================================================================
# MODULE 4: KWIC VALIDATION  (REVISED v2)
# ==============================================================================
# Purpose: Export KWIC samples for manual validation of four disclosure indices.
#
# REVISION NOTES (v2):
#   1. Fixed `all_of()` inside rename expression (version-dependent behaviour)
#   2. Report file numbered correctly as 04 (was 03)
#   3. Added negation-context KWIC for tone validation
#   4. Parameterised seed for reproducibility
#   5. Handles edge case where index column may contain all NAs
#
# Inputs:
#   - data/interim/tokens_objects.rds   (Module 2)
#   - data/interim/cleaned_text.rds     (Module 1)
#   - data/interim/disclosure_indices.rds (Module 3)
# Outputs:
#   - output/tables/kwic_samples.xlsx
#   - reports/04_kwic_validation.md
# ==============================================================================

suppressPackageStartupMessages({
  library(quanteda)
  library(dplyr)
  library(stringr)
})

has_openxlsx <- requireNamespace("openxlsx", quietly = TRUE)
if (!has_openxlsx) stop("Package 'openxlsx' required. Install or adapt to CSV.")

cat("=== MODULE 4: KWIC VALIDATION (v2) ===\n")

# ---- PATHS -------------------------------------------------------------------
project_root <- Sys.getenv("PROJECT_ROOT", unset = getwd())

path_interim  <- file.path(project_root, "data", "interim")
path_reports  <- file.path(project_root, "reports")
path_out_tbls <- file.path(project_root, "output", "tables")

dir.create(path_reports,  recursive = TRUE, showWarnings = FALSE)
dir.create(path_out_tbls, recursive = TRUE, showWarnings = FALSE)

path_tokens  <- file.path(path_interim, "tokens_objects.rds")
path_cleaned <- file.path(path_interim, "cleaned_text.rds")
path_indices <- file.path(path_interim, "disclosure_indices.rds")

stopifnot(file.exists(path_tokens), file.exists(path_cleaned),
          file.exists(path_indices))

# ---- PARAMETERS --------------------------------------------------------------
N_TAIL_DOCS <- 10     # docs in top/bottom tails per construct
WINDOW      <- 10     # KWIC window (tokens before/after keyword)
N_MAX_ROWS  <- 50     # max KWIC rows per (construct x pattern x group)
SEED        <- 2025   # reproducibility

# ---- LOAD INPUTS -------------------------------------------------------------
tokens_obj <- readRDS(path_tokens)
cleaned_df <- readRDS(path_cleaned)
indices_df <- readRDS(path_indices)

stopifnot("deal_id" %in% names(cleaned_df), "deal_id" %in% names(indices_df))

cleaned_df <- cleaned_df %>% mutate(deal_id = as.character(deal_id))
indices_df <- indices_df %>% mutate(deal_id = as.character(deal_id))

# Minimal merge (avoid column duplication)
deals <- indices_df %>%
  left_join(cleaned_df %>% select(deal_id), by = "deal_id")

# ---- HELPERS -----------------------------------------------------------------
extract_deal_id_from_docname <- function(docname) {
  out <- str_match(docname, "^deal_(.+)_(mda|risk)$")[, 2]
  ifelse(is.na(out), NA_character_, out)
}

get_tokens_or_stop <- function(obj, name) {
  if (!name %in% names(obj)) {
    stop("tokens_objects.rds does not contain: ", name,
         "\nAvailable: ", paste(names(obj), collapse = ", "))
  }
  obj[[name]]
}

get_top_bottom_ids <- function(deals_df, index_col, n_each) {
  if (!index_col %in% names(deals_df)) {
    warning("Index column not found: ", index_col)
    return(list(top = character(0), bottom = character(0)))
  }
  tmp <- deals_df %>%
    select(deal_id, val = !!sym(index_col)) %>%
    filter(!is.na(val))
  if (nrow(tmp) == 0) return(list(top = character(0), bottom = character(0)))
  list(
    top    = tmp %>% arrange(desc(val)) %>% slice_head(n = n_each) %>% pull(deal_id),
    bottom = tmp %>% arrange(val)       %>% slice_head(n = n_each) %>% pull(deal_id)
  )
}

clean_kwic_text <- function(x) {
  x %>%
    str_replace_all("\\#table_start\\b|\\#table_end\\b", " ") %>%
    str_replace_all("\\s+", " ") %>%
    str_trim()
}

kwic_extract <- function(tokens_x, pattern, deal_ids, window,
                         valuetype, n_max, seed = SEED) {
  if (length(deal_ids) == 0) return(tibble())

  k <- quanteda::kwic(tokens_x, pattern = pattern,
                       window = window, valuetype = valuetype)
  k <- as_tibble(k)
  if (nrow(k) == 0) return(tibble())

  k <- k %>%
    mutate(
      deal_id = extract_deal_id_from_docname(docname),
      pattern = pattern
    ) %>%
    filter(!is.na(deal_id), deal_id %in% deal_ids) %>%
    mutate(pre = clean_kwic_text(pre), post = clean_kwic_text(post))

  if (nrow(k) == 0) return(tibble())

  set.seed(seed)
  if (nrow(k) > n_max) k <- k %>% slice_sample(n = n_max)
  k
}


# ---- INDEX COLUMN MAPPING ----------------------------------------------------
# REVISED: column names aligned with Module 3 output (v5)
idx_oper <- "operational_specificity_norm"
idx_fwd  <- "forward_looking_norm"
idx_risk <- if ("risk_disclosure_tfidf_norm" %in% names(indices_df)) {
  "risk_disclosure_tfidf_norm"
} else { "risk_disclosure_raw_norm" }
idx_tone <- "tone_lm_norm"

needed <- c(idx_oper, idx_fwd, idx_risk, idx_tone)
missing_needed <- needed[!needed %in% names(indices_df)]
if (length(missing_needed) > 0) {
  stop("Missing index columns: ", paste(missing_needed, collapse = ", "))
}

# ---- KWIC PATTERNS -----------------------------------------------------------
patterns <- list(
  operational = list(
    tokens_name = "mda_compounds",
    valuetype   = "fixed",
    window      = WINDOW,
    patterns    = c("capital_expenditure", "gross_margin", "operating_cash_flow")
  ),
  forward = list(
    tokens_name = "mda_uni_dict",
    valuetype   = "glob",
    window      = WINDOW,
    patterns    = c("expect*", "anticipat*", "forecast*")
  ),
  risk = list(
    tokens_name = "risk_uni_dict",
    valuetype   = "glob",
    window      = WINDOW,
    patterns    = c("risk", "risks", "uncertain*", "adverse*")
  ),
  tone = list(
    tokens_name = "mda_uni_dict",
    valuetype   = "glob",
    window      = WINDOW,
    patterns    = c("improv*", "strong*", "declin*")
  ),
  # REVISION: Negation-context KWIC for tone validation
  negation = list(
    tokens_name = "mda_uni_dict",
    valuetype   = "glob",
    window      = WINDOW,
    patterns    = c("not", "no", "never")
  )
)

# ---- TOP/BOTTOM GROUPS -------------------------------------------------------
groups <- list(
  operational = get_top_bottom_ids(deals, idx_oper, n_each = N_TAIL_DOCS),
  forward     = get_top_bottom_ids(deals, idx_fwd,  n_each = N_TAIL_DOCS),
  risk        = get_top_bottom_ids(deals, idx_risk, n_each = N_TAIL_DOCS),
  tone        = get_top_bottom_ids(deals, idx_tone, n_each = N_TAIL_DOCS),
  negation    = get_top_bottom_ids(deals, idx_tone, n_each = N_TAIL_DOCS)
)

# ---- KWIC EXTRACTION ---------------------------------------------------------
kwic_all <- list()

for (construct in names(patterns)) {
  cfg <- patterns[[construct]]
  tok <- get_tokens_or_stop(tokens_obj, cfg$tokens_name)

  for (pat in cfg$patterns) {
    k_top <- kwic_extract(tok, pat, groups[[construct]]$top,
                          window = cfg$window, valuetype = cfg$valuetype,
                          n_max = N_MAX_ROWS) %>%
      mutate(construct = construct, group = "top")

    k_bot <- kwic_extract(tok, pat, groups[[construct]]$bottom,
                          window = cfg$window, valuetype = cfg$valuetype,
                          n_max = N_MAX_ROWS) %>%
      mutate(construct = construct, group = "bottom")

    kwic_all[[paste(construct, pat, sep = "__")]] <- bind_rows(k_top, k_bot)
  }
}

kwic_df <- bind_rows(kwic_all)

# Remove "risk-free" false positives
kwic_df <- kwic_df %>%
  filter(!(construct == "risk" & str_to_lower(keyword) == "risk-free"))

# REVISION: Fixed all_of() inside rename (version-dependent dplyr behaviour)
# Use explicit rename after select instead.
score_lookup <- deals %>%
  select(deal_id,
         !!idx_oper, !!idx_fwd, !!idx_risk, !!idx_tone) %>%
  rename(
    operational_score = !!idx_oper,
    forward_score     = !!idx_fwd,
    risk_score        = !!idx_risk,
    tone_score        = !!idx_tone
  )

kwic_df <- kwic_df %>%
  left_join(score_lookup, by = "deal_id") %>%
  mutate(
    score = case_when(
      construct == "operational" ~ operational_score,
      construct == "forward"     ~ forward_score,
      construct == "risk"        ~ risk_score,
      construct %in% c("tone", "negation") ~ tone_score,
      TRUE ~ NA_real_
    ),
    reviewer_flag_false_positive = NA,
    reviewer_flag_false_negative = NA,
    reviewer_notes = NA_character_
  ) %>%
  select(
    construct, group, pattern, deal_id, docname, score,
    pre, keyword, post, from, to,
    reviewer_flag_false_positive, reviewer_flag_false_negative, reviewer_notes
  ) %>%
  arrange(construct, pattern, group, desc(score))


# ---- EXPORT: XLSX ------------------------------------------------------------
xlsx_path <- file.path(path_out_tbls, "kwic_samples.xlsx")
# REVISION: Report file numbered as 04 (was incorrectly 03)
md_path   <- file.path(path_reports, "04_kwic_validation.md")

wb <- openxlsx::createWorkbook()

for (construct_name in unique(kwic_df$construct)) {
  df_c  <- kwic_df %>% filter(.data$construct == construct_name)
  sheet <- substr(construct_name, 1, 31)
  openxlsx::addWorksheet(wb, sheetName = sheet)
  openxlsx::writeData(wb, sheet = sheet, x = df_c)
}

# Summary sheet
openxlsx::addWorksheet(wb, sheetName = "SUMMARY")
summary_tab <- kwic_df %>%
  count(construct, pattern, group, name = "n_rows") %>%
  arrange(construct, pattern, group)
openxlsx::writeData(wb, "SUMMARY", summary_tab)

# README sheet
openxlsx::addWorksheet(wb, sheetName = "README")
openxlsx::writeData(
  wb, "README",
  x = data.frame(
    Item = c("Purpose", "Tails", "Reviewer fields", "Decision rule", "Negation"),
    Description = c(
      "Manual construct validation using KWIC snippets.",
      paste0("Top/bottom tails: ", N_TAIL_DOCS, " docs per tail."),
      "Fill: reviewer_flag_false_positive/negative, reviewer_notes.",
      "If false-positive rate >20%, refine pattern and document.",
      "Negation sheet validates tone measure against negation contexts."
    ),
    stringsAsFactors = FALSE
  )
)

openxlsx::saveWorkbook(wb, xlsx_path, overwrite = TRUE)


# ---- REPORT (MD) -------------------------------------------------------------
report <- c(
  "# Module 4 - KWIC Validation Report (v2)",
  "",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "## Inputs",
  paste0("- tokens: `", path_tokens, "`"),
  paste0("- indices: `", path_indices, "`"),
  "",
  "## Index columns used for tails",
  paste0("- operational: `", idx_oper, "`"),
  paste0("- forward-looking: `", idx_fwd, "`"),
  paste0("- risk disclosure: `", idx_risk, "`"),
  paste0("- tone: `", idx_tone, "`"),
  "",
  "## Parameters",
  paste0("- N_TAIL_DOCS: ", N_TAIL_DOCS),
  paste0("- WINDOW: ", WINDOW),
  paste0("- N_MAX_ROWS: ", N_MAX_ROWS),
  paste0("- SEED: ", SEED),
  "",
  "## KWIC patterns",
  paste0("- operational: ",
         paste(patterns$operational$patterns, collapse = ", ")),
  paste0("- forward: ",
         paste(patterns$forward$patterns, collapse = ", ")),
  paste0("- risk: ",
         paste(patterns$risk$patterns, collapse = ", ")),
  paste0("- tone: ",
         paste(patterns$tone$patterns, collapse = ", ")),
  paste0("- negation: ",
         paste(patterns$negation$patterns, collapse = ", ")),
  "",
  paste0("## Total KWIC rows exported: ", nrow(kwic_df)),
  ""
)

if (nrow(summary_tab) > 0) {
  report <- c(report,
    "### Rows by construct / pattern / group",
    "",
    "| construct | pattern | group | n_rows |",
    "|---|---|---|---|",
    apply(summary_tab, 1, function(r) {
      paste0("| ", r[[1]], " | ", r[[2]], " | ", r[[3]], " | ", r[[4]], " |")
    })
  )
}

report <- c(report, "",
  "## Revision Notes (v2)",
  "- Fixed dplyr `all_of()` rename issue",
  "- Report numbered as 04 (was incorrectly 03)",
  "- Added negation-context KWIC sheet for tone validation",
  "- Reproducible seed for KWIC sampling",
  ""
)

writeLines(report, md_path)

cat("Saved outputs:\n")
cat(sprintf("  - %s\n", xlsx_path))
cat(sprintf("  - %s\n", md_path))
cat("\n=== MODULE 4 COMPLETE (v2) ===\n")
