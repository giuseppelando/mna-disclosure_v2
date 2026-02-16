# ==============================================================================
# MODULE 4: KWIC VALIDATION  (REVISED v3)
# ==============================================================================
# Purpose: Export KWIC samples for manual validation of disclosure indices.
#
# REVISION NOTES (v3 — aligned with v6 index constructs):
#   1. Added sentence-based FL validation: exports top/bottom FL sentences
#      (not just token-level KWIC) to validate the new sentence-based measure.
#   2. Added risk transparency validation: samples docs with highest/lowest
#      risk_transparency (cosine deviation from peers) and shows Risk Factors
#      excerpts side-by-side.
#   3. Retained token-level KWIC for operational specificity and tone.
#   4. Column mapping updated for Module 3 v6 output names.
#
# Inputs:
#   - data/interim/tokens_objects.rds   (Module 2)
#   - data/interim/cleaned_text.rds     (Module 1)
#   - data/interim/disclosure_indices.rds (Module 3 v6)
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

cat("=== MODULE 4: KWIC VALIDATION (v3) ===\n")

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
N_TAIL_DOCS <- 10
WINDOW      <- 10
N_MAX_ROWS  <- 50
N_SENTENCES <- 20   # max FL sentences to export per tail group
SEED        <- 2025

# ---- LOAD INPUTS -------------------------------------------------------------
tokens_obj <- readRDS(path_tokens)
cleaned_df <- readRDS(path_cleaned)
indices_df <- readRDS(path_indices)

stopifnot("deal_id" %in% names(cleaned_df), "deal_id" %in% names(indices_df))

cleaned_df <- cleaned_df %>% mutate(deal_id = as.character(deal_id))
indices_df <- indices_df %>% mutate(deal_id = as.character(deal_id))

deals <- indices_df %>%
  left_join(cleaned_df %>% select(deal_id, mda_clean, risk_clean), by = "deal_id")

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


# ---- INDEX COLUMN MAPPING (v3: aligned with Module 3 v6) --------------------
idx_oper <- "operational_specificity_norm"
idx_fwd  <- "forward_looking_norm"         # sentence-based FL
idx_risk <- if ("risk_transparency_norm" %in% names(indices_df)) {
  "risk_transparency_norm"
} else if ("risk_disclosure_raw_norm" %in% names(indices_df)) {
  "risk_disclosure_raw_norm"
} else { "risk_disclosure_tfidf_norm" }
idx_tone <- "tone_lm_norm"

# Check which are actually present (v6 may have different names)
needed <- c(idx_oper, idx_fwd, idx_risk, idx_tone)
available <- needed[needed %in% names(indices_df)]
if (length(available) == 0) {
  stop("No index columns found. Expected: ", paste(needed, collapse = ", "))
}

cat(sprintf("  Index columns: %s\n", paste(available, collapse = ", ")))


# ---- KWIC PATTERNS (token-level — for operational and tone) ------------------
patterns <- list(
  operational = list(
    tokens_name = "mda_compounds",
    valuetype   = "fixed",
    window      = WINDOW,
    patterns    = c("capital_expenditure", "gross_margin", "operating_cash_flow")
  ),
  tone = list(
    tokens_name = "mda_uni_dict",
    valuetype   = "glob",
    window      = WINDOW,
    patterns    = c("improv*", "strong*", "declin*")
  ),
  negation = list(
    tokens_name = "mda_uni_dict",
    valuetype   = "glob",
    window      = WINDOW,
    patterns    = c("not", "no", "never")
  )
)


# ---- TOP/BOTTOM GROUPS -------------------------------------------------------
groups <- list()
if (idx_oper %in% names(indices_df))
  groups$operational <- get_top_bottom_ids(deals, idx_oper, N_TAIL_DOCS)
if (idx_fwd %in% names(indices_df))
  groups$forward     <- get_top_bottom_ids(deals, idx_fwd,  N_TAIL_DOCS)
if (idx_risk %in% names(indices_df))
  groups$risk        <- get_top_bottom_ids(deals, idx_risk, N_TAIL_DOCS)
if (idx_tone %in% names(indices_df)) {
  groups$tone        <- get_top_bottom_ids(deals, idx_tone, N_TAIL_DOCS)
  groups$negation    <- groups$tone
}


# ---- TOKEN-LEVEL KWIC EXTRACTION ---------------------------------------------
kwic_all <- list()

for (construct in intersect(names(patterns), names(groups))) {
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


# ---- v3 NEW: SENTENCE-BASED FL VALIDATION ------------------------------------
cat("\n  Extracting FL sentence samples (v3 new)...\n")

fl_sentence_regex <- paste0("(",
  paste(c(
    "\\bexpect(s|ed|ing)?\\s+(to|that)\\b",
    "\\banticipat(e|ed|es|ing)\\b",
    "\\bforecast(s|ed|ing)?\\b",
    "\\boutlook\\b", "\\bguidance\\b",
    "\\bplan(s|ned)?\\s+to\\b",
    "\\bnext\\s+(quarter|year|fiscal)\\b",
    "\\bover\\s+the\\s+next\\b",
    "\\bwill\\s+(continue|increase|decrease|remain|result|generate)\\b"
  ), collapse = "|"),
")")

extract_fl_sentences <- function(txt, n_max = 10) {
  if (is.na(txt) || nchar(txt) < 50) return(character(0))
  sents <- unlist(str_split(txt, "(?<=[.!?;])\\s+"))
  sents <- sents[nchar(sents) > 15]
  fl_sents <- sents[str_detect(sents, fl_sentence_regex)]
  if (length(fl_sents) > n_max) {
    set.seed(SEED)
    fl_sents <- sample(fl_sents, n_max)
  }
  fl_sents
}

fl_sentence_samples <- list()
if (!is.null(groups$forward)) {
  for (grp in c("top", "bottom")) {
    ids <- groups$forward[[grp]]
    for (id in ids) {
      txt <- deals$mda_clean[deals$deal_id == id]
      if (length(txt) == 0 || is.na(txt)) next
      sents <- extract_fl_sentences(txt, n_max = 5)
      if (length(sents) > 0) {
        fl_sentence_samples[[paste(id, grp)]] <- tibble(
          deal_id = id, group = grp, construct = "forward_sentences",
          sentence = sents
        )
      }
    }
  }
}

fl_sents_df <- bind_rows(fl_sentence_samples)
cat(sprintf("  - FL sentence samples: %d rows\n", nrow(fl_sents_df)))


# ---- v3 NEW: RISK TRANSPARENCY EXCERPTS -------------------------------------
cat("  Extracting risk transparency excerpts (v3 new)...\n")

risk_excerpts <- list()
if (!is.null(groups$risk)) {
  for (grp in c("top", "bottom")) {
    ids <- groups$risk[[grp]]
    for (id in ids) {
      txt <- deals$risk_clean[deals$deal_id == id]
      if (length(txt) == 0 || is.na(txt)) next
      # Take first 500 chars as representative excerpt
      excerpt <- substr(txt, 1, 500)
      risk_excerpts[[paste(id, grp)]] <- tibble(
        deal_id = id, group = grp, construct = "risk_transparency",
        excerpt = excerpt
      )
    }
  }
}

risk_excerpts_df <- bind_rows(risk_excerpts)
cat(sprintf("  - Risk transparency excerpts: %d rows\n", nrow(risk_excerpts_df)))


# ---- EXPORT: XLSX ------------------------------------------------------------
xlsx_path <- file.path(path_out_tbls, "kwic_samples.xlsx")
md_path   <- file.path(path_reports, "04_kwic_validation.md")

wb <- openxlsx::createWorkbook()

# Token-level KWIC sheets
if (nrow(kwic_df) > 0) {
  for (construct_name in unique(kwic_df$construct)) {
    df_c  <- kwic_df %>% filter(.data$construct == construct_name)
    sheet <- substr(construct_name, 1, 31)
    openxlsx::addWorksheet(wb, sheetName = sheet)
    openxlsx::writeData(wb, sheet = sheet, x = df_c)
  }
}

# v3: FL sentence sheet
if (nrow(fl_sents_df) > 0) {
  openxlsx::addWorksheet(wb, sheetName = "forward_sentences")
  openxlsx::writeData(wb, "forward_sentences", fl_sents_df)
}

# v3: Risk transparency excerpt sheet
if (nrow(risk_excerpts_df) > 0) {
  openxlsx::addWorksheet(wb, sheetName = "risk_transparency")
  openxlsx::writeData(wb, "risk_transparency", risk_excerpts_df)
}

# Summary + README
openxlsx::addWorksheet(wb, sheetName = "SUMMARY")
summary_tab <- kwic_df %>%
  count(construct, pattern, group, name = "n_rows") %>%
  arrange(construct, pattern, group)
if (nrow(fl_sents_df) > 0) {
  summary_tab <- bind_rows(summary_tab,
    fl_sents_df %>% count(construct, group, name = "n_rows") %>%
      mutate(pattern = "sentence-level"))
}
openxlsx::writeData(wb, "SUMMARY", summary_tab)

openxlsx::addWorksheet(wb, sheetName = "README")
openxlsx::writeData(
  wb, "README",
  x = data.frame(
    Item = c("Purpose", "Tails", "v3 changes", "FL validation", "Risk validation"),
    Description = c(
      "Manual construct validation via KWIC snippets and sentence excerpts.",
      paste0("Top/bottom tails: ", N_TAIL_DOCS, " docs per tail."),
      "v3 adds sentence-based FL and risk transparency excerpt validation.",
      "forward_sentences sheet: actual FL-classified sentences from top/bottom docs.",
      "risk_transparency sheet: Risk Factors excerpts from most/least idiosyncratic docs."
    ),
    stringsAsFactors = FALSE
  )
)

openxlsx::saveWorkbook(wb, xlsx_path, overwrite = TRUE)


# ---- REPORT ------------------------------------------------------------------
report <- c(
  "# Module 4 — KWIC Validation Report (v3)",
  "",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "## v3 Changes",
  "- Added sentence-based FL validation (not just token KWIC)",
  "- Added risk transparency excerpt comparison (top vs bottom cosine deviation)",
  "- Updated column mapping for Module 3 v6 output",
  "",
  "## Index columns used",
  paste0("- operational: `", idx_oper, "`"),
  paste0("- forward-looking: `", idx_fwd, "`"),
  paste0("- risk: `", idx_risk, "`"),
  paste0("- tone: `", idx_tone, "`"),
  "",
  paste0("## Total KWIC rows: ", nrow(kwic_df)),
  paste0("## FL sentence samples: ", nrow(fl_sents_df)),
  paste0("## Risk transparency excerpts: ", nrow(risk_excerpts_df)),
  ""
)

writeLines(report, md_path)

cat("\nSaved outputs:\n")
cat(sprintf("  - %s\n", xlsx_path))
cat(sprintf("  - %s\n", md_path))
cat("\n=== MODULE 4 COMPLETE (v3) ===\n")
