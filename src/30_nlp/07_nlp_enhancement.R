# ==============================================================================
# NLP PIPELINE ENHANCEMENT MODULE v2
# ==============================================================================
#
# PURPOSE: Enhance existing NLP indices with:
#   1. Readability measures (Fog Index, Flesch-Kincaid)
#   2. Modal certainty index (strong/weak modal ratio)
#   3. Multiple normalization tracks
#   4. Expanded numeric features
#   5. Cleaning/feature diagnostics
#
# DESIGN: Runs AFTER existing pipeline, ADDS to existing indices
#         Does NOT modify working code
#
# INPUTS:
#   - data/interim/cleaned_text.rds (from Module 1)
#   - data/interim/dfm_objects.rds (from Module 2)
#   - data/interim/disclosure_indices.rds (from Module 3)
#   - config/analysis_config.rds
#
# OUTPUTS:
#   - data/interim/enhanced_features.rds
#   - data/interim/indices_enhanced.rds
#   - reports/enhancement_report.md
#
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(quanteda)
  library(stringr)
})

cat("
==============================================================================
NLP PIPELINE ENHANCEMENT MODULE v2
==============================================================================
")
cat(sprintf("Timestamp: %s\n\n", Sys.time()))

# ==============================================================================
# CONFIGURATION
# ==============================================================================

CONFIG <- list(
  # Paths
  paths = list(
    cleaned_text = "data/interim/cleaned_text.rds",
    dfm_objects = "data/interim/dfm_objects.rds",
    indices = "data/interim/disclosure_indices.rds",
    config = "config/analysis_config.rds",
    output_features = "data/interim/enhanced_features.rds",
    output_indices = "data/interim/indices_enhanced.rds",
    output_report = "reports/enhancement_report.md"
  ),
  
  # Protected terms (EXPANDED from original)
  # These should NEVER be trimmed from DFM regardless of document frequency
  protected_terms = c(
    # Modals (original)
    "will", "shall", "may", "might", "could", "would", "should", "must",
    
    # Negations (original)
    "not", "no", "nor", "neither", "never", "none", "cannot",
    
    # Financial terms (NEW - addresses "capital" missing issue)
    "capital", "expenditure", "expenditures", "margin", "margins",
    "revenue", "revenues", "income", "profit", "loss", "losses",
    "debt", "equity", "asset", "assets", "liability", "liabilities",
    "cash", "flow", "flows", "earnings", "sales",
    
    # Comparatives (NEW)
    "increase", "increased", "increasing", "decrease", "decreased", "decreasing",
    "grow", "grew", "growing", "growth", "decline", "declined", "declining",
    "rise", "rose", "rising", "fall", "fell", "falling",
    "improve", "improved", "improving", "deteriorate", "deteriorated",
    "higher", "lower", "more", "less", "better", "worse",
    
    # Hedging/uncertainty (NEW)
    "approximately", "about", "around", "roughly", "nearly", "almost",
    "possibly", "probably", "likely", "unlikely", "uncertain", "expected"
  ),
  
  # Modal verb categories for Modal Certainty index
  modals = list(
    strong = c("will", "shall", "must"),
    weak = c("may", "might", "could", "would", "should")
  ),
  
  # Normalization settings
  normalization = list(
    min_cell_sizes = c(10, 8, 5),  # Try in order, use first that achieves target
    target_success_rate = 0.40,    # At least 40% at ideal level
    fallback_order = c("sic2_year", "sic2", "year", "global")
  )
)

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

#' Safe density calculation (per 1000 words)
safe_density <- function(count, word_count, per = 1000) {
  ifelse(!is.na(word_count) & word_count > 0, 
         count / (word_count / per), 
         NA_real_)
}

#' Z-score standardization
zscore <- function(x) {
  s <- sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(0, length(x)))
  (x - mean(x, na.rm = TRUE)) / s
}

#' Count syllables in a word (approximate)
count_syllables <- function(word) {
  word <- tolower(word)
  # Remove silent e at end
  word <- str_replace(word, "e$", "")
  # Count vowel groups
  n_vowels <- str_count(word, "[aeiouy]+")
  max(n_vowels, 1)
}

#' Vectorized syllable counting for text
count_syllables_text <- function(words_vec) {
  sapply(words_vec, count_syllables, USE.NAMES = FALSE)
}

# ==============================================================================
# BLOCK 1: LOAD EXISTING DATA
# ==============================================================================

cat("BLOCK 1: Loading existing pipeline outputs...\n")

# Check files exist
for (path_name in c("cleaned_text", "dfm_objects", "indices")) {
  path <- CONFIG$paths[[path_name]]
  if (!file.exists(path)) {
    stop(sprintf("Required file not found: %s\nRun the main NLP pipeline first.", path))
  }
}

cleaned_data <- readRDS(CONFIG$paths$cleaned_text)
dfm_objects <- readRDS(CONFIG$paths$dfm_objects)
indices_orig <- readRDS(CONFIG$paths$indices)

# Load config if exists
if (file.exists(CONFIG$paths$config)) {
  analysis_config <- readRDS(CONFIG$paths$config)
} else {
  analysis_config <- list()
}

cat(sprintf("  - Cleaned text: %d documents\n", nrow(cleaned_data)))
cat(sprintf("  - Existing indices: %d documents × %d variables\n", 
            nrow(indices_orig), ncol(indices_orig)))

# Ensure consistent deal_id type
cleaned_data$deal_id <- as.character(cleaned_data$deal_id)
indices_orig$deal_id <- as.character(indices_orig$deal_id)

# ==============================================================================
# BLOCK 2: EXTRACT READABILITY FEATURES
# ==============================================================================

cat("\nBLOCK 2: Extracting readability features...\n")

extract_readability <- function(text, deal_id) {
  
  if (is.na(text) || nchar(text) < 100) {
    return(tibble(
      deal_id = deal_id,
      n_words = NA_integer_,
      n_sentences = NA_integer_,
      n_syllables = NA_integer_,
      n_complex_words = NA_integer_,
      avg_word_length = NA_real_,
      avg_sentence_length = NA_real_,
      fk_grade = NA_real_,
      fk_ease = NA_real_,
      fog_index = NA_real_,
      type_token_ratio = NA_real_
    ))
  }
  
  # Sentence detection (simple but effective)
  sentences <- unlist(str_split(text, "(?<=[.!?])\\s+"))
  sentences <- sentences[nchar(sentences) > 5]  # Filter fragments
  n_sentences <- max(length(sentences), 1)
  
  # Word extraction
  words <- unlist(str_extract_all(tolower(text), "\\b[a-z]{2,}\\b"))
  n_words <- length(words)
  
  if (n_words < 10) {
    return(tibble(
      deal_id = deal_id,
      n_words = n_words,
      n_sentences = n_sentences,
      n_syllables = NA_integer_,
      n_complex_words = NA_integer_,
      avg_word_length = NA_real_,
      avg_sentence_length = NA_real_,
      fk_grade = NA_real_,
      fk_ease = NA_real_,
      fog_index = NA_real_,
      type_token_ratio = NA_real_
    ))
  }
  
  # Syllable analysis
  syllables <- count_syllables_text(words)
  n_syllables <- sum(syllables)
  n_complex_words <- sum(syllables >= 3)  # 3+ syllables = complex
  
  # Basic metrics
  avg_word_length <- mean(nchar(words))
  avg_sentence_length <- n_words / n_sentences
  
  # Flesch-Kincaid Grade Level
  # FK = 0.39 × (words/sentences) + 11.8 × (syllables/words) − 15.59
  fk_grade <- 0.39 * avg_sentence_length + 
              11.8 * (n_syllables / n_words) - 15.59
  
  # Flesch Reading Ease
  # FRE = 206.835 − 1.015 × (words/sentences) − 84.6 × (syllables/words)
  fk_ease <- 206.835 - 1.015 * avg_sentence_length - 
             84.6 * (n_syllables / n_words)
  
  # Gunning Fog Index
  # Fog = 0.4 × [(words/sentences) + 100 × (complex_words/words)]
  fog_index <- 0.4 * (avg_sentence_length + 
                      100 * (n_complex_words / n_words))
  
  # Type-Token Ratio (vocabulary richness)
  type_token_ratio <- n_distinct(words) / n_words
  
  tibble(
    deal_id = deal_id,
    n_words = as.integer(n_words),
    n_sentences = as.integer(n_sentences),
    n_syllables = as.integer(n_syllables),
    n_complex_words = as.integer(n_complex_words),
    avg_word_length = avg_word_length,
    avg_sentence_length = avg_sentence_length,
    fk_grade = fk_grade,
    fk_ease = fk_ease,
    fog_index = fog_index,
    type_token_ratio = type_token_ratio
  )
}

# Apply to MD&A text
cat("  Computing readability for MD&A...\n")
readability_mda <- map2_df(
  cleaned_data$mda_clean, 
  cleaned_data$deal_id,
  extract_readability,
  .progress = TRUE
)

# Add prefix to distinguish
names(readability_mda)[-1] <- paste0("mda_read_", names(readability_mda)[-1])

cat(sprintf("  - Fog Index: mean=%.2f, sd=%.2f\n",
            mean(readability_mda$mda_read_fog_index, na.rm = TRUE),
            sd(readability_mda$mda_read_fog_index, na.rm = TRUE)))
cat(sprintf("  - FK Grade: mean=%.2f, sd=%.2f\n",
            mean(readability_mda$mda_read_fk_grade, na.rm = TRUE),
            sd(readability_mda$mda_read_fk_grade, na.rm = TRUE)))

# ==============================================================================
# BLOCK 3: MODAL CERTAINTY INDEX
# ==============================================================================

cat("\nBLOCK 3: Computing Modal Certainty index...\n")

# Get DFM
dfm_mda <- dfm_objects$mda_uni
if (is.null(dfm_mda)) {
  stop("MD&A unigram DFM not found in dfm_objects")
}

# Count strong and weak modals
strong_modals <- CONFIG$modals$strong
weak_modals <- CONFIG$modals$weak

# Check which modals are in the DFM
strong_in_dfm <- intersect(strong_modals, featnames(dfm_mda))
weak_in_dfm <- intersect(weak_modals, featnames(dfm_mda))

cat(sprintf("  - Strong modals in DFM: %s\n", paste(strong_in_dfm, collapse = ", ")))
cat(sprintf("  - Weak modals in DFM: %s\n", paste(weak_in_dfm, collapse = ", ")))

# Count occurrences
strong_counts <- if (length(strong_in_dfm) > 0) {
  rowSums(as.matrix(dfm_select(dfm_mda, pattern = strong_in_dfm)))
} else {
  rep(0, ndoc(dfm_mda))
}

weak_counts <- if (length(weak_in_dfm) > 0) {
  rowSums(as.matrix(dfm_select(dfm_mda, pattern = weak_in_dfm)))
} else {
  rep(0, ndoc(dfm_mda))
}

# Modal certainty = strong / (strong + weak), with guard
total_modals <- strong_counts + weak_counts
modal_certainty <- ifelse(
  total_modals >= 5,  # Require at least 5 modal verbs
  strong_counts / total_modals,
  NA_real_
)

# Create tibble with docvars
modal_df <- tibble(
  deal_id = as.character(docvars(dfm_mda)$deal_id),
  strong_modal_count = as.integer(strong_counts),
  weak_modal_count = as.integer(weak_counts),
  total_modal_count = as.integer(total_modals),
  modal_certainty_raw = modal_certainty
)

cat(sprintf("  - Modal certainty: mean=%.3f, sd=%.3f, N valid=%d\n",
            mean(modal_df$modal_certainty_raw, na.rm = TRUE),
            sd(modal_df$modal_certainty_raw, na.rm = TRUE),
            sum(!is.na(modal_df$modal_certainty_raw))))

# ==============================================================================
# BLOCK 4: EXPANDED NUMERIC FEATURES
# ==============================================================================

cat("\nBLOCK 4: Extracting expanded numeric features...\n")

extract_expanded_numerics <- function(text, word_count) {
  
  if (is.na(text) || is.na(word_count) || word_count < 10) {
    return(tibble(
      n_ranges = NA_integer_,
      n_ratios = NA_integer_,
      n_fiscal_years = NA_integer_,
      n_quarters_expanded = NA_integer_,
      n_comparatives = NA_integer_,
      range_density = NA_real_,
      ratio_density = NA_real_,
      fiscal_density = NA_real_,
      comparative_density = NA_real_
    ))
  }
  
  text_lower <- tolower(text)
  
  # Ranges: "10-15%", "5 to 10", "between 3 and 7"
  n_ranges <- str_count(text_lower, "\\d+(?:\\.\\d+)?\\s*(?:-|to)\\s*\\d+(?:\\.\\d+)?") +
              str_count(text_lower, "between\\s+\\d+(?:\\.\\d+)?\\s+and\\s+\\d+(?:\\.\\d+)?")
  
  # Ratios: "2:1", "3x", "2.5 times"
  n_ratios <- str_count(text_lower, "\\d+(?:\\.\\d+)?\\s*:\\s*\\d+(?:\\.\\d+)?") +
              str_count(text_lower, "\\d+(?:\\.\\d+)?\\s*x\\b") +
              str_count(text_lower, "\\d+(?:\\.\\d+)?\\s+times\\b")
  
  # Fiscal years: "fiscal 2023", "fy23", "fy 2023"
  n_fiscal_years <- str_count(text_lower, "(?:fiscal|fy)\\s*(?:year)?\\s*(?:20)?\\d{2}")
  
  # Quarters (expanded): Q1 2023, first quarter, 1Q23, Q1'23
  n_quarters_expanded <- str_count(text_lower, "q[1-4]\\s*(?:')?(?:20)?\\d{2}") +
                         str_count(text_lower, "[1-4]q\\s*(?:')?(?:20)?\\d{2}") +
                         str_count(text_lower, "(?:first|second|third|fourth)\\s+quarter")
  
  # Comparatives: change-related words
  n_comparatives <- str_count(text_lower, 
    "\\b(?:increase|decreased?|grow(?:th|ing)?|decline[d]?|rise[sn]?|f[ae]ll(?:ing)?|improv(?:ed?|ing)|deteriorat(?:ed?|ing)|higher|lower)\\b")
  
  tibble(
    n_ranges = as.integer(n_ranges),
    n_ratios = as.integer(n_ratios),
    n_fiscal_years = as.integer(n_fiscal_years),
    n_quarters_expanded = as.integer(n_quarters_expanded),
    n_comparatives = as.integer(n_comparatives),
    range_density = safe_density(n_ranges, word_count),
    ratio_density = safe_density(n_ratios, word_count),
    fiscal_density = safe_density(n_fiscal_years, word_count),
    comparative_density = safe_density(n_comparatives, word_count)
  )
}

# Get word counts
mda_wc <- cleaned_data$mda_word_count_clean
if (is.null(mda_wc)) {
  mda_wc <- str_count(cleaned_data$mda_clean, "\\S+")
}

expanded_numerics <- map2_df(
  cleaned_data$mda_clean,
  mda_wc,
  extract_expanded_numerics
)

expanded_numerics$deal_id <- cleaned_data$deal_id

cat(sprintf("  - Ranges per doc: mean=%.1f\n", mean(expanded_numerics$n_ranges, na.rm = TRUE)))
cat(sprintf("  - Comparatives per doc: mean=%.1f\n", mean(expanded_numerics$n_comparatives, na.rm = TRUE)))

# ==============================================================================
# BLOCK 5: COMBINE ALL FEATURES
# ==============================================================================

cat("\nBLOCK 5: Combining features...\n")

enhanced_features <- readability_mda %>%
  left_join(modal_df, by = "deal_id") %>%
  left_join(expanded_numerics, by = "deal_id")

cat(sprintf("  - Enhanced features: %d documents × %d variables\n",
            nrow(enhanced_features), ncol(enhanced_features)))

# ==============================================================================
# BLOCK 6: MULTIPLE NORMALIZATION TRACKS
# ==============================================================================

cat("\nBLOCK 6: Creating multiple normalization tracks...\n")

# Merge with original indices for metadata (sic2, year)
indices_merged <- indices_orig %>%
  left_join(enhanced_features, by = "deal_id")

# Ensure we have normalization metadata
if (!"sic2" %in% names(indices_merged)) {
  if ("target_primary_sic" %in% names(indices_merged)) {
    indices_merged$sic2 <- substr(as.character(indices_merged$target_primary_sic), 1, 2)
  } else {
    warning("No industry code available for normalization")
    indices_merged$sic2 <- "00"
  }
}

# Variables to normalize
vars_to_normalize <- c(
  # Readability
  "mda_read_fog_index",
  "mda_read_fk_grade", 
  "mda_read_avg_sentence_length",
  "mda_read_type_token_ratio",
  # Modal certainty
  "modal_certainty_raw",
  # Expanded numerics
  "comparative_density"
)

vars_to_normalize <- intersect(vars_to_normalize, names(indices_merged))

cat(sprintf("  - Variables to normalize: %s\n", paste(vars_to_normalize, collapse = ", ")))

# Hierarchical normalization function
normalize_hierarchical <- function(df, value_col, min_cell = 10) {
  
  v <- df[[value_col]]
  n <- length(v)
  out <- rep(NA_real_, n)
  lvl <- rep(NA_character_, n)
  
  # Level 1: sic2 × year
  if (all(c("sic2", "year_announced") %in% names(df))) {
    group_key <- paste0(df$sic2, "_", df$year_announced)
    group_sizes <- table(group_key)
    eligible <- names(group_sizes[group_sizes >= min_cell])
    
    for (grp in eligible) {
      idx <- which(group_key == grp & is.na(out) & !is.na(v))
      if (length(idx) >= min_cell) {
        out[idx] <- zscore(v[idx])
        lvl[idx] <- "sic2_year"
      }
    }
  }
  
  # Level 2: sic2 only
  if ("sic2" %in% names(df)) {
    group_sizes <- table(df$sic2)
    eligible <- names(group_sizes[group_sizes >= min_cell])
    
    for (grp in eligible) {
      idx <- which(df$sic2 == grp & is.na(out) & !is.na(v))
      if (length(idx) >= min_cell) {
        out[idx] <- zscore(v[idx])
        lvl[idx] <- "sic2"
      }
    }
  }
  
  # Level 3: year only
  if ("year_announced" %in% names(df)) {
    group_sizes <- table(df$year_announced)
    eligible <- names(group_sizes[group_sizes >= min_cell])
    
    for (grp in eligible) {
      idx <- which(df$year_announced == as.numeric(grp) & is.na(out) & !is.na(v))
      if (length(idx) >= min_cell) {
        out[idx] <- zscore(v[idx])
        lvl[idx] <- "year"
      }
    }
  }
  
  # Level 4: global
  idx <- which(is.na(out) & !is.na(v))
  if (length(idx) > 0) {
    out[idx] <- zscore(v[idx])
    lvl[idx] <- "global"
  }
  
  # Missing
  lvl[is.na(v)] <- "missing"
  
  list(norm = out, level = lvl)
}

# Create normalization tracks for each variable
for (var in vars_to_normalize) {
  
  cat(sprintf("  Normalizing %s...\n", var))
  
  # Track 1: Global z-score
  indices_merged[[paste0(var, "_global_z")]] <- zscore(indices_merged[[var]])
  
  # Track 2: Full hierarchical (adaptive)
  best_result <- NULL
  best_success <- 0
  
  for (min_cell in CONFIG$normalization$min_cell_sizes) {
    result <- normalize_hierarchical(indices_merged, var, min_cell)
    success_rate <- mean(result$level == "sic2_year", na.rm = TRUE)
    
    if (success_rate > best_success) {
      best_result <- result
      best_success <- success_rate
    }
    
    if (success_rate >= CONFIG$normalization$target_success_rate) {
      break
    }
  }
  
  indices_merged[[paste0(var, "_norm")]] <- best_result$norm
  indices_merged[[paste0(var, "_norm_level")]] <- best_result$level
  
  # Track 3: Percentile rank
  indices_merged[[paste0(var, "_pctl")]] <- percent_rank(indices_merged[[var]])
  
  cat(sprintf("    Success rate: %.1f%% at ideal level\n", 100 * best_success))
}

# ==============================================================================
# BLOCK 7: CREATE COMBINED INDICES
# ==============================================================================

cat("\nBLOCK 7: Creating combined indices...\n")

# Reading difficulty (average of Fog and FK)
if (all(c("mda_read_fog_index", "mda_read_fk_grade") %in% names(indices_merged))) {
  indices_merged$reading_difficulty_raw <- (
    indices_merged$mda_read_fog_index + indices_merged$mda_read_fk_grade
  ) / 2
  
  # Normalize
  result <- normalize_hierarchical(indices_merged, "reading_difficulty_raw", min_cell = 8)
  indices_merged$reading_difficulty_norm <- result$norm
  indices_merged$reading_difficulty_norm_level <- result$level
  
  cat(sprintf("  - Reading difficulty: mean=%.2f, sd=%.2f\n",
              mean(indices_merged$reading_difficulty_raw, na.rm = TRUE),
              sd(indices_merged$reading_difficulty_raw, na.rm = TRUE)))
}

# ==============================================================================
# BLOCK 8: VALIDATION DIAGNOSTICS
# ==============================================================================

cat("\nBLOCK 8: Running validation diagnostics...\n")

# Check protected terms in DFM
check_protected_terms <- function(dfm_obj, protected) {
  in_dfm <- intersect(tolower(protected), featnames(dfm_obj))
  missing <- setdiff(tolower(protected), featnames(dfm_obj))
  
  list(
    n_protected = length(protected),
    n_in_dfm = length(in_dfm),
    n_missing = length(missing),
    missing_terms = missing
  )
}

protected_check <- check_protected_terms(dfm_mda, CONFIG$protected_terms)
cat(sprintf("  - Protected terms: %d/%d in DFM\n", 
            protected_check$n_in_dfm, protected_check$n_protected))

if (protected_check$n_missing > 0) {
  cat(sprintf("  - Missing protected terms: %s\n",
              paste(head(protected_check$missing_terms, 10), collapse = ", ")))
}

# Correlation between new and existing indices
new_indices <- c("reading_difficulty_norm", "modal_certainty_raw", "comparative_density")
new_indices <- intersect(new_indices, names(indices_merged))

existing_indices <- c("operational_specificity_norm", "forward_looking_norm", 
                      "risk_disclosure_tfidf_norm", "tone_lm_norm")
existing_indices <- intersect(existing_indices, names(indices_merged))

if (length(new_indices) > 0 && length(existing_indices) > 0) {
  all_indices <- c(existing_indices, new_indices)
  cor_matrix <- cor(indices_merged[, all_indices], use = "pairwise.complete.obs")
  
  cat("\n  Correlation matrix (new vs existing indices):\n")
  print(round(cor_matrix, 3))
}

# ==============================================================================
# BLOCK 9: SAVE OUTPUTS
# ==============================================================================

cat("\nBLOCK 9: Saving outputs...\n")

# Enhanced features only
saveRDS(enhanced_features, CONFIG$paths$output_features)
cat(sprintf("  - Saved: %s\n", CONFIG$paths$output_features))

# Full enhanced indices
saveRDS(indices_merged, CONFIG$paths$output_indices)
cat(sprintf("  - Saved: %s (%d × %d)\n", 
            CONFIG$paths$output_indices,
            nrow(indices_merged), ncol(indices_merged)))

# ==============================================================================
# BLOCK 10: GENERATE REPORT
# ==============================================================================

cat("\nBLOCK 10: Generating enhancement report...\n")

report_lines <- c(
  "# NLP Pipeline Enhancement Report",
  "",
  sprintf("**Generated:** %s", Sys.time()),
  sprintf("**Documents:** %d", nrow(indices_merged)),
  "",
  "## New Indices Added",
  "",
  "### 1. Reading Difficulty",
  sprintf("- **Fog Index:** mean=%.2f, sd=%.2f",
          mean(indices_merged$mda_read_fog_index, na.rm = TRUE),
          sd(indices_merged$mda_read_fog_index, na.rm = TRUE)),
  sprintf("- **FK Grade:** mean=%.2f, sd=%.2f",
          mean(indices_merged$mda_read_fk_grade, na.rm = TRUE),
          sd(indices_merged$mda_read_fk_grade, na.rm = TRUE)),
  sprintf("- **Combined:** mean=%.2f, sd=%.2f",
          mean(indices_merged$reading_difficulty_raw, na.rm = TRUE),
          sd(indices_merged$reading_difficulty_raw, na.rm = TRUE)),
  "",
  "### 2. Modal Certainty",
  sprintf("- **Strong modals in DFM:** %s", paste(strong_in_dfm, collapse = ", ")),
  sprintf("- **Weak modals in DFM:** %s", paste(weak_in_dfm, collapse = ", ")),
  sprintf("- **Modal certainty:** mean=%.3f, sd=%.3f",
          mean(indices_merged$modal_certainty_raw, na.rm = TRUE),
          sd(indices_merged$modal_certainty_raw, na.rm = TRUE)),
  sprintf("- **Valid observations:** %d/%d",
          sum(!is.na(indices_merged$modal_certainty_raw)),
          nrow(indices_merged)),
  "",
  "### 3. Comparative Intensity",
  sprintf("- **Comparatives per doc:** mean=%.1f",
          mean(expanded_numerics$n_comparatives, na.rm = TRUE)),
  sprintf("- **Comparative density:** mean=%.3f per 1000 words",
          mean(indices_merged$comparative_density, na.rm = TRUE)),
  "",
  "## Protected Terms Check",
  sprintf("- **Protected terms defined:** %d", protected_check$n_protected),
  sprintf("- **Found in DFM:** %d", protected_check$n_in_dfm),
  sprintf("- **Missing from DFM:** %d", protected_check$n_missing),
  ""
)

if (protected_check$n_missing > 0) {
  report_lines <- c(report_lines,
    sprintf("- **Missing terms:** %s", 
            paste(protected_check$missing_terms, collapse = ", ")),
    ""
  )
}

report_lines <- c(report_lines,
  "## Normalization Tracks",
  "",
  "Each new index has multiple normalization options:",
  "- `*_global_z`: Global z-score",
  "- `*_norm`: Hierarchical (sic2×year → sic2 → year → global)",
  "- `*_pctl`: Percentile rank (0-1)",
  "",
  "## Output Files",
  sprintf("- Enhanced features: `%s`", CONFIG$paths$output_features),
  sprintf("- Enhanced indices: `%s`", CONFIG$paths$output_indices),
  "",
  "## Usage in Regressions",
  "",
  "```r",
  "# Load enhanced indices",
  "indices <- readRDS('data/interim/indices_enhanced.rds')",
  "",
  "# Example: Premium regression with new indices",
  "model <- feols(",
  "  premium_pct_w ~ ",
  "    operational_specificity_norm +",
  "    forward_looking_norm +",
  "    reading_difficulty_norm +      # NEW",
  "    modal_certainty_raw +          # NEW",
  "    controls... |",
  "    fe_industry_year,",
  "  data = indices,",
  "  vcov = ~sic2",
  ")",
  "```",
  ""
)

dir.create(dirname(CONFIG$paths$output_report), recursive = TRUE, showWarnings = FALSE)
writeLines(report_lines, CONFIG$paths$output_report)
cat(sprintf("  - Report: %s\n", CONFIG$paths$output_report))

# ==============================================================================
# SUMMARY
# ==============================================================================

cat("\n")
cat(rep("=", 78), "\n", sep = "")
cat("ENHANCEMENT COMPLETE\n")
cat(rep("=", 78), "\n", sep = "")

cat("\nNew indices added:\n")
cat("  1. reading_difficulty_norm   - Fog + FK combined, normalized\n")
cat("  2. modal_certainty_raw       - Strong/(Strong+Weak) modal ratio\n")
cat("  3. comparative_density       - Change-related words per 1000 words\n")
cat("  4. mda_read_fog_index        - Gunning Fog Index (raw)\n")
cat("  5. mda_read_fk_grade         - Flesch-Kincaid Grade (raw)\n")
cat("  6. mda_read_type_token_ratio - Vocabulary richness\n")

cat("\nNormalization tracks available:\n")
cat("  - *_global_z  : Simple z-score\n")
cat("  - *_norm      : Hierarchical industry×year\n")
cat("  - *_pctl      : Percentile rank\n")

cat(sprintf("\nEnhanced dataset: %d observations × %d variables\n",
            nrow(indices_merged), ncol(indices_merged)))

cat(sprintf("\nTimestamp: %s\n", Sys.time()))
