# Text Processing and Analysis Action Plan
## M&A Disclosure Quality Project

**Date:** 2025-02-03  
**Context:** Pre-deal 10-K disclosure (MD&A + Risk Factors) → Deal outcomes (premium, completion)  
**Methodological Boundary:** MSc-level NLP (BoW, dictionaries, tf-idf, KWIC) – NO transformers/deep learning

---

## EXECUTIVE SUMMARY

This plan operationalizes the text analysis pipeline for your M&A disclosure project. It translates your research design into **six sequential modules**, each grounded in your course materials and defensible for an MSc thesis. The pipeline transforms raw 10-K text into **four theory-driven disclosure indices** ready for econometric analysis.

**Core principle:** Maximize transparency, replicability, and academic defensibility. Every choice is motivated by literature or course precedent.

---

## DATA CONTEXT

**Input file:** `deals_with_10k_text_analysis.rds` (1,846 deals)  
**Key text columns:**
- `mda_text` – Management's Discussion and Analysis
- `risk_factors_text` – Item 1A Risk Factors  
- `mda_word_count`, `risk_word_count` – initial counts

**Timing constraint enforced:** Each 10-K filed ≥90 days before deal announcement  
**One-to-one matching:** Each deal → single pre-announcement 10-K

---

## MODULE 1: TEXT CLEANING AND PREPROCESSING

### 1.1 Objectives
- Remove EDGAR boilerplate and artifacts that inflate word counts without information
- Standardize text for valid cross-document comparison
- Prepare corpus for tokenization while preserving relevant punctuation and numerical information

### 1.2 Operations (in sequence)

**Step 1.1: EDGAR-specific cleaning**
```r
# Rationale: EDGAR HTML artifacts distort frequency counts and readability
# Course ref: slides.pdf encoding/preprocessing

- Remove HTML entities (e.g., &nbsp;, &#160;, &#8217;)
- Strip HTML tags (<div>, <table>, etc.)
- Remove XBRL tags and structured data blocks
- Delete table headers/footers patterns
- Remove page numbers, filing metadata headers
```

**Step 1.2: Standardization**
```r
# Rationale: Ensure consistent encoding and prevent tokenization errors
# Course ref: 06_Preprocessing_and_cleaning.pdf encoding

- Convert to UTF-8 encoding (handle any malformed characters)
- Normalize whitespace (collapse multiple spaces/newlines to single space)
- Convert to lowercase (EXCEPT for financial acronyms – handle separately)
- Preserve numerical patterns (keep decimals, percentages, currency)
```

**Step 1.3: Boilerplate detection (conservative)**
```r
# Rationale: Repeated legal disclaimers don't reflect managerial disclosure choice
# Approach: Pattern-based removal (NOT aggressive)

- Remove "Forward-Looking Statements" disclaimers (standard SEC language)
- Remove "Website Access to Reports" sections
- Flag but DON'T remove Item 1A Risk Factor standard intro language
  (it's part of the section structure)
```

**Step 1.4: Preserve information-bearing elements**
```r
# Critical: These are FEATURES for your indices, not noise

- KEEP: Numbers (1,234.56), percentages (23.4%), currency ($M, $B)
- KEEP: Dates (Q1 2022, fiscal 2021)
- KEEP: Operational bigrams (will extract separately)
- KEEP: Modal verbs (will, may, could) – needed for forward-looking dictionary
```

### 1.3 Validation checks
- Compare word counts before/after → document removal ≥30% suggests over-aggressive cleaning
- Spot-check 5 random documents (head/tail) to ensure text remains interpretable
- Verify numerical patterns intact (essential for Operational Specificity index)

### 1.4 Output
- `data/interim/cleaned_text.rds`: cleaned text columns (`mda_clean`, `risk_clean`)
- `reports/01_cleaning_report.md`: Summary statistics (chars removed, distributions)

---

## MODULE 2: TOKENIZATION AND DOCUMENT-TERM MATRIX CONSTRUCTION

### 2.1 Objectives
- Create standardized token representation for dictionary matching
- Build DTM structures for both unigrams and bigrams
- Implement stopword removal with finance-aware exceptions

### 2.2 Operations

**Step 2.1: Unigram tokenization**
```r
# Course ref: slides.pdf tokenization, 05_Preprocessing.pdf

library(quanteda)

# Tokenize to unigrams
tokens_uni <- tokens(
  corpus(cleaned_text),
  what = "word",
  remove_punct = FALSE,  # We'll handle selectively
  remove_numbers = FALSE, # KEEP – needed for specificity
  remove_separators = TRUE
)
```

**Step 2.2: Stopword removal (finance-aware)**
```r
# Rationale: Standard stopwords reduce noise; but finance terms differ
# Course ref: 05_Preprocessing.pdf stopword removal example (Figure 1 vs 2)

# Base: English stopwords from quanteda
stopwords_base <- stopwords("en")

# EXCEPTIONS (keep these even though they're in standard stopwords):
# - "will", "may", "could", "would" (forward-looking indicators)
# - "more", "less", "increase", "decrease" (operational change)
# - "not" (negation needed for sentiment)

stopwords_adjusted <- setdiff(stopwords_base, 
  c("will", "may", "could", "would", "might", "shall",
    "not", "no", "more", "less"))

tokens_uni_clean <- tokens_remove(tokens_uni, stopwords_adjusted)
```

**Step 2.3: Bigram tokenization (separate track)**
```r
# Rationale: Operational specificity requires multiword expressions
# Example: "capital expenditure", "gross margin", "operating cash flow"

tokens_bi <- tokens_ngrams(tokens_uni, n = 2)

# Bigram stopwords: remove if BOTH tokens are stopwords
# But keep if at least one is substantive
```

**Step 2.4: DTM construction**
```r
# Create separate DTMs for analysis flexibility

# Unigram DTM (for dictionaries, readability, volume)
dtm_uni <- dfm(tokens_uni_clean)

# Bigram DTM (for operational specificity)
dtm_bi <- dfm(tokens_bi)

# Apply document frequency trimming (conservative)
# Keep terms in ≥2 documents AND ≤95% documents
dtm_uni_trimmed <- dfm_trim(dtm_uni, min_docfreq = 2, docfreq_type = "count",
                              max_docfreq = 0.95, docfreq_type = "prop")
```

### 2.3 Validation checks
- Vocabulary size reasonable? (expect 8k-15k unigrams post-trim)
- Inspect top 50 terms → should see financial/operational words, not artifacts
- Sparsity <98% for unigrams, <99.5% for bigrams

### 2.4 Output
- `data/interim/dtm_unigram.rds`
- `data/interim/dtm_bigram.rds`
- `data/interim/tokens_objects.rds` (for KWIC analysis)
- `reports/02_tokenization_report.md`: Vocabulary statistics, frequency distributions

---

## MODULE 3: CONSTRUCT MEASUREMENT – FOUR DISCLOSURE INDICES

### 3.1 Operational Specificity / Informational Richness

**Conceptual definition:** Density of quantitative, operational detail in MD&A  
**Literature anchor:** Garcia & Norli (2012) geographic dispersion; Li (2008) specificity  
**Course reference:** Bag-of-words frequency, bigram extraction

**Implementation:**

```r
# Component 3.1.1: Numerical density
# Count pattern matches per 1000 words

patterns_numeric <- c(
  "\\d+\\.\\d+",      # Decimals: 23.4
  "\\d+%",            # Percentages: 15%
  "\\$\\d+",          # Currency: $500
  "\\d+\\s?(million|billion|thousand)", # Scaled numbers
  "Q[1-4]\\s?\\d{4}" # Quarter references
)

numeric_density <- str_count(mda_clean, patterns_numeric) / 
                   (mda_word_count / 1000)

# Component 3.1.2: Operational bigram frequency
# Predefined list based on financial reporting context

operational_bigrams <- c(
  "capital_expenditure", "operating_income", "gross_margin",
  "cash_flow", "revenue_growth", "operating_cash",
  "earnings_per_share", "return_on", "working_capital",
  "debt_to", "interest_expense", "tax_rate",
  "market_share", "same_store", "unit_volume",
  "average_selling_price", "capacity_utilization"
)

# Count in bigram DTM
bigram_matches <- dtm_bi[, operational_bigrams]
bigram_density <- rowSums(bigram_matches) / (mda_word_count / 1000)

# Component 3.1.3: Combined index (standardized)
operational_specificity_raw <- 0.6 * numeric_density + 
                                0.4 * bigram_density

# Industry × year normalization
# Rationale: Reporting norms vary by sector and time
operational_specificity <- operational_specificity_raw %>%
  group_by(target_primary_sic_2digit, year_announced) %>%
  mutate(op_spec_norm = scale(operational_specificity_raw)) %>%
  pull(op_spec_norm)
```

**Validation:** KWIC check for top/bottom decile deals on bigram matches

---

### 3.2 Forward-Looking Intensity

**Conceptual definition:** Prevalence of forward-oriented disclosure in MD&A  
**Literature anchor:** Muslu et al. (2015) forward-looking; Li (2010) textual proxies  
**Course reference:** Dictionary matching (slides2024.pdf LM sentiment approach)

**Implementation:**

```r
# Forward-looking dictionary (regex-based)
# Based on SEC plain-English guidance + finance literature

forward_looking_dict <- c(
  # Verbs
  "will", "expect", "anticipate", "believe", "plan", "intend",
  "forecast", "project", "estimate", "target", "outlook",
  "guidance", "predict", "foresee", "envision",
  
  # Phrases (detected via regex)
  "going forward", "in the future", "next year", "upcoming",
  "fiscal 20\\d{2}", "quarters? ahead", "long[- ]?term",
  "short[- ]?term", "near[- ]?term"
)

# Count matches in MD&A (case-insensitive)
fwd_count <- str_count(tolower(mda_clean), 
                        paste(forward_looking_dict, collapse = "|"))

# Normalize by MD&A length
forward_looking_intensity_raw <- fwd_count / (mda_word_count / 1000)

# Industry × year normalization
forward_looking_intensity <- forward_looking_intensity_raw %>%
  group_by(target_primary_sic_2digit, year_announced) %>%
  mutate(fwd_norm = scale(forward_looking_intensity_raw)) %>%
  pull(fwd_norm)
```

**Validation:**  
- KWIC analysis on "expect*", "anticip*", "will" → verify context is genuinely forward-looking
- Manual review 10 high/low cases → check for false positives (legal "will" vs. future "will")

---

### 3.3 Execution Risk / Uncertainty Disclosure

**Conceptual definition:** Intensity of risk/uncertainty language in Risk Factors section  
**Literature anchor:** Loughran & McDonald (2011) uncertainty + negative wordlists  
**Course reference:** slides2024.pdf LM dictionary application, tf-idf weighting

**Implementation:**

```r
# Load Loughran-McDonald dictionaries
# Source: https://sraf.nd.edu/loughranmcdonald-master-dictionary/

lm_negative <- read.csv("config/LM_negative.csv")$word
lm_uncertainty <- read.csv("config/LM_uncertainty.csv")$word

# Combine for risk disclosure composite
risk_dict <- union(lm_negative, lm_uncertainty)

# Method 3.3.1: Simple frequency (baseline)
risk_count <- str_count(tolower(risk_factors_clean),
                        paste(risk_dict, collapse = "|"))

risk_intensity_simple <- risk_count / (risk_word_count / 1000)

# Method 3.3.2: TF-IDF weighted (preferred)
# Rationale: Rare risk terms more informative than boilerplate "may", "could"
# Course ref: slides2024.pdf tf-idf formula

# Compute term frequency per document
tf <- dtm_uni_risk / rowSums(dtm_uni_risk)

# Compute IDF
N <- nrow(dtm_uni_risk)
df <- colSums(dtm_uni_risk > 0)
idf <- log(N / df)

# TF-IDF matrix
tfidf <- tf * matrix(rep(idf, each = nrow(tf)), nrow = nrow(tf))

# Extract risk terms only
tfidf_risk <- tfidf[, colnames(tfidf) %in% risk_dict]

# Average TF-IDF of risk terms per document
risk_disclosure_tfidf <- rowMeans(tfidf_risk, na.rm = TRUE)

# Industry × year normalization (preferred measure)
risk_disclosure <- risk_disclosure_tfidf %>%
  group_by(target_primary_sic_2digit, year_announced) %>%
  mutate(risk_norm = scale(risk_disclosure_tfidf)) %>%
  pull(risk_norm)
```

**Validation:**
- Correlation check: simple vs. tf-idf versions (expect 0.6-0.8)
- Inspect top tf-idf risk terms → should see substantive risks ("litigation", "regulatory"), not generic modals
- KWIC on "uncertain*", "risk*" in top/bottom quintiles

---

### 3.4 Managerial Tone (Sentiment)

**Conceptual definition:** Net sentiment orientation in MD&A  
**Literature anchor:** Loughran & McDonald (2011); Davis et al. (2012) 10-K tone  
**Course reference:** slides2024.pdf LM pos/neg, tone = (pos-neg)/(pos+neg)

**Implementation:**

```r
# Load LM positive/negative dictionaries
lm_positive <- read.csv("config/LM_positive.csv")$word
lm_negative <- read.csv("config/LM_negative.csv")$word

# Count in MD&A
pos_count <- str_count(tolower(mda_clean), 
                       paste(lm_positive, collapse = "|"))
neg_count <- str_count(tolower(mda_clean), 
                       paste(lm_negative, collapse = "|"))

# Component measures
pos_share <- pos_count / mda_word_count
neg_share <- neg_count / mda_word_count

# Net tone (standard LM formula)
tone_lm_raw <- (pos_count - neg_count) / (pos_count + neg_count + 1)

# Industry × year normalization
# Rationale: Tone varies systematically by sector (tech optimistic, utilities neutral)
tone_lm <- tone_lm_raw %>%
  group_by(target_primary_sic_2digit, year_announced) %>%
  mutate(tone_norm = scale(tone_lm_raw)) %>%
  pull(tone_norm)

# Alternative: Positive/negative shares (separate regressors)
# Allows non-linear effects and interaction with fundamentals
```

**Validation:**
- Sanity check: tone distribution should be roughly symmetric, centered near 0
- Compare tone with deal outcomes (UNIVARIATE) → expect correlation ≠0
- KWIC on top positive/negative terms → verify context (not sarcasm/negation issues)

---

### 3.5 Summary of indices

| Index | Section | Dictionary/Method | Normalization | Unit |
|-------|---------|-------------------|---------------|------|
| `operational_specificity` | MD&A | Numeric patterns + bigrams | Industry × year | Std score |
| `forward_looking_intensity` | MD&A | Forward-looking dict | Industry × year | Std score |
| `risk_disclosure` | Risk Factors | LM uncertainty + negative (tf-idf) | Industry × year | Std score |
| `tone_lm` | MD&A | LM positive − negative | Industry × year | Std score |

All indices saved in `data/processed/disclosure_indices.rds`

---

## MODULE 4: KWIC VALIDATION (QUALITATIVE CHECK)

### 4.1 Objectives
- **Validate dictionary accuracy** via manual inspection of keyword contexts
- **Detect false positives** (e.g., "risk" in "at-risk youth program" ≠ firm risk)
- **Assess construct validity** (do high-scoring documents actually exhibit the construct?)

### 4.2 Operations

**Step 4.1: Generate KWIC tables**
```r
# Course ref: 08_Keywords_in_context.pdf

library(quanteda)

# Example: Forward-looking keywords
kwic_forward <- kwic(tokens_mda, pattern = "expect*", 
                     window = 10, valuetype = "glob")

# Extract for top/bottom decile deals by forward_looking_intensity
top_deals <- deals %>% arrange(desc(forward_looking_intensity)) %>% head(10)
bottom_deals <- deals %>% arrange(forward_looking_intensity) %>% head(10)

kwic_top <- kwic_forward %>% filter(docname %in% top_deals$deal_id)
kwic_bottom <- kwic_forward %>% filter(docname %in% bottom_deals$deal_id)
```

**Step 4.2: Manual review protocol**
- For EACH of the four indices, generate KWIC for the 3 most diagnostic keywords
- Review 5 random instances per keyword in high/low scoring groups
- Flag patterns:
  - **False positives:** Keyword present but construct absent (e.g., "risk" in product name)
  - **False negatives:** Construct present but missed by dictionary (synonyms?)
  - **Context dependence:** Negative + negation = positive (handled by simple dict? NO → note as limitation)

**Step 4.3: Document refinements**
- If false positive rate >20% for a keyword → consider removal or regex refinement
- If false negative evident → expand dictionary (cautiously, with justification)

### 4.3 Output
- `reports/03_kwic_validation.md`: Summary table with examples and decisions
- `output/tables/kwic_samples.xlsx`: Exportable KWIC snippets for thesis appendix

---

## MODULE 5: DESCRIPTIVE ANALYSIS AND CROSS-VALIDATION

### 5.1 Objectives
- Characterize distributions of disclosure indices
- Check for multicollinearity among indices
- Validate against "known groups" (industry differences, time trends)

### 5.2 Operations

**Step 5.1: Univariate distributions**
```r
# Histograms, Q-Q plots, summary statistics
# Expected: Near-normal after industry × year normalization

indices <- c("operational_specificity", "forward_looking_intensity",
             "risk_disclosure", "tone_lm")

for (idx in indices) {
  p <- ggplot(deals, aes_string(x = idx)) +
    geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
    labs(title = paste("Distribution of", idx),
         x = idx, y = "Frequency") +
    theme_minimal()
  
  ggsave(paste0("output/figures/hist_", idx, ".png"), p, width = 8, height = 5)
}
```

**Step 5.2: Correlation matrix**
```r
# Correlation among indices
cor_matrix <- cor(deals[, indices], use = "complete.obs")

# Heatmap
library(corrplot)
corrplot(cor_matrix, method = "color", type = "upper",
         addCoef.col = "black", tl.col = "black", tl.srt = 45)

# Concern: if |r| > 0.7 between indices → multicollinearity in regressions
# Mitigation: Use indices separately or create composite via PCA (only if needed)
```

**Step 5.3: Industry differences (known-groups validity)**
```r
# Hypothesis: Tech firms (SIC 73xx) higher forward-looking than utilities (SIC 49xx)

anova_fwd <- aov(forward_looking_intensity ~ as.factor(target_primary_sic_2digit), 
                 data = deals)
summary(anova_fwd)

# Expectation: Significant F-statistic → indices capture real between-industry variation
```

**Step 5.4: Time trends**
```r
# Plot average index values by year_announced
# Check for drift (if significant → year FE necessary in regressions)

trends <- deals %>%
  group_by(year_announced) %>%
  summarise(across(all_of(indices), mean, na.rm = TRUE))

ggplot(trends %>% pivot_longer(cols = indices), 
       aes(x = year_announced, y = value, color = name)) +
  geom_line() +
  facet_wrap(~name, scales = "free_y") +
  theme_minimal()
```

### 5.3 Output
- `reports/04_descriptive_analysis.md`: Summary statistics, distributional checks
- `output/figures/`: Histograms, correlation heatmap, time trends
- `output/tables/correlation_matrix.csv`

---

## MODULE 6: FINAL DATASET ASSEMBLY FOR ECONOMETRICS

### 6.1 Objectives
- Merge disclosure indices with deal characteristics and outcomes
- Construct control variables (e.g., readability if needed)
- Create analysis flags and subsamples
- Export clean, documented dataset for modeling

### 6.2 Operations

**Step 6.1: Merge indices with deal data**
```r
# Start with original deals_with_10k_text_analysis.rds
deals_final <- deals_base %>%
  left_join(disclosure_indices, by = "deal_id") %>%
  # Outcomes
  mutate(
    premium_1d = premium_paid_1_day_prior_to_announcement,
    premium_1w = premium_paid_1_week_prior_to_announcement,
    premium_4w = premium_paid_4_weeks_prior_to_announcement,
    
    completion = case_when(
      deal_status == "Completed" ~ 1,
      deal_status == "Withdrawn" ~ 0,
      TRUE ~ NA_real_
    ),
    
    time_to_close = number_of_days_between_date_announced_and_date_effective
  )
```

**Step 6.2: Construct supplementary controls (optional)**
```r
# Readability (if emphasized in lit review, but use cautiously)
# Flesch-Kincaid or Fog index on MD&A

library(quanteda.textstats)

readability_mda <- textstat_readability(corpus_mda, measure = "Flesch.Kincaid")

deals_final <- deals_final %>%
  left_join(readability_mda, by = c("deal_id" = "doc_id"))

# NOTE: Readability correlated with complexity (firm size, industry)
# Don't interpret as "quality" – use only as control if literature requires
```

**Step 6.3: Define analysis sample flags**
```r
# Flag observations with complete data for each analysis

deals_final <- deals_final %>%
  mutate(
    # Premium analysis sample
    sample_premium = !is.na(premium_1d) & 
                     !is.na(operational_specificity) &
                     !is.na(target_share_price_1_day_prior_to_announcement_usd) &
                     target_share_price_1_day_prior_to_announcement_usd > 0,
    
    # Completion analysis sample
    sample_completion = !is.na(completion) &
                        !is.na(risk_disclosure),
    
    # Time-to-close (conditional on completion)
    sample_duration = completion == 1 & !is.na(time_to_close)
  )

# Report sample sizes
table(deals_final$sample_premium)
table(deals_final$sample_completion)
table(deals_final$sample_duration)
```

**Step 6.4: Create industry × year FE identifiers**
```r
# For regression specs

deals_final <- deals_final %>%
  mutate(
    sic2 = substr(target_primary_sic, 1, 2),
    ind_year_fe = paste0(sic2, "_", year_announced)
  )
```

**Step 6.5: Final data quality checks**
```r
# Check for extreme outliers (>5 SD from mean) in indices
# Flag but don't drop – reviewer can assess sensitivity

for (idx in indices) {
  mean_idx <- mean(deals_final[[idx]], na.rm = TRUE)
  sd_idx <- sd(deals_final[[idx]], na.rm = TRUE)
  
  deals_final[[paste0(idx, "_outlier")]] <- 
    abs(deals_final[[idx]] - mean_idx) > 5 * sd_idx
}

# Report outlier counts
sapply(deals_final[, paste0(indices, "_outlier")], sum, na.rm = TRUE)
```

### 6.3 Output
- `data/processed/deals_analysis_ready.rds`: Final dataset
- `data/processed/deals_analysis_ready.csv`: Human-readable backup
- `data/processed/data_dictionary_final.csv`: Column definitions
- `reports/05_final_dataset_report.md`: Sample sizes, missing data patterns, outliers

---

## PIPELINE ORCHESTRATION

### Master script: `scripts/run_text_analysis_pipeline.R`

```r
# TEXT ANALYSIS PIPELINE FOR M&A DISCLOSURE PROJECT
# Author: [Your name]
# Date: 2025-02-03
# Description: Orchestrates all text processing modules

library(tidyverse)
library(quanteda)
library(quanteda.textstats)

# Configuration
source("config/analysis_parameters.R")  # Paths, thresholds, dictionaries

# MODULE 1: Cleaning
cat("MODULE 1: Text cleaning...\n")
source("src/nlp/01_text_cleaning.R")

# MODULE 2: Tokenization
cat("MODULE 2: Tokenization and DTM...\n")
source("src/nlp/02_tokenization.R")

# MODULE 3: Index construction
cat("MODULE 3: Disclosure indices...\n")
source("src/nlp/03_construct_indices.R")

# MODULE 4: KWIC validation
cat("MODULE 4: KWIC validation...\n")
source("src/nlp/04_kwic_validation.R")

# MODULE 5: Descriptive analysis
cat("MODULE 5: Descriptive analysis...\n")
source("src/nlp/05_descriptive_analysis.R")

# MODULE 6: Final dataset
cat("MODULE 6: Final dataset assembly...\n")
source("src/merge/06_assemble_analysis_dataset.R")

cat("\nPipeline complete. Check reports/ for summaries.\n")
```

---

## TIMELINE ESTIMATE (assuming half-time work)

| Module | Estimated days | Dependencies |
|--------|---------------|--------------|
| 1. Cleaning | 2-3 | None |
| 2. Tokenization | 1-2 | Module 1 |
| 3. Index construction | 3-4 | Module 2 |
| 4. KWIC validation | 2-3 | Module 3 |
| 5. Descriptive analysis | 2 | Module 3 |
| 6. Final assembly | 1-2 | Modules 3, 5 |
| **TOTAL** | **11-16 days** | Sequential |

*Buffer for iteration and debugging: add 20-30%*

---

## KEY DECISIONS AND RATIONALE SUMMARY

| Decision | Choice | Justification | Course Ref |
|----------|--------|---------------|------------|
| Stopword removal | Yes, but keep modals/negation | Forward-looking needs "will"; sentiment needs "not" | 05_Preprocessing.pdf |
| Stemming/lemmatization | NO | Loses precision for financial terms; not standard in LM papers | Literature (L&M 2011) |
| Bigrams | Yes (separate DTM) | Operational specificity requires multiword units | slides.pdf bigrams |
| TF-IDF | Yes (for risk disclosure) | Weights rare substantive risks over boilerplate | slides2024.pdf tf-idf |
| Industry × year norm | YES (all indices) | Reporting norms vary; enables within-cohort comparison | Literature standard |
| Readability | Include as control only | Correlated with complexity, not quality per se | Cautious interpretation |
| KWIC validation | Mandatory | Defends dictionary validity against reviewers | 08_KWIC.pdf |

---

## LIMITATIONS TO ACKNOWLEDGE (for thesis methods section)

1. **Dictionary limitations:** Bag-of-words cannot capture negation ("not risky"), sarcasm, or context-dependent meaning. KWIC validation mitigates but doesn't eliminate false positives.

2. **No causal claims:** Indices are correlational. Disclosure is endogenous (managers choose it). Timing + controls reduce confounding but don't establish causality.

3. **Construct validity:** Indices proxy constructs (e.g., "informational richness") but don't measure them perfectly. Triangulation with multiple methods would strengthen (but is beyond MSc scope).

4. **Generalizability:** US 10-Ks only. Results may not extend to other disclosure regimes or languages.

5. **Methodological boundary:** Deliberately excludes deep learning / embeddings. These could capture richer semantics but are black-box and hard to defend in academic writing.

---

## NEXT STEPS AFTER THIS PLAN

1. **Review and approve this plan** → adjust if needed based on data quirks or advisor input
2. **Implement Module 1-2** (cleaning + tokenization) → sanity-check outputs
3. **Pilot Module 3** on subsample (e.g., 100 deals) → validate indices before full run
4. **Full pipeline execution** → generate all outputs
5. **Write methods section** using this plan as scaffold (already defensible and structured)
6. **Proceed to econometric modeling** (separate phase, not covered here)

---

## REFERENCES (for thesis methods section)

- **Loughran, T., & McDonald, B. (2011).** When is a liability not a liability? Textual analysis, dictionaries, and 10-Ks. *Journal of Finance*, 66(1), 35-65.
- **Garcia, D., & Norli, Ø. (2012).** Geographic dispersion and stock returns. *Journal of Financial Economics*, 106(3), 547-565.
- **Li, F. (2008).** Annual report readability, current earnings, and earnings persistence. *Journal of Accounting and Economics*, 45(2-3), 221-247.
- **Muslu, V., et al. (2015).** Forward-looking MD&A disclosures and the information environment. *Management Science*, 61(5), 931-948.
- **Course materials:** BAN432 Applied Textual Data Analysis (slides + lectures on preprocessing, sentiment, KWIC, tf-idf)

---

**END OF ACTION PLAN**

*This document is designed to be both a working guide and a skeleton for your thesis methodology chapter. Every choice is documented and defensible.*
