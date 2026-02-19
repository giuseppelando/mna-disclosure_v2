# MODULE 2: TOKENIZATION AND DTM CONSTRUCTION - DETAILED PLAN

**Input:** `data/interim/cleaned_text.rds` (2,391 deals with `mda_clean`, `risk_clean`)  
**Output:** DTM objects + token objects for subsequent index construction  
**Tools:** quanteda package (modern, well-maintained, superior to tm)

---

## STRATEGIC OVERVIEW

### Why This Module Matters
Tokenization and DTM construction form the **bridge** between cleaned text and measurable constructs. The choices made here directly affect:
1. **Operational Specificity Index** → needs bigrams preserved
2. **Forward-Looking Intensity** → needs modal verbs retained
3. **Risk Disclosure** → needs proper vocabulary for LM dictionary matching
4. **Managerial Tone** → needs negation words preserved

### Key Design Principles
1. **Separate pipelines for unigrams and bigrams** (different uses)
2. **Finance-aware stopword handling** (NOT generic English stopwords)
3. **Conservative trimming** (preserve rare but meaningful terms)
4. **Preserve tokens object** (needed for KWIC validation in Module 4)

---

## CONCEPTUAL WORKFLOW

```
cleaned_text.rds
    ↓
[Create quanteda corpus objects]
    ↓
[Tokenize with finance-aware rules]
    ↓
┌─────────────────┬─────────────────┐
│   UNIGRAM PATH  │   BIGRAM PATH   │
│                 │                 │
│ • Stopwords adj.│ • n-grams(2)    │
│ • Keep modals   │ • Keep compounds│
│ • Keep negation │ • Operational   │
│                 │   bigrams focus │
└────────┬────────┴────────┬────────┘
         ↓                 ↓
    [Create DFM]     [Create DFM]
         ↓                 ↓
    [Trim vocab]     [Trim vocab]
         ↓                 ↓
  dtm_mda_uni      dtm_mda_bi
  dtm_risk_uni     dtm_risk_bi
         ↓                 ↓
  [Save as RDS objects + metadata]
```

---

## DETAILED IMPLEMENTATION PLAN

### BLOCK 0: Setup and Load

**Purpose:** Load cleaned data, initialize quanteda, verify input

**Operations:**
```r
library(quanteda)
library(quanteda.textstats)
library(dplyr)
library(stringr)

# Load cleaned data
cleaned_data <- readRDS("data/interim/cleaned_text.rds")

# Verify expected columns
stopifnot(c("deal_id", "mda_clean", "risk_clean") %in% names(cleaned_data))

# Create document IDs (quanteda requires unique doc names)
cleaned_data <- cleaned_data %>%
  mutate(
    doc_id_mda = paste0("deal_", deal_id, "_mda"),
    doc_id_risk = paste0("deal_", deal_id, "_risk")
  )
```

**Validation:** Print sample sizes, check for NA text

---

### BLOCK 1: Corpus Construction

**Purpose:** Convert cleaned text to quanteda corpus objects

**Why corpus objects?**
- Preserve metadata (deal_id, year, SIC)
- Enable efficient tokenization
- Standard input for quanteda functions

**Operations:**
```r
# MD&A corpus
corpus_mda <- corpus(
  cleaned_data$mda_clean,
  docnames = cleaned_data$doc_id_mda,
  docvars = data.frame(
    deal_id = cleaned_data$deal_id,
    year_announced = cleaned_data$year_announced,
    target_primary_sic = cleaned_data$target_primary_sic
  )
)

# Risk Factors corpus
corpus_risk <- corpus(
  cleaned_data$risk_clean,
  docnames = cleaned_data$doc_id_risk,
  docvars = data.frame(
    deal_id = cleaned_data$deal_id,
    year_announced = cleaned_data$year_announced,
    target_primary_sic = cleaned_data$target_primary_sic
  )
)
```

**Validation:** 
- `ndoc(corpus_mda)` should equal 2,391
- `docvars(corpus_mda)` should show metadata

---

### BLOCK 2: Finance-Aware Stopword List

**Purpose:** Define which words to remove/keep for financial text analysis

**Critical Decision:** Standard English stopwords REMOVE important financial signals!

**Problems with generic stopwords:**
- "will", "may", "could" → **ESSENTIAL** for forward-looking detection
- "not", "no" → **ESSENTIAL** for sentiment (negation)
- "more", "less", "increase", "decrease" → **ESSENTIAL** for operational disclosure

**Our Approach:**
```r
# Start with base English stopwords
stopwords_base <- stopwords("en", source = "snowball")  # ~175 words

# EXCEPTIONS: Words to KEEP even though they're in base list
keep_words <- c(
  # Modal verbs (forward-looking indicators)
  "will", "shall", "may", "might", "could", "would", "should",
  
  # Negation (sentiment crucial)
  "not", "no", "nor", "neither", "never", "none",
  
  # Quantitative/change words (operational specificity)
  "more", "most", "less", "least", "few", "many", "several",
  "above", "below", "over", "under",
  "up", "down", "increase", "decrease",
  
  # Hedge/uncertainty (risk disclosure)
  "can", "cannot", "must", "need"
)

# Adjusted stopword list
stopwords_finance <- setdiff(stopwords_base, keep_words)

# Result: ~150 stopwords (instead of 175)
# Removes: "the", "a", "an", "and", "or", "of", "to", "in", etc.
# Keeps: modals, negation, quantitative language
```

**Justification:** 
- Loughran & McDonald (2011) emphasize that "not" reverses sentiment
- Muslu et al. (2015) use modal verbs for forward-looking detection
- Our indices NEED these words

---

### BLOCK 3: Tokenization - Unigrams

**Purpose:** Create unigram tokens for dictionary-based indices

**Tokenization settings:**
```r
tokens_mda_uni <- tokens(
  corpus_mda,
  what = "word",              # Word-level tokens
  remove_punct = TRUE,        # Remove . , ; : etc. (already cleaned)
  remove_symbols = TRUE,      # Remove @, #, etc.
  remove_numbers = FALSE,     # KEEP numbers (for Operational Specificity)
  remove_url = TRUE,          # Remove URLs (if any remain)
  remove_separators = TRUE,   # Remove excess whitespace
  split_hyphens = FALSE       # Keep "long-term", "short-term" intact
)

# Apply finance-aware stopword removal
tokens_mda_uni <- tokens_remove(tokens_mda_uni, stopwords_finance)

# Same for Risk Factors
tokens_risk_uni <- tokens(corpus_risk, ...) %>%
  tokens_remove(stopwords_finance)
```

**Why keep numbers?**
- Operational Specificity index counts numerical patterns
- If removed here, we'd have to re-tokenize later (inefficient)

**Why keep hyphens intact?**
- "Long-term" ≠ "long" + "term" semantically
- Financial compounds are meaningful units

---

### BLOCK 4: Tokenization - Bigrams

**Purpose:** Create bigram tokens for Operational Specificity index

**Approach:**
```r
# Generate bigrams from unigram tokens (efficient)
tokens_mda_bi <- tokens_ngrams(
  tokens_mda_uni,  # Use already-cleaned unigram tokens
  n = 2,           # Bigrams only
  concatenator = "_"  # Join with underscore: "capital_expenditure"
)

# Same for Risk Factors
tokens_risk_bi <- tokens_ngrams(tokens_risk_uni, n = 2, concatenator = "_")
```

**Why from unigram tokens?**
- Ensures consistent stopword handling
- Bigrams automatically exclude stopword-only pairs ("the_of")

**Expected bigrams:**
- "capital_expenditure", "operating_income", "gross_margin"
- "cash_flow", "market_share", "debt_equity"

---

### BLOCK 5: Document-Feature Matrix (DFM) - Unigrams

**Purpose:** Convert tokens to frequency matrices for analysis

**Construction:**
```r
# MD&A unigram DFM
dfm_mda_uni <- dfm(tokens_mda_uni)

# Risk Factors unigram DFM
dfm_risk_uni <- dfm(tokens_risk_uni)
```

**Initial statistics (before trimming):**
```r
cat("MD&A unigram DFM:\n")
cat("  Documents:", ndoc(dfm_mda_uni), "\n")
cat("  Features:", nfeat(dfm_mda_uni), "\n")
cat("  Sparsity:", sparsity(dfm_mda_uni), "\n")

# Expected:
# Documents: 2,391
# Features: ~25,000-40,000 (before trimming)
# Sparsity: 99.5%+ (normal for text data)
```

---

### BLOCK 6: Vocabulary Trimming - Conservative Approach

**Purpose:** Remove extremely rare/common terms without losing information

**Rationale:**
- Terms in only 1 document → likely OCR errors, typos, firm-specific jargon
- Terms in >95% documents → uninformative (e.g., "company", "business")

**Approach:**
```r
# Trim MD&A unigrams
dfm_mda_uni_trim <- dfm_trim(
  dfm_mda_uni,
  min_docfreq = 2,           # Must appear in ≥2 documents
  max_docfreq = 0.95,        # Must appear in <95% of documents
  docfreq_type = "prop"      # Proportions (not counts)
)

# Expected reduction: ~40,000 → ~12,000-15,000 features
# Sparsity: 99.5% → 98.5% (less sparse but still sparse)

# Same for Risk Factors
dfm_risk_uni_trim <- dfm_trim(dfm_risk_uni, ...)
```

**Why conservative?**
- Rare financial terms may be meaningful (e.g., "mezzanine", "tranche")
- Better to keep and not use than lose and need later

**Alternative for stricter trimming (if needed):**
```r
# min_docfreq = 5   # In ≥5 documents (~0.2%)
# max_docfreq = 0.90  # In <90% documents
```

---

### BLOCK 7: DFM - Bigrams

**Purpose:** Create bigram frequency matrix for Operational Specificity

**Construction:**
```r
# MD&A bigrams
dfm_mda_bi <- dfm(tokens_mda_bi)

# Trim more aggressively (bigrams are sparser)
dfm_mda_bi_trim <- dfm_trim(
  dfm_mda_bi,
  min_docfreq = 3,      # Bigrams rarer than unigrams
  max_docfreq = 0.90,   # Keep even moderately common ones
  docfreq_type = "prop"
)

# Risk Factors bigrams (less important, but keep for consistency)
dfm_risk_bi_trim <- dfm_trim(dfm(tokens_risk_bi), ...)
```

**Expected bigram vocabulary:**
- Before trimming: ~100,000-200,000 bigrams
- After trimming: ~5,000-10,000 bigrams
- Includes operational bigrams we care about

---

### BLOCK 8: Validation and Diagnostics

**Purpose:** Verify DFMs are sensible before saving

**Checks:**

1. **Top features make sense**
```r
# MD&A top unigrams
topfeatures(dfm_mda_uni_trim, n = 20)
# Expected: "revenue", "income", "year", "business", "2020", "results", ...

# MD&A top bigrams
topfeatures(dfm_mda_bi_trim, n = 20)
# Expected: "operating_income", "fiscal_year", "cash_flow", ...
```

2. **Finance-specific words retained**
```r
# Check that important words survived trimming
important_words <- c("revenue", "margin", "debt", "capital", 
                     "risk", "uncertainty", "expect", "will")

for (word in important_words) {
  present <- word %in% featnames(dfm_mda_uni_trim)
  cat(sprintf("%-15s: %s\n", word, ifelse(present, "✓", "✗")))
}
```

3. **Vocabulary size reasonable**
```r
# Summary statistics
summary_stats <- data.frame(
  Section = c("MD&A Unigram", "MD&A Bigram", "Risk Unigram", "Risk Bigram"),
  Documents = c(ndoc(dfm_mda_uni_trim), ndoc(dfm_mda_bi_trim), 
                ndoc(dfm_risk_uni_trim), ndoc(dfm_risk_bi_trim)),
  Features = c(nfeat(dfm_mda_uni_trim), nfeat(dfm_mda_bi_trim),
               nfeat(dfm_risk_uni_trim), nfeat(dfm_risk_bi_trim)),
  Sparsity = c(sparsity(dfm_mda_uni_trim), sparsity(dfm_mda_bi_trim),
               sparsity(dfm_risk_uni_trim), sparsity(dfm_risk_bi_trim))
)

print(summary_stats)
```

4. **Check for numeric tokens**
```r
# Numbers should be present (for Operational Specificity)
numeric_features <- featnames(dfm_mda_uni_trim)[grepl("^\\d", featnames(dfm_mda_uni_trim))]
cat("Numeric features found:", length(numeric_features), "\n")
cat("Examples:", head(numeric_features, 10), "\n")
```

---

### BLOCK 9: Save Outputs

**Purpose:** Persist all objects needed for Module 3 (Index Construction)

**What to save:**

1. **Tokens objects** (for KWIC in Module 4)
```r
saveRDS(
  list(
    mda_uni = tokens_mda_uni,
    mda_bi = tokens_mda_bi,
    risk_uni = tokens_risk_uni,
    risk_bi = tokens_risk_bi
  ),
  "data/interim/tokens_objects.rds"
)
```

2. **DFM objects** (for index construction in Module 3)
```r
saveRDS(
  list(
    mda_uni = dfm_mda_uni_trim,
    mda_bi = dfm_mda_bi_trim,
    risk_uni = dfm_risk_uni_trim,
    risk_bi = dfm_risk_bi_trim
  ),
  "data/interim/dfm_objects.rds"
)
```

3. **Vocabulary lists** (for documentation)
```r
saveRDS(
  list(
    mda_uni_features = featnames(dfm_mda_uni_trim),
    mda_bi_features = featnames(dfm_mda_bi_trim),
    risk_uni_features = featnames(dfm_risk_uni_trim),
    risk_bi_features = featnames(dfm_risk_bi_trim),
    stopwords_used = stopwords_finance
  ),
  "data/interim/vocabulary_lists.rds"
)
```

4. **Summary statistics** (for reporting)
```r
saveRDS(summary_stats, "data/interim/tokenization_summary.rds")
```

---

### BLOCK 10: Generate Report

**Purpose:** Document choices and validation for thesis

**Report structure:**
```markdown
# Tokenization and DTM Construction Report

## Input
- Cleaned text: 2,391 deals
- Sections: MD&A + Risk Factors

## Tokenization Settings
- Stopwords: 150 finance-aware (175 base - 25 kept)
- Kept: modals, negation, quantitative words
- Numbers: preserved
- Hyphens: preserved

## DFM Summary
[Insert summary_stats table]

## Top Features
### MD&A Unigrams (top 20)
[Insert topfeatures output]

### MD&A Bigrams (top 20)
[Insert topfeatures output]

## Validation
- Finance words retained: [list]
- Numeric tokens present: [count]
- Vocabulary size reasonable: Yes/No

## Next Steps
- Ready for Module 3: Index Construction
- KWIC validation deferred to Module 4
```

---

## OUTPUT FILES SUMMARY

After Module 2 completion:

```
data/interim/
├─ tokens_objects.rds        # For KWIC (Module 4)
├─ dfm_objects.rds            # For indices (Module 3)
├─ vocabulary_lists.rds       # For documentation
└─ tokenization_summary.rds  # For reporting

reports/
└─ 02_tokenization_report.md  # Human-readable summary
```

---

## EXPECTED RESULTS (Sanity Checks)

### Vocabulary Sizes (Approximate)

| DFM | Before Trim | After Trim | Notes |
|-----|-------------|------------|-------|
| MD&A Unigram | 35,000-45,000 | 12,000-18,000 | Includes numbers |
| MD&A Bigram | 150,000-250,000 | 6,000-12,000 | Operational phrases |
| Risk Unigram | 25,000-35,000 | 8,000-12,000 | Less diverse |
| Risk Bigram | 100,000-150,000 | 4,000-8,000 | Less important |

### Sparsity (After Trim)

- Unigrams: 97-99% (normal for bag-of-words)
- Bigrams: 99-99.5% (sparser, expected)

### Top Features Should Include

**MD&A Unigrams:** revenue, income, year, business, operations, results, financial, market, products, services, sales, million, billion, percent, increase, decrease

**MD&A Bigrams:** fiscal_year, operating_income, net_income, cash_flow, capital_expenditures, gross_margin, interest_expense, income_tax, net_revenue, operating_expenses

---

## METHODOLOGICAL JUSTIFICATION (For Thesis)

### Why quanteda over tm?

1. **Modern and maintained:** tm package aging, quanteda actively developed
2. **Better performance:** Handles large corpora efficiently (2,391 docs × 10k words = 24M tokens)
3. **Sparse matrix handling:** Native support for large, sparse DFMs
4. **Unicode support:** Better UTF-8 handling (important for EDGAR text)

### Why separate unigram/bigram pipelines?

1. **Different uses:** Unigrams for dictionaries, bigrams for operational specificity
2. **Efficiency:** Don't want stopword bigrams ("the_of", "and_the")
3. **Trimming thresholds:** Bigrams need different frequency cutoffs

### Why this stopword approach?

1. **Finance-specific:** Generic lists remove financial signals
2. **Literature support:** Loughran & McDonald (2011) emphasize context-specific word lists
3. **Transparency:** Explicit list of kept words (replicable)

---

## RISKS AND MITIGATIONS

### Risk 1: Vocabulary too large (memory issues)

**Symptom:** R crashes during DFM construction  
**Mitigation:** Increase min_docfreq to 3 or 5  
**Impact:** Minimal (rare words likely not in dictionaries anyway)

### Risk 2: Important words trimmed

**Symptom:** Dictionary matches too low in Module 3  
**Mitigation:** Check vocabulary_lists.rds, re-run with lower min_docfreq  
**Impact:** Module 3 would need re-run

### Risk 3: Numbers tokenized oddly

**Symptom:** "23.4" becomes "23" and "4" separately  
**Mitigation:** Already handled by quanteda's default tokenizer  
**Impact:** None expected (tested in cleaning phase)

---

## TIME ESTIMATE

- Block 0-1: 1 minute (load, corpus creation)
- Block 2-4: 2 minutes (tokenization)
- Block 5-7: 5-10 minutes (DFM construction + trimming, depends on machine)
- Block 8-10: 2 minutes (validation + save)

**Total: ~10-15 minutes** for full pipeline execution

---

## NEXT STEPS AFTER MODULE 2

With tokens and DFMs ready:

1. **Module 3:** Construct 4 disclosure indices using DFMs
2. **Module 4:** KWIC validation using tokens objects
3. **Module 5:** Descriptive analysis of indices
4. **Module 6:** Final dataset assembly for econometrics

---

**END OF DETAILED PLAN**

Ready to implement!
