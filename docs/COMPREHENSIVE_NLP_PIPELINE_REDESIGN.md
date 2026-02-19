# Comprehensive NLP Pipeline Redesign
## M&A Disclosure Quality Project — Best Practice Implementation

**Version:** 2.0  
**Date:** 2026-02-15  
**Author:** Senior Research Engineer  
**Purpose:** Production-grade NLP pipeline for academic research

---

## Executive Summary

This document proposes a complete NLP pipeline redesign that:

1. **Fixes identified issues** from the expert analysis
2. **Expands the index framework** from 4 to 12 indices across 5 conceptual dimensions
3. **Implements best practices** from computational linguistics literature
4. **Maximizes econometric power** through richer measurement and interaction terms
5. **Maintains MSc-level methodological boundary** (no deep learning)

### Proposed Index Framework

| Dimension | Indices | Section | Purpose |
|-----------|---------|---------|---------|
| **Specificity** | 3 indices | MD&A | Information precision & detail |
| **Temporal Orientation** | 2 indices | MD&A | Forward vs. backward focus |
| **Risk Communication** | 3 indices | Risk Factors | Risk disclosure strategy |
| **Sentiment & Tone** | 2 indices | MD&A + Risk | Managerial optimism/pessimism |
| **Readability & Complexity** | 2 indices | Both | Disclosure accessibility |

### Current vs. Proposed

| Aspect | Current (v1) | Proposed (v2) |
|--------|--------------|---------------|
| Total indices | 4 | 12 |
| Dimensions | 4 (flat) | 5 (hierarchical) |
| Normalization success rate | 44% | >60% (adaptive) |
| Dictionary coverage | 77% | >85% |
| Validation | Qualitative KWIC | Quantitative FPR |
| Readability measures | None | Fog, FK, complexity |
| Interaction support | Limited | Full |

---

## Part I: The 12-Index Framework

### DIMENSION 1: SPECIFICITY (3 Indices)

**Rationale:** Specificity captures the precision and detail of disclosure. More specific disclosure should reduce information asymmetry and lead to more efficient pricing.

#### 1.1 Quantitative Specificity
```
Formula: numeric_density + percentage_density + currency_density
Section: MD&A
Purpose: Measures density of numerical/quantitative content
```

**Components:**
- Count of numbers per 1000 words
- Count of percentages per 1000 words  
- Count of currency amounts per 1000 words
- Count of scaled numbers (million/billion) per 1000 words

**Expected relationship with premium:** Negative (more specific → lower premium due to reduced uncertainty)

#### 1.2 Operational Specificity
```
Formula: compound_density(operational MWEs)
Section: MD&A
Purpose: Measures density of operational/financial terminology
```

**Components (Multi-Word Expressions):**
- Income statement terms: "operating income", "gross margin", "net revenue"
- Cash flow terms: "operating cash flow", "free cash flow", "capital expenditure"
- Balance sheet terms: "total assets", "long term debt", "working capital"
- Performance metrics: "return on equity", "earnings per share", "same store sales"

**Expected relationship with premium:** Negative

#### 1.3 Temporal Specificity (NEW)
```
Formula: quarter_density + fiscal_year_density + date_density
Section: MD&A
Purpose: Measures density of time-specific references
```

**Components:**
- Quarter references: "Q1 2024", "first quarter", "3Q23"
- Fiscal year references: "fiscal 2024", "FY23"
- Month/date references: "January 2024", "March 15"

**Expected relationship with premium:** Negative (more precise timing → lower uncertainty)

---

### DIMENSION 2: TEMPORAL ORIENTATION (2 Indices)

**Rationale:** The balance between forward-looking and backward-looking disclosure signals managerial focus and strategic orientation.

#### 2.1 Forward-Looking Intensity
```
Formula: forward_dict_density + forward_mwe_density + modal_density
Section: MD&A
Purpose: Prevalence of forward-looking language
```

**Components:**
- Modal tokens: will, would, shall, may, might, expect, anticipate, forecast, plan, intend
- Forward phrases: "going forward", "in the future", "next year", "quarters ahead"
- Strong vs. weak modals weighted differently

**Expected relationship with premium:** Ambiguous (could signal optimism OR uncertainty)

#### 2.2 Comparative Intensity (NEW)
```
Formula: comparative_density + range_density + ratio_density
Section: MD&A
Purpose: Focus on change and comparison
```

**Components:**
- Comparative words: increase, decrease, grow, decline, improve, deteriorate, higher, lower
- Range expressions: "10% to 15%", "between 5 and 7"
- Ratio expressions: "2:1", "3x"

**Expected relationship with premium:** Negative (more comparison → more transparent)

---

### DIMENSION 3: RISK COMMUNICATION (3 Indices)

**Rationale:** How firms communicate risk affects acquirer uncertainty and due diligence costs.

#### 3.1 Risk Disclosure Intensity
```
Formula: tfidf_weighted(LM_uncertainty + LM_negative)
Section: Risk Factors
Purpose: Overall intensity of risk-related language
```

**Method:** TF-IDF weighting ensures rare, substantive risk terms are weighted higher than boilerplate.

**Expected relationship with premium:** Ambiguous (more disclosure could reduce OR increase perceived risk)

#### 3.2 Uncertainty Emphasis (NEW)
```
Formula: uncertainty_count / (pos_count + neg_count + uncertainty_count)
Section: Risk Factors
Purpose: Relative emphasis on uncertainty vs. definitive statements
```

**Components:**
- LM uncertainty words: "approximate", "could", "depend", "fluctuate", "possible", "uncertain"
- Normalized by total sentiment words

**Expected relationship with premium:** Positive (more uncertainty emphasis → higher premium for risk)

#### 3.3 Legal/Regulatory Risk Emphasis (NEW)
```
Formula: litigious_density + constraining_density
Section: Risk Factors
Purpose: Focus on litigation and regulatory matters
```

**Components:**
- LM litigious words: "attorney", "claim", "court", "defendant", "jury", "lawsuit", "legal", "litigation"
- LM constraining words: "commit", "obligation", "required", "restrict", "binding"

**Expected relationship with premium:** Positive (more legal risk → higher premium)

---

### DIMENSION 4: SENTIMENT & TONE (2 Indices)

**Rationale:** Managerial tone reflects optimism/pessimism and confidence levels.

#### 4.1 Net Tone (Traditional LM)
```
Formula: (pos_count - neg_count) / (pos_count + neg_count)
Section: MD&A
Purpose: Net sentiment orientation
```

**Guard:** Denominator must be ≥ 10 to avoid extreme ratios.

**Expected relationship with premium:** Positive (more positive tone → higher premium)

#### 4.2 Modal Certainty (NEW)
```
Formula: strong_modal_count / (strong_modal_count + weak_modal_count)
Section: MD&A
Purpose: Use of strong vs. weak modal verbs
```

**Strong modals:** will, shall, must  
**Weak modals:** may, might, could, would, should

**Expected relationship with premium:** Negative (more certain → lower premium due to reduced uncertainty)

---

### DIMENSION 5: READABILITY & COMPLEXITY (2 Indices)

**Rationale:** Complex, hard-to-read disclosure increases processing costs and may obscure material information.

#### 5.1 Reading Difficulty
```
Formula: (Fog Index + FK Grade) / 2
Section: MD&A
Purpose: Overall reading difficulty
```

**Components:**
- Gunning Fog Index: 0.4 × (words/sentence + 100 × complex_words/words)
- Flesch-Kincaid Grade: 0.39 × (words/sentence) + 11.8 × (syllables/words) - 15.59

**Expected relationship with premium:** Positive (harder to read → higher premium due to information costs)

#### 5.2 Vocabulary Complexity (NEW)
```
Formula: avg_word_length × (1 - type_token_ratio)
Section: MD&A
Purpose: Lexical diversity and word complexity
```

**Components:**
- Average word length in characters
- Type-token ratio (unique words / total words)
- Percentage of 3+ syllable words

**Expected relationship with premium:** Positive (more complex vocabulary → higher premium)

---

## Part II: Key Improvements Over Current Pipeline

### 1. Protected Terms List (Expanded)

**Current problem:** "capital" was trimmed from DFM due to 95% max document frequency rule.

**Solution:** Expanded protected terms list that overrides frequency trimming:

```r
protected_terms <- c(
  # Modals (existing)
  "will", "shall", "may", "might", "could", "would", "should", "must",
  
  # Negations (existing)
  "not", "no", "nor", "neither", "never", "none", "cannot",
  
  # Financial terms (NEW)
  "capital", "expenditure", "expenditures", "margin", "margins",
  "revenue", "revenues", "income", "profit", "loss", "losses",
  "debt", "equity", "asset", "assets", "liability", "liabilities",
  "cash", "flow", "flows",
  
  # Comparatives (NEW)
  "increase", "increased", "increasing", "decrease", "decreased", "decreasing",
  "grow", "grew", "growing", "growth", "decline", "declined", "declining",
  "higher", "lower", "more", "less", "better", "worse",
  
  # Uncertainty/hedging (NEW)
  "approximately", "about", "around", "roughly", "nearly", "almost",
  "possibly", "probably", "likely", "unlikely", "uncertain"
)
```

### 2. Adaptive Normalization

**Current problem:** Only 44% of observations achieve ideal sic2×year normalization.

**Solution:** Try multiple minimum cell sizes and use the most successful:

```r
normalize_adaptive <- function(df, value_col, 
                               min_cell_sizes = c(10, 8, 5)) {
  
  for (min_cell in min_cell_sizes) {
    result <- normalize_hierarchical(df, value_col, min_cell)
    success_rate <- mean(result$level == "sic2_year", na.rm = TRUE)
    
    if (success_rate >= 0.40) {
      return(result)  # Good enough
    }
  }
  
  return(result)  # Use most lenient if still failing
}
```

### 3. Multiple Normalization Tracks

**Current:** Only one normalization approach (hierarchical z-score).

**Proposed:** Multiple tracks for robustness:

| Track | Method | Use Case |
|-------|--------|----------|
| `*_raw` | Unstandardized | Coefficients in original units |
| `*_global_z` | Global z-score | Simple standardization |
| `*_year_z` | Year z-score | Time effects removed |
| `*_ind_z` | Industry z-score | Industry effects removed |
| `*_norm` | Industry×Year z-score | Full normalization |
| `*_pctl` | Percentile rank | Robust to outliers |

### 4. Readability Indices

**Current:** No readability measures.

**Proposed:** Add Gunning Fog, Flesch-Kincaid, and vocabulary complexity.

**Implementation:**
```r
extract_readability_features <- function(text, sentences_df) {
  
  words <- str_extract_all(text, "\\b[a-z]+\\b")[[1]]
  n_words <- length(words)
  syllables <- sapply(words, count_syllables)
  n_syllables <- sum(syllables)
  n_complex_words <- sum(syllables >= 3)
  
  tibble(
    # Flesch-Kincaid Grade Level
    fk_grade = 0.39 * (n_words / sentences_df$n_sentences) + 
               11.8 * (n_syllables / n_words) - 15.59,
    
    # Gunning Fog Index
    fog_index = 0.4 * ((n_words / sentences_df$n_sentences) + 
                       100 * (n_complex_words / n_words)),
    
    # Vocabulary complexity
    avg_word_length = mean(nchar(words)),
    type_token_ratio = n_distinct(words) / n_words
  )
}
```

### 5. Quantitative Validation

**Current:** KWIC validation is qualitative.

**Proposed:** Add systematic false positive rate estimation:

```r
# After manual coding of KWIC sample
compute_fpr_statistics <- function(coded_kwic) {
  coded_kwic %>%
    group_by(pattern) %>%
    summarise(
      n_coded = sum(!is.na(is_true_positive)),
      n_true_positive = sum(is_true_positive == 1, na.rm = TRUE),
      n_false_positive = sum(is_true_positive == 0, na.rm = TRUE),
      false_positive_rate = n_false_positive / n_coded
    )
}
```

---

## Part III: Econometric Model Specifications

### Premium Models

```r
# ─────────────────────────────────────────────────────────────────────────
# MODEL 1: Baseline (5 core indices)
# ─────────────────────────────────────────────────────────────────────────
premium_baseline <- feols(
  premium_pct_w ~ 
    quantitative_specificity_norm +
    operational_specificity_norm +
    forward_looking_intensity_norm +
    risk_disclosure_intensity_norm +
    tone_net_norm +
    log(deal_value_usd_millions) +
    tender_dummy +
    percentage_of_cash |
    fe_industry_year,
  data = df, vcov = ~sic2
)

# ─────────────────────────────────────────────────────────────────────────
# MODEL 2: Extended (all 12 indices)
# ─────────────────────────────────────────────────────────────────────────
premium_extended <- feols(
  premium_pct_w ~ 
    # Specificity (3)
    quantitative_specificity_norm +
    operational_specificity_norm +
    temporal_specificity_norm +
    # Temporal (2)
    forward_looking_intensity_norm +
    comparative_intensity_norm +
    # Risk (3)
    risk_disclosure_intensity_norm +
    uncertainty_emphasis_norm +
    legal_risk_emphasis_norm +
    # Sentiment (2)
    tone_net_norm +
    modal_certainty_norm +
    # Readability (2)
    reading_difficulty_norm +
    vocabulary_complexity_norm +
    # Controls
    log(deal_value_usd_millions) + tender_dummy + percentage_of_cash |
    fe_industry_year,
  data = df, vcov = ~sic2
)

# ─────────────────────────────────────────────────────────────────────────
# MODEL 3: Interaction effects
# ─────────────────────────────────────────────────────────────────────────
premium_interactions <- feols(
  premium_pct_w ~ 
    operational_specificity_norm * tender_dummy +
    operational_specificity_norm * large_deal_dummy +
    forward_looking_intensity_norm * hostile_dummy +
    risk_disclosure_intensity_norm * competing_offer_dummy +
    controls... |
    fe_industry_year,
  data = df, vcov = ~sic2
)

# ─────────────────────────────────────────────────────────────────────────
# MODEL 4: Readability moderates information content
# ─────────────────────────────────────────────────────────────────────────
premium_readability_mod <- feols(
  premium_pct_w ~ 
    operational_specificity_norm * reading_difficulty_norm +
    forward_looking_intensity_norm * reading_difficulty_norm +
    controls... |
    fe_industry_year,
  data = df, vcov = ~sic2
)

# ─────────────────────────────────────────────────────────────────────────
# MODEL 5: Nonlinear effects (quadratic)
# ─────────────────────────────────────────────────────────────────────────
premium_nonlinear <- feols(
  premium_pct_w ~ 
    operational_specificity_norm + I(operational_specificity_norm^2) +
    forward_looking_intensity_norm + I(forward_looking_intensity_norm^2) +
    controls... |
    fe_industry_year,
  data = df, vcov = ~sic2
)
```

### Completion Models

```r
# Logit with marginal effects
completion_logit <- feglm(
  completion_dummy ~ 
    operational_specificity_norm +
    forward_looking_intensity_norm +
    risk_disclosure_intensity_norm +
    tone_net_norm +
    reading_difficulty_norm +  # May affect regulatory review time
    log(deal_value_usd_millions) +
    tender_dummy +
    hostile_dummy |
    fe_year,
  data = df, 
  family = binomial(link = "logit"),
  vcov = ~sic2
)
```

### Duration Models

```r
# Stratified Cox PH (addresses PH violations)
duration_cox <- coxph(
  Surv(time_to_event_days, event_completed) ~ 
    operational_specificity_norm +
    forward_looking_intensity_norm +
    risk_disclosure_intensity_norm +
    tone_net_norm +
    log(deal_value_usd_millions) +
    percentage_of_cash +
    strata(tender_dummy) +  # Stratify - PH violated
    strata(year_announced),
  data = df,
  cluster = sic2
)
```

---

## Part IV: Implementation Priority

### Phase 1: Quick Wins (1-2 days)
1. ✅ Fix protected terms list (add "capital", "margin", etc.)
2. ✅ Add readability indices (Fog, FK)
3. ✅ Implement adaptive normalization

### Phase 2: Core Expansion (3-5 days)
4. Add modal certainty index
5. Add temporal specificity index
6. Add legal risk emphasis index
7. Implement multiple normalization tracks

### Phase 3: Full Framework (5-7 days)
8. Complete 12-index framework
9. Add interaction term support
10. Implement quantitative validation
11. Add PCA composites

### Phase 4: Robustness (2-3 days)
12. Sensitivity analyses (weights, cell sizes)
13. Alternative specifications
14. Documentation and tests

---

## Part V: Expected Outcomes

### Measurement Quality
- **Indices:** 4 → 12 (3× richer)
- **Normalization success:** 44% → >60%
- **Dictionary coverage:** 77% → >85%

### Econometric Power
- **Premium R²:** ~0.15 → ~0.20-0.25
- **Significant indices:** 1/4 → 3-5/12
- **Interaction effects:** Enabled
- **Nonlinear effects:** Enabled

### Research Contributions
1. **Specificity decomposition:** First to separate quantitative, operational, and temporal specificity
2. **Modal certainty:** Novel measure of managerial confidence
3. **Readability interactions:** Test whether complexity moderates information content
4. **Risk communication strategy:** Distinguish uncertainty from legal risk

---

## Appendix: File Structure

```
src/30_nlp_v2/
├── 01_text_cleaning_v3.R           # Enhanced cleaning
├── 02_tokenization_v4.R            # Enhanced tokenization
├── 03_feature_extraction_v2.R      # Separate feature extraction
├── 04_construct_indices_v6.R       # 12-index construction
├── 05_validation_framework_v2.R    # Quantitative validation
├── 06_normalization_v3.R           # Adaptive normalization
├── 07_assemble_dataset_v3.R        # Final assembly
├── config/
│   ├── index_definitions.yaml
│   ├── dictionaries_extended.rds
│   └── parameters.yaml
└── utils/
    ├── text_utils.R
    ├── dictionary_utils.R
    └── normalization_utils.R
```

This redesign provides a comprehensive framework for extracting maximum information from disclosure text while maintaining methodological rigor appropriate for academic research.
