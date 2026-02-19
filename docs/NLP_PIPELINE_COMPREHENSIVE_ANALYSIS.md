# ==============================================================================
# COMPREHENSIVE NLP PIPELINE ANALYSIS AND FIXES
# ==============================================================================
# Date: 2026-02-15
# Purpose: Systematic analysis of the 6-module NLP pipeline with fixes
# ==============================================================================

## EXECUTIVE SUMMARY

### Current Pipeline Structure

```
Module 1: Text Cleaning (01_text_cleaning.R)
    └── Input: deals_with_10k_text_analysis.rds
    └── Output: cleaned_text.rds
    
Module 2: Tokenization (02_tokenization_dtm.R)
    └── Input: cleaned_text.rds
    └── Output: dfm_objects.rds, numeric_features.rds, tokens_objects.rds
    
Module 3: Index Construction (03_construct_indices.R)
    └── Input: dfm_objects.rds, numeric_features.rds, analysis_config.rds
    └── Output: disclosure_indices.rds
    
Module 4: KWIC Validation (04_kwic_validation.R)
    └── Validation of regex patterns
    
Module 5: Descriptive Analysis (05_descriptive_analysis.R)
    └── EDA and summary statistics
    
Module 6: Dataset Assembly (06_assemble_analysis_dataset.R)
    └── Merge with deal-level variables
    
Module 7: Enhancement (07_nlp_enhancement.R) [NEW]
    └── Readability, modal certainty, comparative density
```

---

## IDENTIFIED ISSUES AND FIXES

### ISSUE 1: Missing Protected Terms in DFM (CRITICAL)

**Problem:** The tokenization module trims terms appearing in >95% of documents,
which removes critical financial terms like "capital", "revenue", "income", etc.

**Evidence from enhancement run:**
```
Missing protected terms: capital, revenue, income, loss, debt, assets, 
                         liabilities, cash, flows, sales
```

**Root Cause:** The `protected_terms` list in Module 2 only includes modals and
negations, but not core financial vocabulary.

**Fix:** Expand `protected_terms` to include all finance-critical terms.

---

### ISSUE 2: Arbitrary 0.5/0.5 Weights in Operational Specificity

**Problem:** The operational specificity index uses:
```r
operational_specificity_raw <- 0.5 * numeric_density + 0.5 * operational_compound_density
```

These weights are arbitrary and not justified by theory or data.

**Recommendation:** 
- Option A: Use data-driven weights (PCA first component loadings)
- Option B: Report both components separately in regressions
- Option C: Document the arbitrary choice and run sensitivity analysis

---

### ISSUE 3: 56% Normalization Fallback Rate

**Problem:** Only 44% of observations achieve ideal sic2×year normalization.

**Evidence:**
```
Normalizing mda_read_fog_index...
    Success rate: 44.2% at ideal level
```

**Cause:** Many sic2×year cells have <10 observations.

**Recommendation:** 
- Reduce min_cell_size from 10 to 5 for readability indices
- Use adaptive min_cell_size: try 10, then 8, then 5
- Report multiple normalization tracks for robustness

---

### ISSUE 4: 23% Dictionary Terms Unmatched

**Problem:** 23% of LM dictionary terms don't match any DFM features.

**Cause:** 
1. Multi-word terms preprocessed away (correct behavior)
2. Some single-word terms may be rare or trimmed

**Recommendation:**
- Audit unmatched terms to distinguish multiword vs. actually-missing
- For truly-missing single-word terms, check if they're being trimmed

---

### ISSUE 5: No Readability Measures (Previously)

**Status:** FIXED by Module 7 enhancement.

Now includes:
- Gunning Fog Index
- Flesch-Kincaid Grade Level
- Type-Token Ratio
- Average sentence length

---

## NEW FINDINGS FROM ENHANCEMENT MODULE

### Significant New Result: Comparative Density

```
comparative_density: β = -1.454 (SE = 0.533), p < 0.01
```

**Interpretation:** Firms that discuss change more (increases, decreases, growth,
decline) are associated with **lower acquisition premiums**.

**Theoretical Explanation:** 
- More explicit discussion of performance trajectory → lower information asymmetry
- Acquirer has clearer picture of target's direction → less uncertainty premium

### Modal Certainty Pattern

```
modal_certainty_raw: mean = 0.352, sd = 0.109
```

**Interpretation:** On average, 35% of modal verbs are "strong" (will/shall/must)
vs "weak" (may/might/could/would/should).

**Correlation with other indices:**
- Correlated with forward_looking_norm (r = 0.26)
- Negatively correlated with reading_difficulty (r = -0.12)

---

## RECOMMENDED IMPLEMENTATION PRIORITY

### Phase 1: Critical Fixes (Do Now)

1. **Fix protected terms list** in Module 2
   - Add: capital, revenue, income, profit, loss, debt, equity, 
          asset(s), liability/liabilities, cash, flow(s), earnings, sales
   - Add: margin(s), expenditure(s), growth, increase, decrease

2. **Re-run pipeline** after protected terms fix
   - Check if "capital" now appears in DFM
   - Verify dictionary match rates improve

### Phase 2: Robustness Improvements

3. **Add adaptive normalization** to Module 3
   - Try min_cell = 10, then 8, then 5
   - Report success rates for each

4. **Decompose operational specificity**
   - Report numeric_density and operational_compound_density separately
   - Let the regression determine optimal weighting

### Phase 3: Novel Contributions

5. **Formalize comparative density** as distinct index
   - Already implemented in Module 7
   - Shows significant relationship with premium

6. **Add temporal specificity** subindex
   - Fiscal year references
   - Quarter references
   - Year-over-year comparisons

---

## CORRELATION MATRIX (ALL INDICES)

From enhancement module output:

```
                             op_spec  fwd_look  risk_tfidf  tone   read_diff  modal_cert  comp_dens
operational_specificity_norm   1.000    -0.363       0.010  -0.059    -0.039      -0.045      0.170
forward_looking_norm          -0.363     1.000       0.015   0.145    -0.101       0.262     -0.046
risk_disclosure_tfidf_norm     0.010     0.015       1.000  -0.116    -0.046      -0.058      0.024
tone_lm_norm                  -0.059     0.145      -0.116   1.000    -0.016       0.117      0.039
reading_difficulty_norm       -0.039    -0.101      -0.046  -0.016     1.000      -0.123     -0.098
modal_certainty_raw           -0.045     0.262      -0.058   0.117    -0.123       1.000     -0.066
comparative_density            0.170    -0.046       0.024   0.039    -0.098      -0.066      1.000
```

**Key Observations:**
1. **No multicollinearity issues** - all correlations < 0.4
2. **Operational specificity & forward-looking are negatively correlated** (r=-0.36)
   - Firms that are more specific are less forward-looking (or vice versa)
3. **Modal certainty correlates with forward-looking** (r=0.26)
   - More "will/must" language in forward-looking statements
4. **Comparative density correlates with operational specificity** (r=0.17)
   - More change-related words → more numeric specificity

---

## ECONOMETRIC IMPLICATIONS

### Current Model Results

| Variable | Premium Coefficient | SE | Significant? |
|----------|--------------------|----|--------------|
| operational_specificity_norm | -2.76 to -2.82 | ~1.1-1.5 | Yes (p<0.05) |
| forward_looking_norm | Not significant | - | No |
| risk_disclosure_tfidf_norm | Not significant | - | No |
| tone_lm_norm | Not significant | - | No |
| reading_difficulty_norm | +4.2 | 3.7 | No (p≈0.26) |
| modal_certainty_raw | +32.4 | 26.9 | No (p≈0.23) |
| **comparative_density** | **-1.45** | **0.53** | **Yes (p<0.01)** |

### Recommended Model Specification

```r
# Baseline (current)
premium ~ operational_specificity_norm + forward_looking_norm + 
          risk_disclosure_tfidf_norm + tone_lm_norm + controls | FE

# Extended (with new indices)
premium ~ operational_specificity_norm + forward_looking_norm + 
          risk_disclosure_tfidf_norm + tone_lm_norm +
          reading_difficulty_norm + comparative_density + controls | FE

# Decomposed specificity (recommended)
premium ~ numeric_density + operational_compound_density + 
          comparative_density + forward_looking_norm + 
          risk_disclosure_tfidf_norm + tone_lm_norm + controls | FE
```

---

## NEXT STEPS

1. Run the fixed tokenization module (02_tokenization_dtm_v4.R)
2. Re-run Module 3 to regenerate indices
3. Re-run Module 7 enhancement
4. Compare results before/after protected terms fix
5. Document any changes to coefficients

