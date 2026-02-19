# MODULE 3: INDEX CONSTRUCTION - TECHNICAL DOCUMENTATION (UPDATED)

**Date:** 2025-02-04 (Updated with Official LM CSV Integration)  
**Module:** Disclosure Quality Index Construction  
**Status:** Production-ready, MSc-defensible (Enhanced)  
**Major Update:** Complete official LM dictionaries from CSV (not subset)

---

## EXECUTIVE SUMMARY

This module constructs four theory-driven disclosure quality indices from document-feature matrices (DFMs) generated in Module 2. 

**MAJOR UPDATE (2025-02-04):**  
We now use the **complete Loughran-McDonald Master Dictionary** directly from the official CSV file, rather than a hand-curated subset. This provides:

- ✅ **6-26× better coverage** (350+ positive vs 60, 2,350+ negative vs 90)
- ✅ **Unimpeachable source** ("official LM CSV" vs "core subset")
- ✅ **Maximum defensibility** for MSc thesis
- ✅ **Future-proof** (tracks LM updates automatically)

---

## DICTIONARY SOURCE COMPARISON

### Before: Hand-Curated Subset

| Category | Terms | Source | Defensibility |
|----------|-------|--------|---------------|
| Positive | ~60 | Hand-selected | Moderate (subset rationale needed) |
| Negative | ~90 | Hand-selected | Moderate (completeness questioned) |
| Uncertainty | ~85 | Hand-selected | Moderate (potential omission bias) |
| Litigious | ~80 | Hand-selected | Moderate (cherry-picking concerns) |

**Thesis language:** "We use a core subset of LM dictionaries..."  
**Reviewer concern:** "Why only 60 positive words when LM has 350+?"

### After: Official CSV (Current)

| Category | Terms | Source | Defensibility |
|----------|-------|--------|---------------|
| Positive | ~350 | Official LM CSV | **Excellent** (complete dictionary) |
| Negative | ~2,350 | Official LM CSV | **Excellent** (no omissions) |
| Uncertainty | ~300 | Official LM CSV | **Excellent** (full coverage) |
| Litigious | ~900 | Official LM CSV | **Excellent** (unimpeachable) |

**Thesis language:** "We extract all terms from the official Loughran-McDonald Master Dictionary CSV..."  
**Reviewer response:** ✅ "Standard approach, replicable."

---

## METHODOLOGICAL ADVANTAGE: SEPARATION OF CONCERNS

### Problem with Previous Approach

The hand-curated subset **conflated** LM official categories with project extensions:

```r
# PROBLEMATIC:
forward_looking_dict <- c(
  lm_dicts$modal,  # ← LM category (subset)
  "expect", "anticipate"  # ← Project additions
)
```

**Reviewer question:** "Is this LM or yours? How do I replicate exactly?"

### Solution: Explicit Separation

New configuration **cleanly separates**:

```r
# From config/analysis_config.rds:

config$lm_positive     # ← Official LM (complete, 350 terms)
config$lm_negative     # ← Official LM (complete, 2,350 terms)
config$lm_uncertainty  # ← Official LM (complete, 300 terms)
config$lm_litigious    # ← Official LM (complete, 900 terms)

# vs

config$forward_looking_extended  # ← Project list (65 terms, explicit)
config$operational_phrases       # ← Project list (80 phrases, explicit)
```

**Result:** Crystal-clear provenance, exact replicability.

---

## THE FOUR INDICES (UNCHANGED LOGIC, BETTER DATA)

### 1. Operational Specificity

**Measurement:** Same as before  
**Change:** No direct impact (uses numeric patterns + compounds, not LM)  
**Coverage:** Unchanged

### 2. Forward-Looking Intensity

**Measurement:** Same as before  
**Change:** Uses project-defined dictionary (not LM), so no change  
**Coverage:** Unchanged (65 terms, project-defined)

**Note:** LM CSV may include optional `strong_modal` / `weak_modal` categories. If present, these can be used for robustness checks.

### 3. Risk Disclosure

**Measurement:** LM Uncertainty + Negative, TF-IDF weighted  
**Change:** ✅ **NOW USES COMPLETE OFFICIAL DICTIONARIES**  
**Coverage Before:** ~175 unique terms (85 uncertainty + 90 negative)  
**Coverage After:** ~2,600 unique terms (300 uncertainty + 2,350 negative)  
**Improvement:** **15× better coverage**

**Impact:**
- Fewer false negatives (missed risk language)
- More robust measurement across diverse disclosure styles
- Better capture of sector-specific risk terminology

### 4. Managerial Tone

**Measurement:** LM Positive vs Negative, net sentiment  
**Change:** ✅ **NOW USES COMPLETE OFFICIAL DICTIONARIES**  
**Coverage Before:** ~150 unique terms (60 positive + 90 negative)  
**Coverage After:** ~2,700 unique terms (350 positive + 2,350 negative)  
**Improvement:** **18× better coverage**

**Impact:**
- More comprehensive sentiment capture
- Reduced measurement error from missed sentiment words
- Better alignment with original LM (2011) methodology

---

## EXPECTED EMPIRICAL CHANGES

### Match Rates (DFM Coverage)

**Before (Subset):**
```
Risk Disclosure:
  - Dictionary: 175 terms
  - Matched in DFM: ~120 terms (69%)
  - Missing: 55 terms not in corpus

Managerial Tone:
  - Dictionary: 150 terms
  - Matched in DFM: ~95 terms (63%)
  - Missing: 55 terms not in corpus
```

**After (Complete):**
```
Risk Disclosure:
  - Dictionary: 2,600 terms
  - Matched in DFM: ~800-1,200 terms (31-46%)
  - Missing: ~1,400-1,800 terms not in financial 10-K corpus
  - BUT: 7-10× more matched terms in absolute numbers

Managerial Tone:
  - Dictionary: 2,700 terms
  - Matched in DFM: ~600-900 terms (22-33%)
  - Missing: ~1,800-2,100 terms not in financial corpus
  - BUT: 6-9× more matched terms in absolute numbers
```

**Key insight:** Lower match *rate* (%) is expected because LM dictionary covers broader financial corpus. But absolute matched terms increase dramatically, improving measurement quality.

### Index Distributions

**Expected changes:**
1. **Means shift slightly** (more complete measurement)
2. **Standard deviations may increase** (capturing more variation)
3. **Correlations stable** (same constructs, just better measured)
4. **Outliers may change** (previously missed extreme cases now captured)

**Not a problem:** These are improvements in measurement quality, not changes in constructs.

### Regression Coefficients

**Expected:**
- Magnitude may change (different scale due to better measurement)
- Sign should remain same (same underlying relationship)
- Statistical significance may improve (less measurement error)

**Action:** Report both versions as robustness check if substantial changes occur.

---

## UPDATED THESIS METHODOLOGY TEXT

### Risk Disclosure (Updated)

**Before:**
> "We measure risk disclosure using a core subset of Loughran-McDonald Uncertainty and Negative dictionaries applied to Item 1A Risk Factors, weighted by TF-IDF..."

**After:**
> "We measure risk disclosure by applying the complete Loughran-McDonald (2011) Uncertainty and Negative dictionaries to Item 1A Risk Factors. We extract all terms flagged in these categories directly from the official LM Master Dictionary CSV (version 2024), ensuring maximum coverage and exact replicability. The combined dictionary comprises approximately 2,600 unique terms. We apply TF-IDF weighting (quanteda::dfm_tfidf) to downweight generic boilerplate and emphasize substantive risk disclosure, following Loughran & McDonald (2011)."

### Managerial Tone (Updated)

**Before:**
> "We calculate managerial tone using core Loughran-McDonald Positive and Negative sentiment dictionaries..."

**After:**
> "We calculate managerial tone using the complete Loughran-McDonald (2011) Positive and Negative sentiment dictionaries, extracted directly from the official LM Master Dictionary CSV (version 2024). The dictionaries comprise approximately 350 positive terms and 2,350 negative terms. We compute ToneLM = (Positive - Negative) / (Positive + Negative) as a net sentiment measure, following Loughran & McDonald (2011)."

### Data Section (Add)

> **Disclosure Measurement Dictionaries.** We employ the complete Loughran-McDonald Master Dictionary (Loughran & McDonald, 2011), obtained from the official CSV file (version 2024, available at https://sraf.nd.edu/loughranmcdonald-master-dictionary/). Unlike generic sentiment dictionaries (e.g., Harvard IV-4), the LM dictionary is specifically validated for financial text and outperforms general-purpose alternatives in predicting firm outcomes (Loughran & McDonald, 2011). We extract all terms flagged in each category (Positive, Negative, Uncertainty, Litigious) directly from the CSV, ensuring complete coverage and exact replicability."

---

## CONFIGURATION FILE STRUCTURE

### How It Works

**Step 1: Run config script**
```r
source("config/create_lm_dictionaries.R")
# Reads: /mnt/data/Loughran-McDonald_MasterDictionary_1993-2024.csv
# Writes: config/analysis_config.rds
```

**Step 2: Module 3 loads config**
```r
config <- readRDS("config/analysis_config.rds")

# Access official LM dictionaries
lm_positive <- config$lm_positive     # Complete (350 terms)
lm_negative <- config$lm_negative     # Complete (2,350 terms)
lm_uncertainty <- config$lm_uncertainty  # Complete (300 terms)
lm_litigious <- config$lm_litigious   # Complete (900 terms)

# Access optional LM categories (if present in CSV)
if (!is.null(config$lm_optional$strong_modal)) {
  strong_modals <- config$lm_optional$strong_modal
}

# Access project-specific lists
forward_looking <- config$forward_looking_extended  # 65 terms
operational <- config$operational_phrases           # 80 phrases
```

**Result:** Single source of truth, clean separation of LM vs project lists.

---

## VALIDATION CHECKLIST (UPDATED)

### Pre-Run Checks
- [ ] Official LM CSV file present at `/mnt/data/Loughran-McDonald_MasterDictionary_1993-2024.csv`
- [ ] Config script run successfully (`config/analysis_config.rds` exists)
- [ ] Config contains expected dictionary sizes (Positive ~350, Negative ~2,350)

### Post-Run Checks
- [ ] Match rates reported (should be lower % but higher absolute counts)
- [ ] Risk Disclosure: ~800-1,200 terms matched (not ~120)
- [ ] Managerial Tone: ~600-900 terms matched (not ~95)
- [ ] Index distributions reasonable (check report)
- [ ] Correlation matrix stable (vs previous run, if available)

### Comparison Checks (Optional)
If you have results from previous (subset) version:
- [ ] Compare index means (should be similar, ±0.2 SD)
- [ ] Compare correlations (should be within ±0.05)
- [ ] Check regression signs (should match)
- [ ] Report as robustness if substantial changes

---

## ROBUSTNESS CHECKS ENABLED

### 1. Subset vs Complete Comparison

If concerned about changes:

```r
# Create subset version (for comparison)
lm_positive_subset <- lm_positive[1:60]  # First 60 terms
lm_negative_subset <- lm_negative[1:90]  # First 90 terms

# Compute indices both ways
# Report: "Results robust to dictionary choice (subset vs complete)"
```

### 2. LM Optional Categories

If CSV includes `strong_modal` / `weak_modal`:

```r
# Compare project modal list vs LM official
forward_looking_lm <- config$lm_optional$strong_modal
# Compute Forward-Looking index both ways
# Report: "Results consistent across modal verb definitions"
```

### 3. Alternative Weighting (Risk)

```r
# Risk disclosure with different weights
risk_uncertainty_only <- lm_uncertainty  # Exclude negative
risk_negative_only <- lm_negative        # Exclude uncertainty
# Compute separately
# Report: "Risk disclosure robust to category composition"
```

---

## TROUBLESHOOTING

### Issue: "LM CSV not found"

**Solution:**
1. Download from: https://sraf.nd.edu/loughranmcdonald-master-dictionary/
2. Place at: `/mnt/data/Loughran-McDonald_MasterDictionary_1993-2024.csv`
3. Update path in `config/create_lm_dictionaries.R` if different location

### Issue: "CSV has unexpectedly few rows"

**Check:**
- Expected: >10,000 rows (full dictionary)
- If <1,000: corrupted file, re-download
- Verify: CSV should have columns "Word", "Positive", "Negative", etc.

### Issue: "Match rates very low (<20%)"

**Cause:** Many LM terms not relevant to 10-K corpus (expected)  
**Action:** Check absolute matched counts (should be 600-1,200)  
**Not a problem:** Lower % but higher absolute counts improves measurement

### Issue: "Index distributions changed dramatically"

**Check:**
- If means shift >0.5 SD: investigate (may indicate issue)
- If means shift <0.2 SD: normal (better measurement)
- Compare histograms before/after
- **Report as robustness** if presenting both versions

---

## ADVANTAGES SUMMARY

| Dimension | Subset Version | Complete CSV Version | Improvement |
|-----------|---------------|---------------------|-------------|
| **Defensibility** | "Core subset selected..." | "Official LM CSV extracted..." | ⬆️⬆️⬆️ |
| **Coverage (Risk)** | ~175 terms | ~2,600 terms | 15× |
| **Coverage (Tone)** | ~150 terms | ~2,700 terms | 18× |
| **Replicability** | Good (explicit list) | Excellent (CSV download) | ⬆️ |
| **Measurement Error** | Higher (omissions) | Lower (complete) | ⬆️ |
| **False Negatives** | More (missed terms) | Fewer (comprehensive) | ⬆️ |
| **Maintenance** | Manual updates | Automatic (CSV version) | ⬆️ |
| **Transparency** | Subset rationale needed | Self-evident (official) | ⬆️ |

**Verdict:** Complete CSV version superior on all substantive dimensions.

---

## REFERENCES (UPDATED)

**Core Methodological Papers (Unchanged):**

- Loughran, T., & McDonald, B. (2011). When is a liability not a liability? Textual analysis, dictionaries, and 10-Ks. *Journal of Finance*, 66(1), 35-65.
  - **Official dictionary source:** https://sraf.nd.edu/loughranmcdonald-master-dictionary/

- Li, F. (2008). Annual report readability, current earnings, and earnings persistence. *Journal of Accounting and Economics*, 45(2-3), 221-247.

- Muslu, V., et al. (2015). Forward-looking MD&A disclosures and the information environment. *Management Science*, 61(5), 931-948.

- Campbell, J. L., et al. (2014). The information content of mandatory risk factor disclosures in corporate filings. *Review of Accounting Studies*, 19(1), 396-455.

- Kravet, T., & Muslu, V. (2013). Textual risk disclosures and investors' risk perceptions. *Review of Accounting Studies*, 18(4), 1088-1122.

---

## SUMMARY

**What Changed:**
- ✅ Use complete official LM dictionaries (not subset)
- ✅ Clean separation of LM vs project lists
- ✅ 15-18× better coverage for Risk/Tone indices
- ✅ Maximum defensibility for MSc thesis

**What Stayed Same:**
- Index logic and formulas
- Normalization approach
- Component transparency
- All other methodology

**Action Required:**
1. Download official LM CSV
2. Run updated config script
3. Run updated Module 3 script
4. Update thesis Methods section (text provided above)
5. Optional: Compare results to previous version

**Result:** Production-ready, maximally defensible disclosure indices using best-in-class dictionaries.

---

**END OF UPDATED TECHNICAL DOCUMENTATION**
