# NLP Pipeline Enhancement: Implementation Summary

**Date:** 2026-02-15  
**Status:** Ready for execution

---

## What Was Implemented

Based on careful analysis of risks and benefits, I implemented **targeted enhancements** that maximize value while minimizing risk of breaking the existing pipeline.

### New Files Created

| File | Purpose |
|------|---------|
| `src/30_nlp/07_nlp_enhancement.R` | Enhancement module - adds new indices |
| `src/50_models/10_run_enhanced_models.R` | Extended econometric models |

### Design Philosophy

1. **Additive, not disruptive** - Enhancement module runs AFTER existing pipeline
2. **No modifications to working code** - Original indices unchanged
3. **Multiple normalization tracks** - Enables robustness checks
4. **Graceful fallback** - Models work with or without new indices

---

## New Indices Added

### 1. Reading Difficulty (Combined)
```
reading_difficulty = (Fog Index + FK Grade) / 2
```
- **Gunning Fog Index:** 0.4 × (words/sentence + 100 × complex_words/words)
- **Flesch-Kincaid Grade:** 0.39 × (words/sentence) + 11.8 × (syllables/words) - 15.59
- **Normalization:** Hierarchical sic2×year

**Expected effect on premium:** Positive (harder to read → higher premium due to information processing costs)

### 2. Modal Certainty
```
modal_certainty = strong_modals / (strong_modals + weak_modals)
```
- **Strong modals:** will, shall, must
- **Weak modals:** may, might, could, would, should
- **Guard:** Requires ≥5 modal verbs for validity

**Expected effect on premium:** Negative (more certain language → lower premium due to reduced uncertainty)

### 3. Comparative Intensity
```
comparative_density = comparative_words / word_count × 1000
```
- **Words counted:** increase, decrease, grow, decline, rise, fall, improve, deteriorate, higher, lower
- **Purpose:** Captures focus on change and performance trajectory

**Expected effect on premium:** Ambiguous (could signal transparency OR volatility)

---

## Normalization Tracks

Each new index is available in multiple forms:

| Track | Suffix | Description | Use Case |
|-------|--------|-------------|----------|
| Raw | (none) | Original values | Coefficients in natural units |
| Global z-score | `_global_z` | (x - μ) / σ | Simple standardization |
| Hierarchical | `_norm` | sic2×year → sic2 → year → global | Primary specification |
| Percentile | `_pctl` | Rank / N | Robust to outliers |

---

## Model Specifications Added

### Premium Models (6 specifications)

| Model | Description | Purpose |
|-------|-------------|---------|
| M1 | Core indices + Year FE | Baseline |
| M2 | Core indices + Industry×Year FE | Granular FE |
| M3 | Core + New indices | Test new measures |
| M4 | Specificity × Tender interaction | Heterogeneous effects |
| M5 | Specificity × Readability interaction | Moderation test |
| M6 | Specificity + Specificity² | Nonlinear effects |

### Completion Models (2 specifications)

| Model | Description |
|-------|-------------|
| M1 | Logit with core indices |
| M2 | Logit with core + new indices |

---

## How to Run

### Step 1: Run the Enhancement Module
```r
setwd("C:/Users/giuse/Documents/GitHub/mna-disclosure")
source("src/30_nlp/07_nlp_enhancement.R")
```

**Output:**
- `data/interim/enhanced_features.rds` - New features only
- `data/interim/indices_enhanced.rds` - Original + new indices
- `reports/enhancement_report.md` - Summary statistics

### Step 2: Run Enhanced Models
```r
source("src/50_models/10_run_enhanced_models.R")
```

**Output:**
- `output/tables/enhanced_premium_results.csv`
- `output/tables/enhanced_completion_results.csv`
- `output/tables/model_comparison.csv`
- `reports/enhanced_models_report.md`

---

## Protected Terms List (Fixed Issue)

The enhancement module includes an expanded protected terms list that addresses the "capital missing from DFM" issue:

```r
protected_terms <- c(
  # Modals (original)
  "will", "shall", "may", "might", "could", "would", "should", "must",
  
  # Negations (original)
  "not", "no", "nor", "neither", "never", "none", "cannot",
  
  # Financial terms (NEW - fixes "capital" issue)
  "capital", "expenditure", "margin", "revenue", "income", "profit",
  "debt", "equity", "asset", "liability", "cash", "flow", "earnings",
  
  # Comparatives (NEW)
  "increase", "decrease", "grow", "growth", "decline", "rise", "fall",
  "improve", "deteriorate", "higher", "lower", "more", "less",
  
  # Hedging (NEW)
  "approximately", "about", "around", "roughly", "nearly",
  "possibly", "probably", "likely", "unlikely", "uncertain"
)
```

---

## Expected Benefits

### Measurement Quality
- **New dimension:** Readability (completely absent before)
- **Finer sentiment:** Modal certainty separates confidence from hedging
- **Change focus:** Comparative intensity captures performance trajectory

### Econometric Power
- **Interaction tests:** Can now test whether effects vary by deal type or readability
- **Nonlinear tests:** Can detect U-shaped or threshold effects
- **Robustness:** Multiple normalization tracks for sensitivity analysis

### Research Contributions
1. First to test whether disclosure **readability** moderates **specificity** effects
2. Novel **modal certainty** measure distinguishes confidence from hedging
3. **Comparative intensity** captures management's focus on change

---

## What Was NOT Implemented (and Why)

| Proposal | Reason for Exclusion |
|----------|---------------------|
| 12-index framework | Too many indices for N≈2,600; multicollinearity risk |
| PCA composites | Harder to interpret; adds complexity without clear benefit |
| Temporal specificity as separate index | Partially captured by existing specificity |
| Uncertainty emphasis as separate index | Already embedded in risk_disclosure_tfidf |
| Full pipeline rewrite | Risk of breaking working code; marginal benefit |

---

## Validation Checklist

After running the enhancement module, verify:

- [ ] `indices_enhanced.rds` has more columns than original
- [ ] `reading_difficulty_norm` has reasonable distribution (mean ≈ 0, sd ≈ 1)
- [ ] `modal_certainty_raw` is between 0 and 1
- [ ] Protected terms check shows "capital" present in DFM
- [ ] Correlation matrix shows new indices not perfectly correlated with existing

---

## Files Modified

| File | Change |
|------|--------|
| None | **No existing files were modified** |

All changes are additive. The original pipeline remains unchanged.
