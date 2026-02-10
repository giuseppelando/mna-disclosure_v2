# M&A Disclosure NLP Pipeline — Quality Assessment Report

**Assessment Date:** 2025-02-09  
**Pipeline Status:** ✅ **PRODUCTION READY**

---

## Executive Summary

The full NLP pipeline has been executed successfully. All six modules completed without errors, producing a high-quality, regression-ready dataset. The pipeline processed **2,391 M&A deals** with matched 10-K filings and constructed **four disclosure indices** that are methodologically sound and ready for econometric analysis.

---

## Pipeline Execution Summary

| Module | Status | Key Outputs |
|--------|--------|-------------|
| Module 1: Text Cleaning | ✅ Complete | 2,391 cleaned documents |
| Module 2: Tokenization | ✅ Complete | DFMs, tokens, numeric features |
| Module 3: Index Construction | ✅ Complete | 4 disclosure indices |
| Module 4: KWIC Validation | ✅ Complete | kwic_samples.xlsx |
| Module 5: Descriptive Analysis | ✅ Complete | Statistics, figures, reports |
| Module 6: Final Assembly | ✅ Complete | analysis_dataset.rds (95 cols) |

---

## Data Quality Assessment

### Sample Sizes

| Metric | Count | Notes |
|--------|-------|-------|
| Total deals | 2,391 | All with matched 10-K |
| MD&A empty after cleaning | 0 | 0% loss |
| Risk Factors empty | 0 | 0% loss |
| sample_core (all 3 indices) | 2,391 | 100% |
| sample_tone | 2,384 | 99.7% |
| sample_premium | 2,374 | 99.3% |
| sample_completion | 2,391 | 100% |
| sample_duration | 2,388 | 99.9% |

### Index Completeness

| Index | N | Missing | Missing % |
|-------|---|---------|-----------|
| operational_specificity_norm | 2,391 | 0 | 0.00% |
| forward_looking_norm | 2,391 | 0 | 0.00% |
| risk_disclosure_tfidf_norm | 2,391 | 0 | 0.00% |
| tone_lm_norm | 2,384 | 7 | 0.29% |

**Note:** The 7 missing tone values are due to the sparse-denominator rule (pos + neg < 10), which is methodologically correct behavior.

### Outliers (|z| > 5 SD)

| Index | Outliers |
|-------|----------|
| operational_specificity_norm | 1 |
| forward_looking_norm | 0 |
| risk_disclosure_tfidf_norm | 0 |
| tone_lm_norm | 0 |

**Conclusion:** Minimal outliers; no action required. Flags available for sensitivity analysis.

---

## Construct Validity Assessment

### 1. Correlation Structure

```
                          op_spec    fwd_look   risk_disc   tone
operational_specificity     1.00      -0.37       0.03     -0.08
forward_looking            -0.37       1.00      -0.03      0.12
risk_disclosure             0.03      -0.03       1.00     -0.14
tone                       -0.08       0.12      -0.14      1.00
```

**Assessment:**
- ✅ No high correlations (|r| ≥ 0.70) — no multicollinearity concerns
- ✅ Moderate negative correlation between operational specificity and forward-looking (-0.37) is theoretically sensible (specific firms focus on details, less forward-looking language)
- ✅ Negative correlation between risk disclosure and tone (-0.14) aligns with theory

### 2. Known-Groups Validity (Industry ANOVA)

| Index | F-statistic | p-value | Interpretation |
|-------|-------------|---------|----------------|
| operational_specificity_raw | 21.53 | <0.001 | Strong industry differentiation |
| forward_looking_density | 8.12 | <0.001 | Significant industry effects |
| risk_disclosure_tfidf | 8.53 | <0.001 | Significant industry effects |
| tone_lm | 8.97 | <0.001 | Significant industry effects |

**Assessment:** ✅ All indices show highly significant between-industry variation (all p < 0.001), confirming the indices capture meaningful sector-level differences in disclosure practices.

### 3. Forward-Looking Construct Channels

- Modal tokens active: 37/39 (95%)
- Regex patterns active: 12/12 (100%)
- Total matches: 222,463 (157,786 modal + 64,677 regex)
- Contribution: Modal 70.9% | Regex 29.1%

**Assessment:** ✅ Both channels contribute meaningfully; construct is well-specified.

### 4. Risk Dictionary Coverage

- Raw LM dictionary: 2,642 terms
- Single-token after preprocessing: 2,602 terms
- Matched in DFM: 1,882/2,602 (72.3%)
- Unmatched: 720 terms (mostly rare inflections)

**Assessment:** ✅ Good dictionary coverage; unmatched terms are rare variants that don't affect measurement.

---

## Normalization Assessment

### Hierarchical Fallback Distribution

| Level | Count | Share |
|-------|-------|-------|
| sic2×year | 711 | 29.7% |
| sic2 | 1,456 | 60.9% |
| year | 224 | 9.4% |
| global | 0 | 0% |

**Assessment:** ⚠️ Only ~30% at intended sic2×year level due to sparse cells (min_cell_size=10). This is a documented methodological choice:
- The hierarchical fallback ensures robust z-scores
- Industry-level normalization (60.9%) still captures sector variation
- Should be noted in thesis methodology

---

## Outcome Variables

### Premium Distribution

- N: 2,374
- Mean: 119.92% (inflated by outliers)
- Median: 28.60%
- SD: 3,833.73

**Note:** High SD indicates extreme outliers in premium. Consider winsorizing at 1st/99th percentile for regressions.

### Completion

- Completed: 2,391 (100%)
- Withdrawn: 0 (0%)

**⚠️ Important:** The sample contains **no withdrawn deals**. This means:
- The `completion` outcome variable has no variation
- Logit/probit models for completion probability are **not feasible** with this sample
- Recommend obtaining withdrawn deals from SDC if completion analysis is required

### Time to Close

- N: 2,391 completed deals
- Mean: 128.3 days
- Median: 104.0 days

**Assessment:** ✅ Duration analysis (Cox PH) is feasible with this data.

---

## File Outputs

### Data Files
| File | Size | Status |
|------|------|--------|
| analysis_dataset.rds | 85 MB | ✅ Full dataset with text |
| analysis_dataset.csv | 2 MB | ✅ No text columns |

### Documentation
- ✅ data_dictionary_final.csv (95 columns documented)
- ✅ 06_final_dataset_report.md

### Figures (14 total)
- ✅ Histograms for all indices (6)
- ✅ Q-Q plots for all indices (6)
- ✅ Correlation heatmap (1)
- ✅ Time trends (1)

---

## Recommendations for Econometric Analysis

### Ready to Proceed

1. **Premium regressions (OLS)**
   - Sample: 2,374 deals
   - Winsorize premium at 1st/99th percentile
   - Use all four disclosure indices
   - Include industry×year FE (`ind_year_fe`)

2. **Duration analysis (Cox PH)**
   - Sample: 2,388 deals (completed with valid time_to_close)
   - Event = completion; time = time_to_close
   - Use disclosure indices as covariates

### Not Feasible with Current Sample

3. **Completion probability (Logit/Probit)**
   - ❌ Cannot estimate: no withdrawn deals in sample
   - **Recommendation:** Obtain withdrawn deals from SDC Platinum

### Suggested Model Specifications

```r
# Premium regression (OLS with FE)
lm(premium_1d ~ operational_specificity_norm + forward_looking_norm + 
               risk_disclosure_tfidf_norm + tone_lm_norm + 
               [controls] | ind_year_fe, data = df)

# Duration analysis (Cox PH)
coxph(Surv(time_to_close, completion) ~ operational_specificity_norm + 
      forward_looking_norm + risk_disclosure_tfidf_norm + tone_lm_norm + 
      [controls] + strata(sic2), data = df)
```

### Robustness Checks to Consider

1. Alternative normalization threshold (min_cell_size = 5 or 15)
2. Premium winsorization sensitivity (1%/99% vs 5%/95%)
3. Excluding outlier observations (use `*_outlier` flags)
4. Alternative premium measures (premium_1w, premium_4w)
5. Industry subset analysis (high-disclosure vs low-disclosure sectors)

---

## Conclusion

The M&A Disclosure NLP Pipeline has produced a **high-quality, regression-ready dataset**. All four disclosure indices are methodologically sound, with good construct validity evidenced by:

- No multicollinearity issues
- Significant industry differentiation
- Complete coverage (>99% non-missing)
- Proper normalization with documented fallback

**Primary limitation:** The sample contains only completed deals, precluding completion probability analysis. For a complete thesis, consider extending the sample to include withdrawn deals.

**The dataset is ready for econometric modeling of deal premiums and duration.**
