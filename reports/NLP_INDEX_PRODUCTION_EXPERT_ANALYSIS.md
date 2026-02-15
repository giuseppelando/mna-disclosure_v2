# Expert Analysis: NLP Index Production Pipeline
## M&A Disclosure Quality Project

**Analyst Role:** Senior Research Engineer in Empirical Corporate Finance  
**Analysis Date:** 2026-02-15  
**Scope:** Complete review of text-to-index pipeline for academic research purposes

---

## Executive Summary

This analysis evaluates the NLP pipeline that transforms raw 10-K text (MD&A and Risk Factors) into four disclosure quality indices used in M&A premium and completion regressions.

### Overall Assessment: **B+ (Good with Notable Strengths and Some Concerns)**

| Dimension | Grade | Assessment |
|-----------|-------|------------|
| **Methodological Soundness** | A- | Follows L&M (2011) standards; theory-driven constructs |
| **Code Quality & Hygiene** | A | Modular, documented, reproducible, idempotent |
| **Dictionary Implementation** | B+ | Correct L&M usage; some gaps in coverage |
| **Normalization Approach** | B | Defensible but fallback rate is high |
| **Validation Rigor** | B+ | KWIC implemented; could be more systematic |
| **Research Defensibility** | B+ | MSc-appropriate; limitations acknowledged |

### Key Findings

**Strengths:**
1. Clean separation of concerns across 6 modules
2. Correct implementation of Loughran-McDonald dictionaries
3. Appropriate preservation of financial patterns during cleaning
4. Transparent normalization with audited fallback levels
5. Comprehensive logging and documentation

**Concerns:**
1. Only 44% of observations achieve ideal sic2×year normalization
2. Forward-looking construct mixes modal tokens with regex patterns (methodological ambiguity)
3. Operational specificity weights (0.5/0.5) are arbitrary
4. 606/2602 (23%) of risk dictionary terms never matched in corpus
5. No systematic false positive rate estimation from KWIC

---

## Detailed Module-by-Module Analysis

### MODULE 1: Text Cleaning

**File:** `01_text_cleaning.R` (v2)  
**Purpose:** EDGAR-aware cleaning preserving financial information

#### What It Does Well ✅

1. **EDGAR-Specific Handling:**
   ```r
   # Removes HTML entities, XBRL tags, table artifacts
   # But preserves financial patterns (numbers, %, $)
   ```
   - Correct approach for 10-K filings
   - Entity decoding handles common EDGAR artifacts (&#160;, &amp;, etc.)

2. **Conservative Boilerplate Removal:**
   - Max span reduced from 2000 to 1200 chars (documented decision)
   - Targets only highly standardized disclaimers
   - Preserves substantive forward-looking discussion

3. **Financial Pattern Preservation:**
   - Validation shows 100% of sample retains numbers
   - 98% retain percentages
   - 100% retain currency symbols
   - This is critical for operational specificity index

4. **Vectorized Operations:**
   - Uses `str_count()` instead of slow `sapply()` loops
   - Division-by-zero guards implemented

#### Concerns ⚠️

1. **No Reduction Ratio Reporting:**
   - The report mentions checking "median reduction ratio in [0.70, 0.95]"
   - But actual reduction statistics not shown in the report
   - Cannot verify cleaning wasn't too aggressive

2. **Lowercase Conversion Timing:**
   - Applied AFTER entity decoding (correct)
   - But BEFORE boilerplate removal patterns that use lowercase
   - Could miss some patterns if not careful

#### Recommendation:
- Add explicit reduction ratio statistics to cleaning report
- Verify 3-5 documents manually with before/after comparison

---

### MODULE 2: Tokenization and DTM Construction

**File:** `02_tokenization_dtm.R` (v3)  
**Purpose:** Create token streams and document-feature matrices

#### What It Does Well ✅

1. **Dual-Track Stopword Strategy:**
   ```r
   # Track A (dictionaries): Keep modals/negations
   keep_words_dict <- c("will", "shall", "may", "might", "could", "would", "should",
                        "not", "no", "nor", "neither", "never", "none", ...)
   
   # Track B (bigrams): Minimal removal for cleaner collocations
   ```
   - This is exactly right for sentiment + forward-looking analysis
   - Aligns with L&M (2011) methodology

2. **Finance Word Retention Check:**
   ```
   revenue: OK, margin: OK, debt: OK, risk: OK
   will: OK, may: OK, not: OK, expect: OK
   ```
   - Critical validation that dictionaries can match

3. **Operational Phrase Compounding:**
   - 35 predefined operational bigrams
   - Converted to compounds: "operating_income", "cash_flow", etc.
   - Enables bigram-based specificity measurement

4. **TF-IDF for Risk Disclosure:**
   - Correct implementation: `scheme_tf = "prop", scheme_df = "inverse"`
   - Weights rare substantive risk terms over boilerplate

#### Concerns ⚠️

1. **"capital" Missing from MD&A Unigram DFM:**
   ```
   capital: FALSE (MD&A), TRUE (Risk)
   ```
   - This is concerning - "capital" is high-frequency in MD&A
   - May have been trimmed by `max_docfreq = 0.95` rule
   - The protected-terms patch doesn't include "capital"

2. **High Sparsity:**
   - MD&A bigrams: 99.35% sparse (695,148 features!)
   - This is extremely sparse; most bigrams occur in <1% of docs
   - May affect bigram-based operational specificity reliability

3. **Quarter Regex:**
   - Pattern: `"q[1-4]\\s?\\d{4}"` matches "q1 2022" or "q12022"
   - But misses "Q1 FY2022", "first quarter 2022", "1Q22"
   - Undercount of temporal references

#### Recommendations:
- Add "capital", "expenditure", "margin" to protected terms
- Consider more lenient docfreq trimming for operational terms
- Expand quarter regex patterns

---

### MODULE 3: Index Construction

**File:** `03_construct_indices.R` (v5)  
**Purpose:** Construct four theory-driven disclosure indices

This is the core methodological module. I'll analyze each index separately.

#### Index 1: Operational Specificity

**Formula:**
```r
operational_specificity_raw = 0.5 × numeric_density + 0.5 × compound_density
```

**Assessment:**

| Aspect | Evaluation |
|--------|------------|
| Theoretical Grounding | **B+** — Captures quantitative detail; proxy for information richness |
| Implementation | **B** — Correct but arbitrary weights |
| Validation | **B-** — Limited KWIC (only 3 compound patterns tested) |

**Issues:**

1. **Arbitrary Weights:**
   - The 0.5/0.5 split is not theoretically motivated
   - Action plan suggested 0.6/0.4 (also arbitrary)
   - Sensitivity analysis should vary weights

2. **Numeric Density Components:**
   ```r
   patterns_numeric <- c(
     "\\d+\\.\\d+",      # Decimals
     "\\d+%",            # Percentages
     "\\$\\d+",          # Currency
     "\\d+\\s?(million|billion|thousand)",
     "Q[1-4]\\s?\\d{4}"  # Quarters
   )
   ```
   - Missing: Ratios (2:1), ranges (10-15%), fiscal years (FY2022)
   - These omissions could undercount specificity

3. **Compound Density:**
   - Only 35 operational phrases defined
   - KWIC shows very low match rates for some:
     - `capital_expenditure`: 1-2 matches in top/bottom deciles
     - `operating_cash_flow`: 0-2 matches
   - May be too restrictive

**Mean/SD:** 62.8 / 20.0 (reasonable distribution)

---

#### Index 2: Forward-Looking Intensity

**Formula:**
```r
forward_looking_density = (modal_matches + regex_matches) / word_count × 1000
```

**Assessment:**

| Aspect | Evaluation |
|--------|------------|
| Theoretical Grounding | **A-** — Well-established in literature (Muslu et al. 2015) |
| Implementation | **B+** — Dual-channel approach is sophisticated |
| Validation | **A-** — Good KWIC coverage |

**What Works Well:**

1. **Dual-Channel Detection:**
   - Modal channel (34 tokens): will, would, shall, may, might, expect, anticipate, forecast, plan, intend, etc.
   - Regex channel (12 patterns): "going forward", "in the future", "expects to", "plans to", etc.
   
2. **Contribution Tracking:**
   ```
   Total modal matches: 281,282 (70.5%)
   Total regex matches: 117,596 (29.5%)
   ```
   - Shows modals dominate (expected)
   - Regex captures additional multi-word constructions

3. **KWIC Validation:**
   - "expect*": 50 matches each in top/bottom deciles
   - "anticipat*": 17 (bottom) vs 45 (top) — good discrimination
   - "forecast*": 4 (bottom) vs 21 (top) — strong discrimination

**Issues:**

1. **Methodological Ambiguity:**
   - Mixing single-token DFM matching with regex text matching
   - Could lead to different preprocessing paths
   - Should document this hybrid approach explicitly

2. **"future" Not in Active Tokens:**
   - Config has "future" in modal list
   - But report shows "forthcoming, upcoming, next" — not "future"
   - May have been trimmed as too common (appears in >95% of docs?)

**Mean/SD:** 8.47 / 2.88 per 1000 words (well-behaved)

---

#### Index 3: Risk Disclosure (TF-IDF)

**Formula:**
```r
risk_disclosure_tfidf = TF-IDF mass of (LM_uncertainty + LM_negative) / total TF-IDF mass
```

**Assessment:**

| Aspect | Evaluation |
|--------|------------|
| Theoretical Grounding | **A** — Direct application of L&M (2011) |
| Implementation | **A-** — TF-IDF weighting is appropriate |
| Validation | **B+** — Dictionary transparency is excellent |

**What Works Well:**

1. **Dictionary Transparency:**
   ```
   Raw sizes: Uncertainty=297 | Negative=2345 | Combined=2642
   Final usable: 2602 (single-token after preprocessing)
   Matched in DFM: 1996/2602 (76.7%)
   ```
   - Excellent audit trail

2. **TF-IDF Weighting:**
   - Correctly downweights boilerplate risk terms
   - Upweights rare, substantive risk disclosures

3. **KWIC Coverage:**
   - "risk/risks": 50 matches each in top/bottom
   - "uncertain*": 50 (bottom) vs 36 (top) — interesting reversal
   - "adverse*": 50/50 — good coverage

**Issues:**

1. **23% Dictionary Terms Unmatched:**
   ```
   Unmatched: 606/2602
   Examples: abandons, abdicated, abdicates, ...
   ```
   - Many are rare word forms (verb conjugations)
   - This is acceptable — DFM only keeps terms in ≥2 docs
   - But reduces sensitivity to rare risk language

2. **Uncertainty Reversal in KWIC:**
   - "uncertain*": 50 matches in LOW risk disclosure, 36 in HIGH
   - This seems backwards — investigate!
   - Possible explanation: TF-IDF normalizes differently?

**Mean/SD:** 0.086 / 0.024 (narrow distribution — may lack variance)

---

#### Index 4: Managerial Tone (LM Sentiment)

**Formula:**
```r
tone_lm = (pos_counts - neg_counts) / (pos_counts + neg_counts)
         if (pos + neg) >= 10, else NA
```

**Assessment:**

| Aspect | Evaluation |
|--------|------------|
| Theoretical Grounding | **A** — Standard L&M tone formula |
| Implementation | **A-** — Sparse-denominator guard is appropriate |
| Validation | **B** — KWIC includes negation context (good) |

**What Works Well:**

1. **Sparse Denominator Guard:**
   - Requires pos + neg ≥ 10 words
   - Prevents extreme ratios from near-zero denominators
   - Only 11 documents fail this check (0.26%)

2. **Negation-Context KWIC:**
   - Includes "not", "no", "never" patterns
   - Allows manual checking for negation effects

3. **Distribution:**
   - Mean tone: -0.33 (negative skew, expected for 10-Ks)
   - SD: 0.23 (reasonable variance)

**Issues:**

1. **Simple Bag-of-Words Limitation:**
   - Cannot handle negation: "not profitable" counted as positive
   - Cannot handle context: "risk of strong competition" 
   - This is acknowledged limitation, not a bug

2. **KWIC Shows Potential Issues:**
   - "declin*": 47 (bottom) vs 32 (top) 
   - Decline words more common in LOW tone docs — correct
   - "improv*": 10 (bottom) vs 50 (top) — strong discrimination

---

### Normalization Analysis

**Method:** Hierarchical z-scoring with fallback levels

```
Level 1 (preferred): sic2 × year (min cell size = 10)
Level 2 (fallback):  sic2 only
Level 3 (fallback):  year only
Level 4 (fallback):  global
```

**Actual Fallback Distribution:**

| Level | N | Share |
|-------|---|-------|
| sic2_year | 1,849 | 44.2% |
| sic2 | 2,171 | 51.9% |
| year | 166 | 4.0% |
| global | 0 | 0.0% |
| missing (tone only) | 11 | 0.3% |

**Assessment:**

**Issues:**

1. **Only 44% Achieve Ideal Normalization:**
   - Ideal is sic2×year for comparability within industry-year cohort
   - 51.9% fall back to sic2-only
   - This means many observations compared to different years

2. **min_cell_size = 10 May Be Too Strict:**
   - Many sic2×year cells have 5-9 observations
   - Could use min_cell_size = 5 or 8 as robustness check

3. **No global Fallback Used:**
   - Good sign — all observations have at least year or industry reference
   - But 4% use year-only (loses industry comparability)

**Recommendation:**
- Report sensitivity to min_cell_size = {5, 8, 10, 15}
- Consider industry (sic2) fixed effects in regressions to absorb remaining variation

---

### Cross-Index Correlations

**Correlation Matrix (Normalized Indices, N=4,186):**

| | op_spec | fwd_look | risk_tfidf | tone |
|--|---------|----------|------------|------|
| op_spec | 1.00 | -0.38 | 0.03 | -0.09 |
| fwd_look | | 1.00 | -0.01 | 0.14 |
| risk_tfidf | | | 1.00 | -0.12 |
| tone | | | | 1.00 |

**Assessment:**

1. **No Multicollinearity Concern:**
   - All |r| < 0.40
   - Safe to include all indices in same regression

2. **Interesting Negative Correlation (op_spec vs fwd_look):**
   - r = -0.38 is substantively meaningful
   - Firms with more specific disclosure use less forward-looking language
   - Possible interpretation: Specificity focuses on facts, not projections
   - Or: Different managerial disclosure styles

3. **Risk Disclosure Nearly Orthogonal:**
   - r ≈ 0 with other indices
   - Captures distinct dimension (good for construct validity)

4. **Tone Weakly Related:**
   - Positive with forward-looking (+0.14): Optimistic firms project forward
   - Negative with risk (-0.12): More risk disclosure → more negative tone

---

### Industry Discrimination (ANOVA)

**Raw Indices by SIC2 Industry:**

| Index | F-statistic | p-value | Interpretation |
|-------|-------------|---------|----------------|
| operational_specificity_raw | 23.75 | <10⁻²²³ | **Excellent** discrimination |
| forward_looking_density | 10.99 | <10⁻⁹⁵ | **Strong** discrimination |
| risk_disclosure_tfidf | 10.32 | <10⁻⁸⁸ | **Strong** discrimination |
| tone_lm | 10.55 | <10⁻⁹¹ | **Strong** discrimination |

**Assessment:**
- All indices show highly significant between-industry variation
- This validates the constructs — they capture real industry differences
- Also justifies industry×year normalization approach

---

## Code Quality Assessment

### Strengths

1. **Modular Architecture:**
   - Clean separation: cleaning → tokenization → indices → validation → assembly
   - Each module is independently testable
   - Clear inputs/outputs documented

2. **Idempotent Design:**
   - Re-running produces identical outputs
   - Explicit seeds for reproducibility (seed = 2025 for KWIC)

3. **Error Handling:**
   - Division-by-zero guards throughout
   - Missing column checks with informative errors
   - `stop_if_missing()` helper function

4. **Audit Logging:**
   - Normalization fallback levels tracked per observation
   - Dictionary match rates documented
   - KWIC row counts by construct/pattern

5. **Configuration Externalized:**
   - `analysis_config.rds` stores dictionaries and parameters
   - Easy to modify without changing code

### Minor Issues

1. **Magic Numbers:**
   - `min_cell_size = 10` embedded in code
   - Could be in config for easier sensitivity analysis

2. **Hardcoded Paths:**
   - Some paths use absolute Windows paths
   - Should use relative paths throughout

3. **Limited Unit Tests:**
   - No formal test suite
   - Relies on manual validation reports

---

## Research Defensibility Assessment

### For MSc Thesis: **Highly Defensible**

The pipeline is appropriate for an MSc-level thesis because:

1. **Follows Established Literature:**
   - Loughran-McDonald dictionaries (standard)
   - Industry×year normalization (standard)
   - TF-IDF weighting for risk (appropriate)

2. **Transparent Methodology:**
   - All choices documented
   - Limitations acknowledged
   - KWIC validation included

3. **Within Methodological Boundary:**
   - No black-box models (transformers, embeddings)
   - Explainable to non-technical reviewers

### Potential Reviewer Concerns:

1. **"Why not use more sophisticated NLP?"**
   - Response: Methodological boundary for MSc thesis
   - Dictionary methods more transparent and auditable
   - Consistent with L&M (2011) approach

2. **"How do you handle negation?"**
   - Response: Acknowledged limitation
   - KWIC validation included negation contexts
   - Tone formula uses net (pos-neg), not separate counts

3. **"What about within-document variation?"**
   - Response: Document-level indices appropriate for deal-level outcomes
   - Section-specific extraction (MD&A vs Risk Factors) addresses this

---

## Summary of Recommendations

### High Priority (Address Before Publication)

1. **Add "capital" to Protected Terms:**
   - Currently missing from MD&A unigram DFM
   - Critical for operational specificity

2. **Investigate Uncertainty KWIC Reversal:**
   - Why do LOW risk-disclosure docs have MORE "uncertain*" matches?
   - May indicate TF-IDF doing unexpected normalization

3. **Report Cleaning Reduction Ratios:**
   - Current report missing actual statistics
   - Add median, p10, p90 of word count reduction

### Medium Priority (Robustness)

4. **Sensitivity Analysis for Normalization:**
   - Vary min_cell_size = {5, 8, 10, 15}
   - Report coefficient stability

5. **Sensitivity Analysis for Specificity Weights:**
   - Vary numeric/compound weights: 0.4/0.6, 0.5/0.5, 0.6/0.4
   - Or use PCA to derive weights empirically

6. **Expand Numeric Patterns:**
   - Add ratios, ranges, fiscal years
   - Could increase specificity index variance

### Low Priority (Nice to Have)

7. **Formal KWIC False Positive Estimation:**
   - Sample 50 random matches per pattern
   - Calculate explicit false positive rate

8. **Add Unit Tests:**
   - Test helper functions with known inputs
   - Regression tests for index calculations

---

## Final Verdict

**The NLP index production pipeline is methodologically sound and appropriate for academic research.** The implementation follows best practices from the computational linguistics literature (L&M 2011), maintains transparency through extensive logging, and produces indices with reasonable statistical properties.

**Key strength:** The pipeline is highly reproducible and well-documented.

**Key weakness:** The normalization fallback rate (56% not achieving ideal sic2×year) is higher than desirable, but this is a data limitation rather than a methodological flaw.

**Bottom line:** The indices are suitable for use in premium and completion regressions. The main empirical finding (negative association between operational specificity and premium) is supported by a methodologically defensible measurement approach.

---

## Appendix: Quick Reference

### Files Reviewed:
- `src/30_nlp/01_text_cleaning.R` (v2)
- `src/30_nlp/02_tokenization_dtm.R` (v3)
- `src/30_nlp/03_construct_indices.R` (v5)
- `src/30_nlp/04_kwic_validation.R` (v2)
- `src/30_nlp/05_descriptive_analysis.R` (v2)
- `src/30_nlp/06_assemble_analysis_dataset_v2.R` (v6)
- `src/30_nlp/create_lm_dictionaries.R`
- `config/analysis_config.rds`
- `config/lm/LM_MasterDictionary.csv`

### Reports Reviewed:
- `reports/01_cleaning_report.md`
- `reports/02_tokenization_report.md`
- `reports/03_index_construction_report.md`
- `reports/04_kwic_validation.md`
- `reports/05_descriptive_analysis.md`
- `reports/06_final_dataset_report.md`

### Data Artifacts Checked:
- `output/tables/summary_stats.csv`
- `output/tables/correlation_matrix.csv`
- `output/tables/fallback_usage_by_index.csv`
- `output/tables/anova_industry_by_index.csv`
- `output/tables/high_correlation_pairs_ge_0p70.csv`
