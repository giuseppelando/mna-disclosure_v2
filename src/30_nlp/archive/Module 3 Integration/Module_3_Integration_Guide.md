# MODULE 3 INTEGRATION GUIDE (UPDATED)

**Date:** 2025-02-04 (Updated with Official LM CSV Integration)  
**Purpose:** Connect Module 3 (Index Construction) to complete M&A Disclosure pipeline  
**Major Update:** Now uses complete official LM dictionaries from CSV

---

## UPDATED WORKFLOW: Configuration First

### NEW PREREQUISITE STEP

**Before running Module 3, you must:**

1. **Download Official LM Dictionary CSV**
   - Source: https://sraf.nd.edu/loughranmcdonald-master-dictionary/
   - File: `Loughran-McDonald_MasterDictionary_1993-2024.csv`
   - Place at: `/mnt/data/Loughran-McDonald_MasterDictionary_1993-2024.csv`
   - Size: ~50 MB, ~30,000 rows

2. **Run Configuration Script**
   ```r
   source("config/create_lm_dictionaries.R")
   # Output: config/analysis_config.rds
   ```

3. **Verify Configuration**
   ```r
   config <- readRDS("config/analysis_config.rds")
   length(config$lm_positive)    # Should be ~350 (not ~60)
   length(config$lm_negative)    # Should be ~2,350 (not ~90)
   length(config$lm_uncertainty) # Should be ~300 (not ~85)
   ```

---

## UPDATED PIPELINE OVERVIEW

```
MODULE 0 (NEW)          MODULE 1            MODULE 2                MODULE 3
Config Setup     →     Text Cleaning →    Tokenization/DTM  →    Index Construction
────────────            ─────────────      ────────────────        ──────────────────
• Download LM CSV       • Remove HTML      • Dual-track tokens     • 4 disclosure indices
• Run config script     • Boilerplate      • Finance stopwords     • Official LM dicts
• Generate .rds         • Standardize      • Compounds             • Industry×year norm
                                           • Numeric regex         • Component transparency
Input:                  Input:             • DFM + vocab           
LM official CSV         raw 10-K text                              Input:
                                           Input:                  Config + DFMs + numeric
Output:                 Output:            cleaned text            
analysis_config.rds     cleaned_text.rds                           Output:
                                           Output:                 disclosure_indices.rds
                                           dfm_objects.rds         
                                           tokens_objects.rds      
                                           numeric_features.rds    
```

---

## CONFIGURATION SETUP (DETAILED)

### Step 1: Obtain LM Dictionary CSV

**Official Source:**
- Website: https://sraf.nd.edu/loughranmcdonald-master-dictionary/
- Direct link: https://sraf.nd.edu/textual-analysis/resources/
- Select: "Master Dictionary (CSV Format)"
- Version: 1993-2024 or later

**Alternative Sources:**
- Researcher's institutional repository (if licensed)
- Contact: sraf@nd.edu for access questions

**File Specifications:**
- Format: CSV (comma-separated)
- Expected columns: Word, Seq_num, Positive, Negative, Uncertainty, Litigious, Constraining, etc.
- Expected rows: >10,000 (typically ~30,000)
- Encoding: UTF-8 or ASCII

### Step 2: Place CSV File

```bash
# Recommended location
mkdir -p /mnt/data
cp [download_path]/Loughran-McDonald_MasterDictionary_1993-2024.csv /mnt/data/

# Verify file
head -n 5 /mnt/data/Loughran-McDonald_MasterDictionary_1993-2024.csv
```

**Alternative:** Update path in `config/create_lm_dictionaries.R` line 11:
```r
lm_csv_path <- "/your/custom/path/LM_Dictionary.csv"
```

### Step 3: Run Configuration Script

```r
# From R console or RStudio
setwd("/path/to/mna-disclosure")
source("config/create_lm_dictionaries.R")
```

**Expected Output:**
```
=== LM DICTIONARY CONFIGURATION ===
Loading official LM Master Dictionary from:
  /mnt/data/Loughran-McDonald_MasterDictionary_1993-2024.csv

STEP 1: Loading and standardizing LM dictionary...
  - Loaded 30000 rows from CSV
  - Token column identified: 'word'
  - After cleaning: 29500 valid tokens

STEP 2: Extracting LM official categories...
  - Positive:    354 terms (1.2% of dictionary)
  - Negative:    2355 terms (8.0% of dictionary)
  - Uncertainty: 297 terms (1.0% of dictionary)
  - Litigious:   903 terms (3.1% of dictionary)

STEP 3: Checking for optional LM categories...
  ✓ constraining:      184 terms
  ✓ strong_modal:      8 terms
  ✓ weak_modal:        27 terms

STEP 4: Creating project-specific dictionaries...
  ✓ Forward-Looking:  65 terms (project-defined)
  ✓ Operational:      80 phrases (project-defined)
  ✓ Negation:         12 terms (for validation)

STEP 5: Setting analysis parameters...
  - Index weights: 60% numeric, 40% compounds
  - Min cell size for normalization: 5
  - Outlier threshold: |z| > 5
  - TF-IDF scheme: tf='prop', df='inverse'

STEP 6: Packaging and saving configuration...
  ✓ Configuration saved to: config/analysis_config.rds

=== CONFIGURATION COMPLETE ===

✓ Ready for Module 3: Index Construction
  Load with: config <- readRDS('config/analysis_config.rds')
```

### Step 4: Validate Configuration

```r
# Load and inspect
config <- readRDS("config/analysis_config.rds")

# Check dictionary sizes (should be MUCH larger than before)
cat("LM Positive:", length(config$lm_positive), "\n")         # ~350
cat("LM Negative:", length(config$lm_negative), "\n")         # ~2,350
cat("LM Uncertainty:", length(config$lm_uncertainty), "\n")   # ~300
cat("LM Litigious:", length(config$lm_litigious), "\n")       # ~900

# Check optional categories
names(config$lm_optional)  # May include: constraining, strong_modal, weak_modal

# Check project lists (unchanged)
length(config$forward_looking_extended)  # 65
length(config$operational_phrases)       # 80

# Check metadata
config$metadata$source       # "Loughran & McDonald (2011) Master Dictionary (official CSV)"
config$metadata$lm_csv_path  # Path to CSV file
config$metadata$csv_rows     # Number of rows processed
```

---

## MODULE 3 INPUTS (UPDATED)

### Required Files (Same as Before)

1. **`data/interim/dfm_objects.rds`** (from Module 2)
2. **`data/interim/numeric_features.rds`** (from Module 2)
3. **`data/interim/vocabulary_lists.rds`** (optional, from Module 2)

### NEW Required File

4. **`config/analysis_config.rds`** (from config script)
   - Must be generated before running Module 3
   - Contains complete official LM dictionaries
   - Contains project-specific lists and parameters

---

## MODULE 3 OUTPUTS (UNCHANGED STRUCTURE)

### Primary Output

**`data/interim/disclosure_indices.rds`**

**Structure:** Same as before (2,391 obs × ~35 variables)

**Key Variables:** Unchanged
- `operational_specificity_norm`
- `forward_looking_norm`
- `risk_disclosure_tfidf_norm` (← Better measured with complete LM)
- `tone_lm_norm` (← Better measured with complete LM)

**New Attributes:**
```r
indices <- readRDS("data/interim/disclosure_indices.rds")

# New metadata
attr(indices, "dictionary_source")  # "LM Master Dictionary (official CSV)"
attr(indices, "dictionary_csv")     # Path to CSV file
```

---

## EXPECTED CHANGES IN RESULTS

### What Will Change

1. **Match Rates (Reported in Output)**
   - Risk Disclosure: Was ~120 matched terms → Now ~800-1,200
   - Managerial Tone: Was ~95 matched terms → Now ~600-900
   - **This is improvement** (more comprehensive measurement)

2. **Index Distributions (Minor Changes)**
   - Means may shift slightly (±0.1-0.2 SD)
   - Standard deviations may change (better capture variation)
   - **Normal:** Better measurement, same constructs

3. **Correlation Matrix (Stable)**
   - Should remain similar (within ±0.05)
   - If large changes: investigate (may indicate issue)

### What Will NOT Change

1. **Operational Specificity** (uses numeric patterns, not LM)
2. **Forward-Looking Intensity** (uses project list, not LM)
3. **Index logic and formulas** (unchanged)
4. **Normalization approach** (unchanged)
5. **Output structure** (same variables, same file)

---

## RUNNING MODULE 3 (UPDATED WORKFLOW)

### Complete Workflow from Scratch

```r
# Step 0: Configuration (NEW)
source("config/create_lm_dictionaries.R")
# Verify: config/analysis_config.rds created

# Step 1: Load packages
library(quanteda)
library(dplyr)
library(tidyr)

# Step 2: Run Module 3
source("src/nlp/03_construct_indices.R")
# Output: data/interim/disclosure_indices.rds
#         reports/03_index_construction_report.md

# Step 3: Validate
indices <- readRDS("data/interim/disclosure_indices.rds")
summary(indices[, c("operational_specificity_norm",
                    "forward_looking_norm",
                    "risk_disclosure_tfidf_norm",
                    "tone_lm_norm")])
```

### Troubleshooting

**Error: "Configuration not found"**
```
Error in readRDS(path_config): cannot open the connection
```
**Solution:** Run `source("config/create_lm_dictionaries.R")` first

**Error: "LM CSV not found"**
```
Error: LM CSV not found at: /mnt/data/Loughran-McDonald_MasterDictionary_1993-2024.csv
```
**Solution:** Download CSV and place at specified path, or update path in config script

**Warning: "CSV has unexpectedly few rows"**
```
Warning: LM CSV has unexpectedly few rows (1234). Expected >10,000.
```
**Solution:** Re-download CSV (file may be corrupted or truncated)

---

## COMPARISON WITH PREVIOUS VERSION (OPTIONAL)

If you have results from a previous run with subset dictionaries:

### Step 1: Save Previous Results

```r
# Before updating
indices_old <- readRDS("data/interim/disclosure_indices.rds")
saveRDS(indices_old, "data/interim/disclosure_indices_SUBSET.rds")
```

### Step 2: Run New Version

```r
# Run updated Module 3
source("config/create_lm_dictionaries.R")
source("src/nlp/03_construct_indices.R")

# Load new results
indices_new <- readRDS("data/interim/disclosure_indices.rds")
```

### Step 3: Compare

```r
# Merge for comparison
comp <- indices_old %>%
  select(deal_id, 
         risk_old = risk_disclosure_tfidf_norm,
         tone_old = tone_lm_norm) %>%
  inner_join(
    indices_new %>%
      select(deal_id,
             risk_new = risk_disclosure_tfidf_norm,
             tone_new = tone_lm_norm),
    by = "deal_id"
  )

# Compare means
cat("Risk Disclosure:\n")
cat("  Old mean:", mean(comp$risk_old, na.rm=TRUE), "\n")
cat("  New mean:", mean(comp$risk_new, na.rm=TRUE), "\n")
cat("  Difference:", mean(comp$risk_new - comp$risk_old, na.rm=TRUE), "\n")

cat("\nManagerial Tone:\n")
cat("  Old mean:", mean(comp$tone_old, na.rm=TRUE), "\n")
cat("  New mean:", mean(comp$tone_new, na.rm=TRUE), "\n")
cat("  Difference:", mean(comp$tone_new - comp$tone_old, na.rm=TRUE), "\n")

# Correlation
cor(comp[, c("risk_old", "risk_new", "tone_old", "tone_new")], 
    use = "pairwise.complete.obs")
# Expect: Old vs New correlation >0.90 (same construct, better measurement)
```

### Step 4: Report (If Necessary)

If results differ substantially (|Δmean| > 0.3 SD):

> "We initially computed indices using a core subset of LM dictionaries. Robustness checks using the complete official LM dictionaries yield qualitatively similar results [correlation r=X.XX], with [describe any notable differences]. We report results using the complete dictionaries (maximum coverage) in main analyses."

---

## THESIS METHODS SECTION (UPDATED TEXT)

### Disclosure Measurement - Data Sources

**Add this paragraph:**

> "We employ the complete Loughran-McDonald Master Dictionary (Loughran & McDonald, 2011) for all dictionary-based disclosure measures. The dictionary is obtained directly from the official CSV file (version 2024, available at https://sraf.nd.edu/loughranmcdonald-master-dictionary/), ensuring exact replicability and maximum coverage. The LM dictionary is specifically validated for financial text and outperforms general-purpose sentiment dictionaries in predicting firm outcomes (Loughran & McDonald, 2011). Our measurement approach extracts all terms flagged in each LM category (Positive, Negative, Uncertainty, Litigious) without subsetting, providing complete coverage of financial sentiment and risk language."

### Risk Disclosure (Update)

**Replace old text with:**

> "Risk Disclosure is measured by applying the complete LM Uncertainty (297 terms) and Negative (2,355 terms) dictionaries to Item 1A Risk Factors, yielding approximately 2,600 unique risk-related terms. We apply TF-IDF weighting (quanteda::dfm_tfidf with proportional term frequency scheme) to downweight generic boilerplate and emphasize substantive disclosure. This approach separates transparency about execution risks from mere compliance-driven disclosure length, following Loughran & McDonald (2011) and Campbell et al. (2014)."

### Managerial Tone (Update)

**Replace old text with:**

> "Managerial Tone is calculated using the complete LM Positive (354 terms) and Negative (2,355 terms) dictionaries applied to MD&A. We compute ToneLM = (Positive - Negative) / (Positive + Negative) as a net sentiment measure, following Loughran & McDonald (2011). Unlike generic sentiment dictionaries (e.g., Harvard IV-4), the LM dictionary is specifically calibrated for financial text, where words like 'liability' and 'capital' have domain-specific meanings distinct from general usage."

---

## VALIDATION CHECKLIST (UPDATED)

### Pre-Run Checks
- [ ] Official LM CSV file downloaded and placed correctly
- [ ] Config script (`create_lm_dictionaries.R`) runs without errors
- [ ] Config file (`analysis_config.rds`) created successfully
- [ ] Config contains expected dictionary sizes:
  - [ ] LM Positive: ~350 terms (not ~60)
  - [ ] LM Negative: ~2,350 terms (not ~90)
  - [ ] LM Uncertainty: ~300 terms (not ~85)
  - [ ] LM Litigious: ~900 terms (not ~80)

### Post-Run Checks
- [ ] Module 3 runs without errors
- [ ] Output file created: `data/interim/disclosure_indices.rds`
- [ ] Report created: `reports/03_index_construction_report.md`
- [ ] Match rates increased dramatically:
  - [ ] Risk Disclosure: 800-1,200 matched terms (not ~120)
  - [ ] Managerial Tone: 600-900 matched terms (not ~95)
- [ ] Index distributions reasonable (check report)
- [ ] No warnings about missing critical words (modals, negation)

### Comparison Checks (If Previous Results Available)
- [ ] Old vs new correlation >0.85 (same constructs)
- [ ] Mean differences <0.5 SD (measurement improvement, not construct change)
- [ ] Sign consistency in regressions (direction unchanged)
- [ ] Document any substantial differences in robustness section

---

## ADVANTAGES OF UPDATED WORKFLOW

### For Researcher

✅ **No manual dictionary curation** (error-prone, time-consuming)  
✅ **Future-proof** (LM updates CSV periodically)  
✅ **Transparent provenance** (official source, not researcher discretion)  
✅ **Exact replicability** (download same CSV → identical results)

### For Thesis Defense

✅ **Unimpeachable source** ("official LM CSV" vs "core subset")  
✅ **Maximum coverage** (fewer false negatives)  
✅ **Standard methodology** (complete dictionary is norm in literature)  
✅ **No cherry-picking concerns** ("we use all LM terms")

### For Reviewers

✅ **Easy to verify** (download same CSV, check matches)  
✅ **Clear documentation** (CSV version, path, extraction method)  
✅ **Robust measurement** (complete coverage reduces measurement error)

---

## SUMMARY

**Major Changes:**
1. ✅ New prerequisite: Download official LM CSV and run config script
2. ✅ Module 3 now loads dictionaries from config (not internal hardcoding)
3. ✅ 15-26× better dictionary coverage for Risk/Tone indices
4. ✅ Updated thesis text provided (Methods section)

**Unchanged:**
- Module 3 logic and formulas
- Output structure and variable names
- Integration with Module 2 and Module 6
- Validation and quality checks

**Action Required:**
1. Download official LM CSV from https://sraf.nd.edu/loughranmcdonald-master-dictionary/
2. Run `source("config/create_lm_dictionaries.R")`
3. Run `source("src/nlp/03_construct_indices.R")`
4. Update thesis Methods section (text provided above)
5. Optional: Compare with previous results if available

**Result:** Production-ready indices with maximum defensibility.

---

**END OF UPDATED INTEGRATION GUIDE**
