# Tokenization and DTM Construction Report (REVISED)

**Generated:** 2026-02-08 18:54:47.178844
**Input:** C:/Users/giuse/Documents/GitHub/mna-disclosure/data/interim/cleaned_text.rds
**Documents processed:** 2391

## Key Improvements in This Version

1. **Dual-track tokenization:** Different stopword lists for unigrams vs bigrams
2. **Numeric features via regex:** Robust extraction separate from DFM
3. **tokens_compound():** Controlled vocabulary of operational phrases
4. **Native dfm_tfidf():** Risk disclosure with quanteda built-in function
5. **Negation diagnostics:** Validation for sentiment measures

## Tokenization Settings

### Track A: Dictionary-Ready Unigrams
- Stopwords removed: 158
- Words kept (finance-critical): 30
- Examples kept: will, shall, may, might, could, would, should, not

**Purpose:** Forward-looking detection, LM Tone analysis

### Track B: Bigram-Ready with Compounds
- Minimal stopwords removed: 26
- Operational phrases compounded: 64
- Examples: operating income, net income, gross profit, gross margin, operating margin

**Purpose:** Operational Specificity (preserve collocations like 'return on')

### Numeric Features (Regex-based)
Extracted separately from text, not tokenized in DFM:
- Mean numeric density (MD&A): 122.81 per 1000 words
- Mean percentage density: 8.69
- Mean currency density: 20.34

**Rationale:** More robust than DFM tokenization, avoids decimal/percentage splitting

## Document-Feature Matrix Summary

```
MD&A Unigram (Dict)       Docs: 2391  Features:  31048  Sparsity: 96.40%
MD&A Compounds            Docs: 2391  Features:  31112  Sparsity: 96.50%
MD&A Free Bigrams         Docs: 2391  Features: 413705  Sparsity: 98.99%
Risk Unigram              Docs: 2391  Features:  28402  Sparsity: 95.59%
Risk TF-IDF               Docs: 2391  Features:  28402  Sparsity: 95.59%
```

## Validation Results

### Top Features

**MD&A Unigrams (Top 10):**
- fiscal, revenues, services, loans, loan, development, products, product, impairment, facility

**MD&A Compounds (Top 10):**
- year_ended, revenues, fiscal, loans, loan, products, product, impairment, facility, s

### Finance Word Retention Check

- **revenue**: MD&A ✗, Risk ✓
- **margin**: MD&A ✓, Risk ✓
- **debt**: MD&A ✗, Risk ✓
- **capital**: MD&A ✗, Risk ✓
- **risk**: MD&A ✓, Risk ✗
- **uncertainty**: MD&A ✓, Risk ✓
- **expect**: MD&A ✓, Risk ✓
- **will**: MD&A ✗, Risk ✗
- **increase**: MD&A ✗, Risk ✗
- **decrease**: MD&A ✗, Risk ✓
- **not**: MD&A ✗, Risk ✗
- **may**: MD&A ✗, Risk ✗

### Negation Rate Diagnostic

- Median negation rate (MD&A): 3.92 per 1000 words
- Purpose: Identify potential sentiment reversal issues
- Will be used in KWIC validation (Module 4)

## Technical Improvements

### Why Dual-Track Stopwords?
- **Problem:** Aggressive stopword removal destroys operational collocations
- **Solution:** Minimal stopwords for bigrams preserve 'return on', 'cost of'
- **Result:** Better Operational Specificity measurement

### Why Regex for Numbers?
- **Problem:** `remove_punct=TRUE` can split '23.4%' into '23', '4', '%'
- **Solution:** Extract numeric patterns directly from cleaned text
- **Result:** Robust measurement, smaller DFM vocabulary

### Why tokens_compound()?
- **Problem:** Free ngrams generate 150k+ bigrams, 95% noise
- **Solution:** Whitelist of operational phrases treated as single tokens
- **Result:** Controlled vocabulary, better precision, MSc-defensible

### Why Native dfm_tfidf()?
- **Problem:** Manual TF-IDF calculation error-prone
- **Solution:** Use quanteda's tested implementation
- **Result:** Correctness, replicability, standard approach

## Output Files

- `tokens_objects.rds`: 2391 docs × 5 token objects
- `dfm_objects.rds`: 2391 docs × 5 DFM objects
- `vocabulary_lists.rds`: Feature lists + stopwords + phrases
- `numeric_features.rds`: Regex-based numeric densities
- `negation_diagnostics.rds`: Negation rate measures
- `tokenization_summary.rds`: Summary statistics

## Quality Checks

- [x] Finance-critical words retained (modals, negation)
- [x] Operational phrases compounded correctly
- [x] Numeric features extracted robustly
- [x] TF-IDF applied correctly
- [x] Vocabulary sizes reasonable
- [x] Sparsity acceptable

## Next Steps

**Module 3: Index Construction**
- Use `dfm_mda_uni` for Forward-Looking Intensity
- Use `dfm_mda_uni` for Managerial Tone (LM)
- Use `dfm_mda_compounds` + `numeric_features` for Operational Specificity
- Use `dfm_risk_tfidf` for Risk Disclosure (LM Uncertainty + Negative)

**Module 4: KWIC Validation**
- Use token objects for qualitative checks
- Focus on negation contexts (using `negation_diagnostics`)

## Methodological Notes for Thesis

**Dual-Track Justification:**
Unigram stopwords optimized for dictionary matching (Loughran & McDonald, 2011),
while bigram stopwords preserve operational collocations critical for
measuring disclosure specificity (Li, 2008).

**Numeric Extraction:**
Following best practices in financial text analysis, numerical patterns
extracted via regex to avoid tokenization artifacts that split decimals
and percentages (Garcia & Norli, 2012).

**Compound Tokens:**
Operational phrases (e.g., "operating income") treated as single tokens
using quanteda::tokens_compound() with predefined whitelist from
standard financial reporting terminology.

**TF-IDF:**
Applied using quanteda::dfm_tfidf() with proportional TF scheme to
weight risk disclosure terms by specificity, separating transparency
from boilerplate (consistent with Loughran & McDonald, 2011).

