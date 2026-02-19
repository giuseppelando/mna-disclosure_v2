# Tokenization and DTM Construction Report (v4)

**Generated:** 2026-02-19 02:05:03.549789
**Input:** C:/Users/giuse/Documents/GitHub/mna-disclosure/data/interim/cleaned_text.rds
**Documents:** 4186

## Key changes in v4 (feedback-driven)
1. **NO max_docfreq trimming** on dictionary DFMs. This preserves high-frequency
   LM terms (positive, negative, uncertainty) that were being removed in v3.
2. **Dual word counts** passed through: wc_total for numeric density,
   wc_alpha for dictionary-based density denominators.
3. Exploratory DFMs (for topic models/EDA) retain full trimming.
4. Protected-terms patch removed (unnecessary without max_docfreq trimming).

## DFM Summary
```
                  DFM_Type Documents Features Sparsity
1  MD&A Dict (no max trim)      4186    46469   97.24%
2 MD&A Explore (full trim)      4186    46336   97.52%
3           MD&A Compounds      4186    36985   97.03%
4        MD&A Free Bigrams      4186   685111   99.33%
5  Risk Dict (no max trim)      4186    38935   96.51%
6              Risk TF-IDF      4186    38935   96.51%
```

## Finance Word Retention
                   word in_mda_dict in_risk_dict
revenue         revenue        TRUE         TRUE
margin           margin        TRUE         TRUE
debt               debt        TRUE         TRUE
capital         capital        TRUE         TRUE
risk               risk        TRUE         TRUE
uncertainty uncertainty        TRUE         TRUE
expect           expect        TRUE         TRUE
will               will        TRUE         TRUE
increase       increase        TRUE         TRUE
decrease       decrease        TRUE         TRUE
not                 not        TRUE         TRUE
may                 may        TRUE         TRUE

