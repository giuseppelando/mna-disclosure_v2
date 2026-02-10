# Tokenization and DTM Construction Report (v3)

**Generated:** 2026-02-09 22:57:59.657356
**Input:** C:/Users/giuse/Documents/GitHub/mna-disclosure/data/interim/cleaned_text.rds
**Documents:** 2391

## Revision Notes (v3)
- Fixed million/billion regex (no longer matches 'months', 'below', etc.)
- Division-by-zero guards on all density calculations
- Quarter regex corrected for lowercase text
- Protected-terms patch for high-frequency modals

## DFM Summary
```
             DFM_Type Documents Features Sparsity
1 MD&A Unigram (Dict)      2391    31059   96.38%
2      MD&A Compounds      2391    31121   96.49%
3   MD&A Free Bigrams      2391   414087   98.99%
4        Risk Unigram      2391    28409   95.57%
5         Risk TF-IDF      2391    28409   95.57%
```

## Finance Word Retention
                   word in_mda_uni in_risk_uni
revenue         revenue       TRUE        TRUE
margin           margin       TRUE        TRUE
debt               debt       TRUE        TRUE
capital         capital      FALSE        TRUE
risk               risk       TRUE        TRUE
uncertainty uncertainty       TRUE        TRUE
expect           expect       TRUE        TRUE
will               will       TRUE        TRUE
increase       increase       TRUE        TRUE
decrease       decrease       TRUE        TRUE
not                 not       TRUE        TRUE
may                 may       TRUE        TRUE

