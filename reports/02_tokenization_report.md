# Tokenization and DTM Construction Report (v3)

**Generated:** 2026-02-15 15:51:17.029115
**Input:** C:/Users/giuse/Documents/GitHub/mna-disclosure/data/interim/cleaned_text.rds
**Documents:** 4186

## Revision Notes (v3)
- Fixed million/billion regex (no longer matches 'months', 'below', etc.)
- Division-by-zero guards on all density calculations
- Quarter regex corrected for lowercase text
- Protected-terms patch for high-frequency modals

## DFM Summary
```
             DFM_Type Documents Features Sparsity
1 MD&A Unigram (Dict)      4186    46342   97.50%
2      MD&A Compounds      4186    46405   97.58%
3   MD&A Free Bigrams      4186   695148   99.35%
4        Risk Unigram      4186    38849   96.72%
5         Risk TF-IDF      4186    38849   96.72%
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

