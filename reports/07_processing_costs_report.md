# Module 7 — Processing Costs Report (v4)

Generated: 2026-02-19 02:12:59.569544
Documents: 4186

## Scope
This module computes ONLY processing-cost proxies (readability + volume).
All other disclosure indices are in Module 3 v6.

## Measures

### Readability
- MD&A Fog Index: mean=21.78, sd=1.75
- MD&A FK Grade: mean=17.65, sd=1.65
- Risk Fog Index: mean=22.53, sd=1.52
- Risk FK Grade: mean=18.59, sd=1.43

### Volume
- MD&A log(length): mean=9.16, sd=0.49
- Risk log(length): mean=8.94, sd=0.68

## Implementation
- Readability: quanteda.textstats::textstat_readability() (standard, replicable)
- Word count denominator: wc_alpha (v3)

## Caveats (document in thesis)
- Fog is systematically inflated on 10-Ks because routine financial terms
  (amortization, collateralized, etc.) are counted as complex words.
  Reference: Loughran & McDonald (2014), 'Measuring Readability in
  Financial Disclosures', Journal of Finance.
- log(length) correlates with firm size and regulatory complexity. Always
  include size controls (log assets, number of segments) in regressions.
- These are processing-cost proxies, not information-content measures.
  Their expected sign in premium regressions is ambiguous: longer/harder
  disclosures may indicate complexity (negative) or thoroughness (positive).

## Integration with Module 6
```r
# In Module 6, add after the main merge:
proc_costs <- readRDS('data/interim/processing_costs.rds')
merged <- merged %>% left_join(proc_costs, by = 'deal_id')
```

## Output
- C:/Users/giuse/Documents/GitHub/mna-disclosure/data/interim/processing_costs.rds
