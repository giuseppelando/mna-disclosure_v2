# TEXT CLEANING REPORT (v2)
Generated: 2026-02-09 22:55:44.168698

## Input
- File: C:/Users/giuse/Documents/GitHub/mna-disclosure/data/processed/deals_with_10k_text_analysis.rds
- Deals: 2391

## Cleaning Operations
1. HTML/XBRL artifact removal
2. Entity decoding
3. Conservative boilerplate removal (max span 1200 chars)
4. Whitespace normalisation
5. Lowercase conversion

## Output Summary
- MD&A empty: 0 (0.0%)
- Risk Factors empty: 0 (0.0%)

## Word Count Statistics (Clean)
### MD&A:
- Min: 119
- Median: 10400
- Mean: 11303.5
- Max: 72289

### Risk Factors:
- Min: 221
- Median: 7712
- Mean: 9385.1
- Max: 43269

## Financial Pattern Preservation
Sample size: 100
- With numbers: 100.0%
- With percentages: 98.0%
- With currency: 100.0%
- With decimals: 100.0%

## Validation Notes
- Check median reduction ratio in [0.70, 0.95] (not too aggressive)
- Verify >95% of sample documents retain financial patterns
- Empty documents flagged but not dropped (preserve sample integrity)

