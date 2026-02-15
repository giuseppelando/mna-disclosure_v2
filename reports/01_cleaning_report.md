# TEXT CLEANING REPORT (v2)
Generated: 2026-02-15 15:46:25.131081

## Input
- File: C:/Users/giuse/Documents/GitHub/mna-disclosure/data/processed/deals_with_10k_text_analysis.rds
- Deals: 4186

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
- Median: 10836
- Mean: 11862.5
- Max: 72289

### Risk Factors:
- Min: 99
- Median: 8034.5
- Mean: 9559.7
- Max: 70258

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

