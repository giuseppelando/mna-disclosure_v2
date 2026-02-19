# TEXT CLEANING REPORT (v3 — dual word counts)
Generated: 2026-02-19 01:59:29.556896

## Key change in v3
Introduced dual word counts:
- `wc_total`: all non-whitespace tokens (for numeric density denominators)
- `wc_alpha`: alphabetic tokens only (for dictionary-based density denominators)

This resolves the denominator mismatch where dictionary hits (computed on
alphabetic tokens after `remove_numbers=TRUE`) were divided by total word count
(including numbers), systematically attenuating dictionary-based indices.

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

## Alpha/Total Ratio (diagnostic)
- MD&A  mean: 0.894
- Risk  mean: 0.987
(Expected range: 0.70–0.90 for typical 10-K text)

