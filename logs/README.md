# Pipeline Logs

Created: 2026-02-18

This directory stores structured logs from each pipeline run.
Each script writes a timestamped log file here with full details
suitable for thesis documentation.

## Naming convention

```
STEP_SCRIPTNAME_YYYYMMDD_HHMMSS.log
```

## Steps

| Step | Script | Description |
|------|--------|-------------|
| 01   | ingest | SDC data ingestion and cleaning |
| 02   | restrict | Sample restriction cascade |
| 03   | cik_match | Target-to-CIK matching |
| 04   | filing_id | 10-K filing identification |
| 05   | download | 10-K filing download from EDGAR |
| 06   | parse | Section extraction (MD&A, Risk Factors) |
| 07   | clean_text | EDGAR-aware text cleaning |
| 08   | tokenize | Tokenization and DFM construction |
| 09   | indices | Disclosure index construction |
| 10   | validate | KWIC and distributional validation |
| 11   | merge | Final dataset assembly |
| 12   | econ_prep | Econometric variable preparation |
| 13   | econ_run | Regression models |
