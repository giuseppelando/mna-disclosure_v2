# Module 5 — Descriptive analysis and cross-validation

## Inputs
- Indices dataset: `data/interim/disclosure_indices.rds`
- Base dataset: (not used)

## Indices analyzed
- operational_specificity_norm, forward_looking_norm, risk_disclosure_tfidf_norm, tone_lm_norm, risk_disclosure_tfidf, tone_lm

## Outputs
- Tables in `output/tables/`: summary stats, correlations (and flagged high-correlation pairs), ANOVA summaries, industry means, time-trend means.
- Figures in `output/figures/`: histograms, Q-Q plots, correlation heatmap, time trends.

## Interpretation guide (for thesis write-up)
- Distribution plots: normalized indices should be centered near zero. Departures from normality are documented as empirical properties.
- Correlations: pairs with |r| ≥ 0.70 are flagged for multicollinearity risk in later regressions.
- Industry ANOVA: run only on raw (pre-normalization) indices; normalized indices are standardized within sic2/sic2×year and are therefore not suitable for this check.
- Time trends: year-level drift supports the use of year fixed effects (or industry×year fixed effects) in outcome models.

## Session info

R version 4.5.2 (2025-10-31 ucrt)
Platform: x86_64-w64-mingw32/x64
Running under: Windows 11 x64 (build 26200)

Matrix products: default
  LAPACK version 3.12.1

locale:
[1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8    LC_MONETARY=English_United States.utf8 LC_NUMERIC=C                           LC_TIME=English_United States.utf8    

time zone: Europe/Rome
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] Matrix_1.7-4              quanteda.textstats_0.97.2 quanteda_4.3.1            writexl_1.5.4             openxlsx_4.2.8.1          jsonlite_2.0.0            janitor_2.2.1             readxl_1.4.5             
 [9] httr_1.4.7                glue_1.8.0                lubridate_1.9.4           forcats_1.0.1             stringr_1.6.0             dplyr_1.1.4               purrr_1.2.0               readr_2.1.5              
[17] tidyr_1.3.1               tibble_3.3.0              ggplot2_4.0.0             tidyverse_2.0.0          

loaded via a namespace (and not attached):
 [1] utf8_1.2.6         generics_0.1.4     stringi_1.8.7      lattice_0.22-7     hms_1.1.4          magrittr_2.0.4     grid_4.5.2         timechange_0.3.0   RColorBrewer_1.1-3 cellranger_1.1.0   zip_2.3.3          stopwords_2.3     
[13] scales_1.4.0       textshaping_1.0.4  cli_3.6.5          rlang_1.1.6        crayon_1.5.3       bit64_4.6.0-1      withr_3.0.2        parallel_4.5.2     tools_4.5.2        tzdb_0.5.0         fastmatch_1.1-6    curl_7.0.0        
[25] vctrs_0.6.5        R6_2.6.1           lifecycle_1.0.4    snakecase_0.11.1   bit_4.6.0          vroom_1.6.6        ragg_1.5.0         pkgconfig_2.0.3    pillar_1.11.1      gtable_0.3.6       Rcpp_1.1.0         systemfonts_1.3.1 
[37] tidyselect_1.2.1   rstudioapi_0.17.1  farver_2.1.2       labeling_0.4.3     compiler_4.5.2     nsyllable_1.0.1    S7_0.2.0          
