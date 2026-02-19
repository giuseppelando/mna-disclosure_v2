# extra_stats.R — compute missing descriptive statistics for Chapter 3
library(tidyverse)

ds <- readRDS("C:/Users/giuse/Documents/GitHub/mna-disclosure/data/final/analysis_dataset.rds")

cat("=== DEAL CONTROLS ===\n")
# percentage_of_cash
cat("\npercentage_of_cash:\n")
x <- ds$percentage_of_cash
cat(sprintf("  N=%d, mean=%.1f, sd=%.1f, p5=%.1f, p25=%.1f, median=%.1f, p75=%.1f, p95=%.1f\n",
    sum(!is.na(x)), mean(x,na.rm=T), sd(x,na.rm=T),
    quantile(x,.05,na.rm=T), quantile(x,.25,na.rm=T), quantile(x,.5,na.rm=T),
    quantile(x,.75,na.rm=T), quantile(x,.95,na.rm=T)))

# tender_offer_flag
cat("\ntender_offer_flag:\n")
cat(sprintf("  N=%d, mean=%.3f (= %% tender)\n", sum(!is.na(ds$tender_offer_flag)), mean(ds$tender_offer_flag, na.rm=T)))

# payment_method_clean (for descriptive commentary, not a control)
cat("\npayment_method_clean:\n")
print(table(ds$payment_method_clean, useNA="ifany"))

cat("\n=== FILING / TEXT ===\n")
# days_lag
cat("\ndays_lag:\n")
x <- ds$days_lag
cat(sprintf("  N=%d, mean=%.1f, sd=%.1f, p5=%.0f, p25=%.0f, median=%.0f, p75=%.0f, p95=%.0f\n",
    sum(!is.na(x)), mean(x), sd(x),
    quantile(x,.05), quantile(x,.25), quantile(x,.5), quantile(x,.75), quantile(x,.95)))

# mda_word_count
cat("\nmda_word_count:\n")
x <- ds$mda_word_count
cat(sprintf("  N=%d, mean=%.0f, sd=%.0f, p5=%.0f, p25=%.0f, median=%.0f, p75=%.0f, p95=%.0f\n",
    sum(!is.na(x)), mean(x), sd(x),
    quantile(x,.05), quantile(x,.25), quantile(x,.5), quantile(x,.75), quantile(x,.95)))

# risk_word_count
cat("\nrisk_word_count:\n")
x <- ds$risk_word_count
cat(sprintf("  N=%d, mean=%.0f, sd=%.0f, p5=%.0f, p25=%.0f, median=%.0f, p75=%.0f, p95=%.0f\n",
    sum(!is.na(x)), mean(x), sd(x),
    quantile(x,.05), quantile(x,.25), quantile(x,.5), quantile(x,.75), quantile(x,.95)))

# risk_fog
cat("\nrisk_fog:\n")
x <- ds$risk_fog
cat(sprintf("  N=%d, mean=%.2f, sd=%.2f, p5=%.2f, p25=%.2f, median=%.2f, p75=%.2f, p95=%.2f\n",
    sum(!is.na(x)), mean(x,na.rm=T), sd(x,na.rm=T),
    quantile(x,.05,na.rm=T), quantile(x,.25,na.rm=T), quantile(x,.5,na.rm=T),
    quantile(x,.75,na.rm=T), quantile(x,.95,na.rm=T)))

# risk_log_length
cat("\nrisk_log_length:\n")
x <- ds$risk_log_length
cat(sprintf("  N=%d, mean=%.2f, sd=%.2f, p5=%.2f, p25=%.2f, median=%.2f, p75=%.2f, p95=%.2f\n",
    sum(!is.na(x)), mean(x,na.rm=T), sd(x,na.rm=T),
    quantile(x,.05,na.rm=T), quantile(x,.25,na.rm=T), quantile(x,.5,na.rm=T),
    quantile(x,.75,na.rm=T), quantile(x,.95,na.rm=T)))

cat("\n=== YEAR DISTRIBUTION WITH % ===\n")
yr <- ds %>% count(year_announced) %>% mutate(pct = round(n/sum(n)*100, 1))
print(yr, n=20)

cat("\n=== COMPLETION BY YEAR ===\n")
comp_yr <- ds %>% group_by(year_announced) %>%
  summarise(n=n(), completed=sum(completion==1), rate=round(mean(completion)*100,1))
print(comp_yr, n=20)
