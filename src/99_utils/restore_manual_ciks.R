# =============================================================================
# src/99_utils/restore_manual_ciks.R
# =============================================================================
# Utility to backup and restore manually entered CIKs after re-running pipeline
#
# Usage:
#   source("src/99_utils/restore_manual_ciks.R")
#   
#   # Backup BEFORE re-running pipeline
#   backup_manual_ciks()
#   
#   # Restore AFTER re-running pipeline
#   deals <- readRDS("data/interim/deals_with_cik.rds")
#   deals_restored <- restore_manual_ciks(deals)
#   saveRDS(deals_restored, "data/interim/deals_with_cik.rds")
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
  library(glue)
})

#' Backup Manual CIK Matches
#'
#' Creates a backup of manually entered CIK matches from the Excel file.
#' Run this BEFORE re-running the pipeline to preserve your manual work.
#'
#' @param input_file Path to deals_with_cik_matches.xlsx
#' @param backup_file Path for backup RDS file
#' @return Invisibly returns the backup data frame
#'
backup_manual_ciks <- function(
  input_file = "data/interim/deals_with_cik_matches.xlsx",
  backup_file = "data/interim/manual_cik_backup.rds"
) {
  
  if (!file.exists(input_file)) {
    message(glue("Input file not found: {input_file}"))
    return(invisible(NULL))
  }
  
  message(glue("Reading: {input_file}"))
  df <- read_xlsx(input_file)
  
  # Extract only the essential columns for CIK matching
  cik_lookup <- df %>%
    filter(!is.na(target_cik)) %>%
    select(deal_id, target_name, target_cik) %>%
    distinct(deal_id, .keep_all = TRUE) %>%
    mutate(
      target_cik = as.character(target_cik),
      backup_date = Sys.time()
    )
  
  saveRDS(cik_lookup, backup_file)
  
  message(glue("✓ Backed up {nrow(cik_lookup)} CIK matches"))
  message(glue("  Saved to: {backup_file}"))
  message(glue("  Timestamp: {cik_lookup$backup_date[1]}"))
  
  return(invisible(cik_lookup))
}


#' Restore Manual CIK Matches
#'
#' Restores manually entered CIK matches from backup after re-running the pipeline.
#' Run this AFTER re-running the pipeline to merge back your manual work.
#'
#' @param deals_df Data frame with deals (output from pipeline)
#' @param backup_file Path to backup RDS file
#' @param join_by Column to join on (default: "deal_id")
#' @return Data frame with restored CIKs
#'
restore_manual_ciks <- function(
  deals_df,
  backup_file = "data/interim/manual_cik_backup.rds",
  join_by = "deal_id"
) {
  
  if (!file.exists(backup_file)) {
    message("No CIK backup file found. Returning original data.")
    return(deals_df)
  }
  
  cik_backup <- readRDS(backup_file)
  
  message(glue("Loaded {nrow(cik_backup)} backed up CIK matches"))
  message(glue("Backup timestamp: {cik_backup$backup_date[1]}"))
  
  # Count original coverage
  n_original_cik <- sum(!is.na(deals_df$target_cik) & deals_df$target_cik != "")
  
  # Prepare backup for join
  cik_backup_clean <- cik_backup %>%
    select(deal_id, target_cik_backup = target_cik) %>%
    distinct(deal_id, .keep_all = TRUE)
  
  # Join and restore
  # Use backup CIK where original is missing
  deals_restored <- deals_df %>%
    left_join(cik_backup_clean, by = "deal_id") %>%
    mutate(
      target_cik = case_when(
        # If original CIK is missing or empty, use backup
        is.na(target_cik) | target_cik == "" ~ target_cik_backup,
        # Otherwise keep original
        TRUE ~ target_cik
      )
    ) %>%
    select(-target_cik_backup)
  
  # Count restored coverage
  n_restored_cik <- sum(!is.na(deals_restored$target_cik) & deals_restored$target_cik != "")
  n_added <- n_restored_cik - n_original_cik
  
  message(glue("\nCIK Coverage:"))
  message(glue("  Before restore: {n_original_cik}"))
  message(glue("  After restore:  {n_restored_cik}"))
  message(glue("  Added from backup: {n_added}"))
  
  return(deals_restored)
}


#' Show Backup Status
#'
#' Displays information about the current CIK backup file
#'
#' @param backup_file Path to backup RDS file
#'
show_backup_status <- function(
  backup_file = "data/interim/manual_cik_backup.rds"
) {
  
  if (!file.exists(backup_file)) {
    message("No CIK backup file exists.")
    message("Run backup_manual_ciks() to create one before re-running pipeline.")
    return(invisible(NULL))
  }
  
  cik_backup <- readRDS(backup_file)
  
  cat("\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  cat("CIK BACKUP STATUS\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  cat(glue("File: {backup_file}"), "\n")
  cat(glue("Timestamp: {cik_backup$backup_date[1]}"), "\n")
  cat(glue("CIK matches: {nrow(cik_backup)}"), "\n")
  cat(paste(rep("-", 60), collapse = ""), "\n")
  cat("Sample:\n")
  print(head(cik_backup %>% select(deal_id, target_name, target_cik), 10))
  cat(paste(rep("=", 60), collapse = ""), "\n")
  cat("\n")
  
  return(invisible(cik_backup))
}
