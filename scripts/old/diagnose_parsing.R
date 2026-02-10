# ==============================================================================
# Quick Diagnostic: Test File Access and Parsing
# ==============================================================================

library(tidyverse)
library(glue)

# Detect project root
current_dir <- getwd()
if (dir.exists(file.path(current_dir, "config")) && 
    dir.exists(file.path(current_dir, "src"))) {
  PROJECT_ROOT <- current_dir
} else {
  PROJECT_ROOT <- dirname(current_dir)
}

# Load config
source(file.path(PROJECT_ROOT, "config/pipeline_config.R"))
source(file.path(PROJECT_ROOT, "src/30_nlp/section_parsers.R"))

cat("=== FILE ACCESS DIAGNOSTIC ===\n\n")

# Check raw directory
raw_dir <- PATHS$raw_10k_dir
cat(glue("Raw 10-K directory: {raw_dir}\n"))
cat(glue("Directory exists: {dir.exists(raw_dir)}\n\n"))

# List files
files <- list.files(raw_dir, pattern = "\\.txt$", full.names = FALSE)
cat(glue("Total .txt files found: {length(files)}\n\n"))

if (length(files) > 0) {
  # Show first 5
  cat("First 5 filenames:\n")
  print(head(files, 5))
  cat("\n")
  
  # Test reading the first file
  test_file <- files[1]
  test_path <- file.path(raw_dir, test_file)
  
  cat(glue("Testing file: {test_file}\n"))
  cat(glue("Full path: {test_path}\n"))
  cat(glue("File exists: {file.exists(test_path)}\n"))
  
  if (file.exists(test_path)) {
    # Get file size
    size_mb <- file.size(test_path) / 1024 / 1024
    cat(glue("File size: {round(size_mb, 2)} MB\n\n"))
    
    # Try to read it
    cat("Attempting to read file...\n")
    start_time <- Sys.time()
    
    tryCatch({
      filing_text <- readLines(test_path, warn = FALSE) %>%
        paste(collapse = "\n")
      
      read_time <- as.numeric(Sys.time() - start_time)
      cat(glue("✓ Read successful in {round(read_time, 2)} seconds\n"))
      cat(glue("  Total characters: {nchar(filing_text)}\n\n"))
      
      # Try to parse it
      cat("Attempting to parse sections...\n")
      parse_start <- Sys.time()
      
      parsed <- parse_10k_sections(filing_text)
      
      parse_time <- as.numeric(Sys.time() - parse_start)
      cat(glue("✓ Parse completed in {round(parse_time, 2)} seconds\n"))
      cat(glue("  Parse status: {parsed$parse_status}\n"))
      cat(glue("  MD&A words: {parsed$mda_word_count}\n"))
      cat(glue("  Risk words: {parsed$risk_word_count}\n\n"))
      
      # Estimate total time
      total_time_per_file <- read_time + parse_time
      estimated_minutes <- (total_time_per_file * length(files)) / 60
      
      cat("=== TIME ESTIMATE ===\n")
      cat(glue("Time per file: ~{round(total_time_per_file, 2)} seconds\n"))
      cat(glue("Estimated total time: ~{round(estimated_minutes, 1)} minutes for {length(files)} files\n\n"))
      
      if (total_time_per_file > 5) {
        cat("⚠ WARNING: Processing is very slow (>5 sec per file)\n")
        cat("This might be due to:\n")
        cat("  - Large file sizes\n")
        cat("  - Slow disk I/O\n")
        cat("  - Complex regex patterns\n\n")
      }
      
    }, error = function(e) {
      cat(glue("✗ Error: {e$message}\n"))
    })
    
  } else {
    cat("✗ File does not exist!\n")
  }
  
} else {
  cat("✗ No .txt files found in directory!\n")
  cat("\nCheck if:\n")
  cat("  1. Files are in the correct directory\n")
  cat("  2. Files have .txt extension\n")
  cat("  3. Directory path is correct\n")
}

cat("\n=== DIAGNOSTIC COMPLETE ===\n")
