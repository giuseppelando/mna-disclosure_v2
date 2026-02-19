# ==============================================================================
# 00_archive_and_clean.R
# ==============================================================================
# Purpose:  Archive all outputs from the current pipeline run, then clean
#           working directories for a fresh run.
#
# What gets archived (copied):
#   data/interim/      → archive/data_interim/
#   data/processed/    → archive/data_processed/
#   data/final/        → archive/data_final/
#   output/            → archive/output/
#   reports/           → archive/reports/
#
# What gets cleaned (contents deleted, folder preserved):
#   data/interim/      (except submissions_cache/)
#   data/processed/
#   data/final/
#   output/tables/
#   output/figures/
#   output/logs/
#   reports/
#
# What is NEVER touched:
#   data/raw/          (10-K filings, companies.rds, SDC source)
#   src/               (code)
#   config/            (parameters)
#   scripts/           (orchestration)
#   data/interim/submissions_cache/   (EDGAR API cache, expensive to rebuild)
#
# What gets created:
#   logs/              (new project-root folder for the next run)
#
# Safety: the script verifies the archive is complete before deleting anything.
# ==============================================================================

suppressPackageStartupMessages({
  library(fs)
  library(glue)
})

# ==============================================================================
# CONFIGURATION
# ==============================================================================

PROJECT_ROOT <- "C:/Users/giuse/Documents/GitHub/mna-disclosure"
setwd(PROJECT_ROOT)

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
ARCHIVE_DIR <- file.path("data", paste0("_archive_", timestamp))

# Directories to archive (source → archive subfolder name)
ARCHIVE_MAP <- list(
  "data/interim"    = "data_interim",
  "data/processed"  = "data_processed",
  "data/final"      = "data_final",
  "output"          = "output",
  "reports"         = "reports"
)

# Directories to clean after archival
CLEAN_DIRS <- c(
  "data/interim",
  "data/processed",
  "data/final",
  "output/tables",
  "output/figures",
  "output/logs",
  "reports"
)

# Directories/files to PRESERVE during cleaning (never delete)
PRESERVE <- c(
  "data/interim/submissions_cache"   # EDGAR API cache
)

# Gitkeep files to recreate after cleaning
GITKEEP_DIRS <- c(
  "output/tables",
  "output/figures"
)

cat("\n")
cat(strrep("=", 70), "\n")
cat("ARCHIVE AND CLEAN — SAFE PIPELINE RESET\n")
cat(strrep("=", 70), "\n")
cat(glue("Project root:  {PROJECT_ROOT}"), "\n")
cat(glue("Archive dir:   {ARCHIVE_DIR}"), "\n")
cat(glue("Timestamp:     {timestamp}"), "\n")
cat("\n")

# ==============================================================================
# PHASE 1: PRE-FLIGHT CHECKS
# ==============================================================================

cat(strrep("-", 70), "\n")
cat("PHASE 1: Pre-flight checks\n")
cat(strrep("-", 70), "\n")

# Verify all source directories exist
all_ok <- TRUE
for (src in names(ARCHIVE_MAP)) {
  if (dir_exists(src)) {
    n_files <- length(dir_ls(src, recurse = FALSE, type = "file"))
    cat(glue("  ✓ {src}/ exists ({n_files} top-level files)"), "\n")
  } else {
    cat(glue("  ✗ {src}/ NOT FOUND — skipping"), "\n")
  }
}

# Verify archive directory does not already exist
if (dir_exists(ARCHIVE_DIR)) {
  stop(glue("Archive directory already exists: {ARCHIVE_DIR}\n",
            "This should not happen with timestamped names. Aborting."))
}

# Verify data/raw is intact (sanity check)
n_10k <- length(dir_ls("data/raw/10k_filings", type = "file"))
cat(glue("\n  Safety check: data/raw/10k_filings/ has {n_10k} files"), "\n")
if (n_10k < 100) {
  stop("Unexpectedly few 10-K files. Aborting as safety precaution.")
}
cat("  ✓ data/raw/ looks intact — will NOT be touched\n")

cat("\n")

# ==============================================================================
# PHASE 2: CREATE ARCHIVE
# ==============================================================================

cat(strrep("-", 70), "\n")
cat("PHASE 2: Creating archive\n")
cat(strrep("-", 70), "\n")

dir_create(ARCHIVE_DIR)
cat(glue("  Created: {ARCHIVE_DIR}/"), "\n\n")

archive_manifest <- list()

for (src in names(ARCHIVE_MAP)) {
  dest_name <- ARCHIVE_MAP[[src]]
  dest_path <- file.path(ARCHIVE_DIR, dest_name)
  
  if (!dir_exists(src)) {
    cat(glue("  SKIP {src}/ (does not exist)"), "\n")
    next
  }
  
  # Copy entire directory tree
  dir_copy(src, dest_path)
  
  # Count what was copied
  src_files  <- dir_ls(src, recurse = TRUE, type = "file")
  dest_files <- dir_ls(dest_path, recurse = TRUE, type = "file")
  
  archive_manifest[[src]] <- list(
    source_count = length(src_files),
    archive_count = length(dest_files)
  )
  
  cat(glue("  ✓ {src}/ → {dest_name}/ ({length(src_files)} files)"), "\n")
}

cat("\n")

# ==============================================================================
# PHASE 3: VERIFY ARCHIVE INTEGRITY
# ==============================================================================

cat(strrep("-", 70), "\n")
cat("PHASE 3: Verifying archive integrity\n")
cat(strrep("-", 70), "\n")

verification_passed <- TRUE

for (src in names(archive_manifest)) {
  info <- archive_manifest[[src]]
  if (info$source_count != info$archive_count) {
    cat(glue("  ✗ MISMATCH: {src}/ — source: {info$source_count}, ",
             "archive: {info$archive_count}"), "\n")
    verification_passed <- FALSE
  } else {
    cat(glue("  ✓ {src}/ — {info$source_count} files verified"), "\n")
  }
}

if (!verification_passed) {
  stop("\n  ARCHIVE VERIFICATION FAILED. No files were deleted.\n",
       "  Please inspect the archive manually before proceeding.")
}

cat("\n  ✓ All archives verified — safe to proceed with cleaning\n\n")

# ==============================================================================
# PHASE 4: CLEAN WORKING DIRECTORIES
# ==============================================================================

cat(strrep("-", 70), "\n")
cat("PHASE 4: Cleaning working directories\n")
cat(strrep("-", 70), "\n")

for (clean_dir in CLEAN_DIRS) {
  if (!dir_exists(clean_dir)) {
    cat(glue("  SKIP {clean_dir}/ (does not exist)"), "\n")
    next
  }
  
  # Get all items in the directory
  items <- dir_ls(clean_dir, recurse = FALSE)
  
  deleted_count <- 0
  preserved_count <- 0
  
  for (item in items) {
    # Normalise paths for comparison
    item_norm <- path_norm(item)
    
    # Check if this item should be preserved
    should_preserve <- FALSE
    for (p in PRESERVE) {
      if (startsWith(item_norm, path_norm(p)) || item_norm == path_norm(p)) {
        should_preserve <- TRUE
        break
      }
    }
    
    if (should_preserve) {
      preserved_count <- preserved_count + 1
      next
    }
    
    # Delete the item
    if (is_dir(item)) {
      dir_delete(item)
    } else {
      file_delete(item)
    }
    deleted_count <- deleted_count + 1
  }
  
  cat(glue("  ✓ {clean_dir}/ — deleted {deleted_count} items",
           "{if (preserved_count > 0) paste0(', preserved ', preserved_count) else ''}"),
      "\n")
}

# Recreate .gitkeep files
for (gk_dir in GITKEEP_DIRS) {
  gk_file <- file.path(gk_dir, ".gitkeep")
  if (!file_exists(gk_file)) {
    file_create(gk_file)
  }
}
cat("\n  ✓ .gitkeep files restored\n")

cat("\n")

# ==============================================================================
# PHASE 5: CREATE NEW LOGS DIRECTORY
# ==============================================================================

cat(strrep("-", 70), "\n")
cat("PHASE 5: Creating logs/ directory\n")
cat(strrep("-", 70), "\n")

logs_dir <- "logs"
if (dir_exists(logs_dir)) {
  cat(glue("  logs/ already exists — leaving as-is"), "\n")
} else {
  dir_create(logs_dir)
  cat(glue("  ✓ Created: {logs_dir}/"), "\n")
}

# Create a README for the logs directory
logs_readme <- file.path(logs_dir, "README.md")
if (!file_exists(logs_readme)) {
  writeLines(c(
    "# Pipeline Logs",
    "",
    glue("Created: {Sys.Date()}"),
    "",
    "This directory stores structured logs from each pipeline run.",
    "Each script writes a timestamped log file here with full details",
    "suitable for thesis documentation.",
    "",
    "## Naming convention",
    "",
    "```",
    "STEP_SCRIPTNAME_YYYYMMDD_HHMMSS.log",
    "```",
    "",
    "## Steps",
    "",
    "| Step | Script | Description |",
    "|------|--------|-------------|",
    "| 01   | ingest | SDC data ingestion and cleaning |",
    "| 02   | restrict | Sample restriction cascade |",
    "| 03   | cik_match | Target-to-CIK matching |",
    "| 04   | filing_id | 10-K filing identification |",
    "| 05   | download | 10-K filing download from EDGAR |",
    "| 06   | parse | Section extraction (MD&A, Risk Factors) |",
    "| 07   | clean_text | EDGAR-aware text cleaning |",
    "| 08   | tokenize | Tokenization and DFM construction |",
    "| 09   | indices | Disclosure index construction |",
    "| 10   | validate | KWIC and distributional validation |",
    "| 11   | merge | Final dataset assembly |",
    "| 12   | econ_prep | Econometric variable preparation |",
    "| 13   | econ_run | Regression models |"
  ), logs_readme)
  cat("  ✓ Created logs/README.md\n")
}

cat("\n")

# ==============================================================================
# PHASE 6: FINAL VERIFICATION
# ==============================================================================

cat(strrep("-", 70), "\n")
cat("PHASE 6: Final verification\n")
cat(strrep("-", 70), "\n")

# Check that cleaned directories are empty (except preserved items)
for (clean_dir in CLEAN_DIRS) {
  if (!dir_exists(clean_dir)) next
  remaining <- dir_ls(clean_dir, recurse = FALSE)
  
  # Filter out preserved items
  non_preserved <- remaining
  for (p in PRESERVE) {
    non_preserved <- non_preserved[!startsWith(path_norm(non_preserved), path_norm(p))]
  }
  # Filter out .gitkeep
  non_preserved <- non_preserved[path_file(non_preserved) != ".gitkeep"]
  
  if (length(non_preserved) == 0) {
    cat(glue("  ✓ {clean_dir}/ is clean"), "\n")
  } else {
    cat(glue("  ⚠ {clean_dir}/ has {length(non_preserved)} unexpected items:"), "\n")
    for (np in non_preserved) cat(glue("      {np}"), "\n")
  }
}

# Verify data/raw untouched
n_10k_after <- length(dir_ls("data/raw/10k_filings", type = "file"))
if (n_10k_after == n_10k) {
  cat(glue("\n  ✓ data/raw/10k_filings/ intact ({n_10k_after} files)"), "\n")
} else {
  cat(glue("\n  ✗ WARNING: data/raw/10k_filings/ changed! ",
           "Before: {n_10k}, After: {n_10k_after}"), "\n")
}

# Verify submissions_cache preserved
if (dir_exists("data/interim/submissions_cache")) {
  n_cache <- length(dir_ls("data/interim/submissions_cache", type = "file"))
  cat(glue("  ✓ data/interim/submissions_cache/ preserved ({n_cache} files)"), "\n")
} else {
  cat("  ⚠ data/interim/submissions_cache/ not found\n")
}

# Verify archive exists
n_archive_files <- length(dir_ls(ARCHIVE_DIR, recurse = TRUE, type = "file"))
cat(glue("  ✓ Archive: {ARCHIVE_DIR}/ ({n_archive_files} total files)"), "\n")

# Verify logs directory
cat(glue("  ✓ logs/ directory ready"), "\n")

cat("\n")
cat(strrep("=", 70), "\n")
cat("ARCHIVE AND CLEAN COMPLETE\n")
cat(strrep("=", 70), "\n")
cat(glue("Archive location: {ARCHIVE_DIR}/"), "\n")
cat(glue("Archive size:     {n_archive_files} files"), "\n")
cat("\n")
cat("Preserved:\n")
cat("  • data/raw/                         (source data, 10-K filings)\n")
cat("  • data/interim/submissions_cache/   (EDGAR API cache)\n")
cat("  • src/, config/, scripts/           (code and parameters)\n")
cat("  • logs/                             (new, ready for next run)\n")
cat("\n")
cat("Ready for a fresh pipeline run.\n")
cat(strrep("=", 70), "\n")
