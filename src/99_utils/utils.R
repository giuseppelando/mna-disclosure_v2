# ==============================================================================
# Utility Functions
# Logging, directory management, error handling
# ==============================================================================

library(glue)
library(lubridate)

#' Initialize logging
#'
#' @param log_dir Directory for log files
#' @param script_name Name of the script being run
#' @return Log file path
init_logging <- function(log_dir, script_name) {
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE)
  }
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  log_file <- file.path(log_dir, glue("{script_name}_{timestamp}.log"))
  
  return(log_file)
}

#' Log message to console and file
#'
#' @param msg Message to log
#' @param log_file Path to log file (optional)
#' @param level Log level (INFO, WARNING, ERROR)
log_message <- function(msg, log_file = NULL, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  formatted_msg <- glue("[{timestamp}] [{level}] {msg}")
  
  cat(formatted_msg, "\n")
  
  if (!is.null(log_file)) {
    write(formatted_msg, file = log_file, append = TRUE)
  }
}

#' Log section header
#'
#' @param title Section title
#' @param log_file Path to log file (optional)
log_section <- function(title, log_file = NULL) {
  separator <- strrep("=", 80)
  log_message(separator, log_file, "INFO")
  log_message(title, log_file, "INFO")
  log_message(separator, log_file, "INFO")
}

#' Ensure directory exists
#'
#' @param dir_path Directory path
#' @param log_file Path to log file (optional)
ensure_dir <- function(dir_path, log_file = NULL) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
    log_message(glue("Created directory: {dir_path}"), log_file, "INFO")
  }
}

#' Safe file save with backup
#'
#' @param object Object to save
#' @param file_path File path
#' @param log_file Path to log file (optional)
safe_save <- function(object, file_path, log_file = NULL) {
  # Create backup if file exists
  if (file.exists(file_path)) {
    backup_path <- paste0(file_path, ".backup")
    file.copy(file_path, backup_path, overwrite = TRUE)
    log_message(glue("Created backup: {backup_path}"), log_file, "INFO")
  }
  
  # Save object
  saveRDS(object, file_path)
  file_size_mb <- file.size(file_path) / 1024 / 1024
  log_message(glue("Saved: {file_path} ({round(file_size_mb, 2)} MB)"), log_file, "INFO")
}

#' Progress bar wrapper
#'
#' @param n Total iterations
#' @param label Progress bar label
create_progress <- function(n, label = "Processing") {
  pb <- txtProgressBar(min = 0, max = n, style = 3)
  pb_label <- label
  
  list(
    pb = pb,
    update = function(i) {
      setTxtProgressBar(pb, i)
    },
    close = function() {
      close(pb)
      cat("\n")
    }
  )
}

#' Retry wrapper for functions
#'
#' @param expr Expression to evaluate
#' @param max_retries Maximum number of retries
#' @param delay_seconds Delay between retries
#' @param log_file Path to log file (optional)
retry_on_error <- function(expr, max_retries = 3, delay_seconds = 2, log_file = NULL) {
  for (attempt in 1:max_retries) {
    result <- tryCatch(
      expr,
      error = function(e) {
        if (attempt < max_retries) {
          log_message(
            glue("Attempt {attempt} failed: {e$message}. Retrying in {delay_seconds}s..."),
            log_file,
            "WARNING"
          )
          Sys.sleep(delay_seconds)
          return(NULL)
        } else {
          log_message(
            glue("All {max_retries} attempts failed: {e$message}"),
            log_file,
            "ERROR"
          )
          return(NULL)
        }
      }
    )
    
    if (!is.null(result)) {
      return(result)
    }
  }
  
  return(NULL)
}

#' Rate limiter for API calls
#'
#' @param calls_per_second Maximum calls per second
create_rate_limiter <- function(calls_per_second = 10) {
  last_call_time <- Sys.time()
  min_interval <- 1 / calls_per_second
  
  function() {
    elapsed <- as.numeric(difftime(Sys.time(), last_call_time, units = "secs"))
    if (elapsed < min_interval) {
      Sys.sleep(min_interval - elapsed)
    }
    last_call_time <<- Sys.time()
  }
}
