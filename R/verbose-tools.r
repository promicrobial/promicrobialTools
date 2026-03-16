#' Create a Progress Counter
#'
#' @description
#' Creates a progress counter that can be used in loops to track progress.
#' Provides both percentage and count updates with customizable display options.
#'
#' @param total Integer. Total number of iterations expected
#' @param title Character. Title for the progress messages (default: "Progress")
#' @param show_pct Logical. Whether to show percentage (default: TRUE)
#' @param show_bar Logical. Whether to show progress bar (default: TRUE)
#' @param width Integer. Width of progress bar (default: 50)
#' @param update_freq Integer. Update frequency in iterations (default: 1)
#'
#' @return A function that can be called to update progress
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' counter <- make_progress_counter(total = 100)
#' for(i in 1:100) {
#'   counter()
#'   Sys.sleep(0.1)  # Some operation
#' }
#'
#' # Custom configuration
#' counter <- make_progress_counter(
#'   total = 50,
#'   title = "Processing files",
#'   show_pct = TRUE,
#'   show_bar = TRUE,
#'   width = 40,
#'   update_freq = 5
#' )
#' }
#'
#' @export
make_progress_counter <- function(total,
                                title = "Progress",
                                show_pct = TRUE,
                                show_bar = TRUE,
                                width = 50,
                                update_freq = 1) {
  
  # Validate inputs
  stopifnot(
    is.numeric(total) && total > 0,
    is.character(title),
    is.logical(show_pct),
    is.logical(show_bar),
    is.numeric(width) && width > 0,
    is.numeric(update_freq) && update_freq > 0
  )
  
  # Initialize counter
  count <- 0
  start_time <- Sys.time()
  last_update <- start_time
  
  # Create progress bar characters
  bar_chars <- list(
    done = "\U0002590",
    remaining = "\U0002591",
    left = "[",
    right = "]"
  )
  
  # Function to format time
  format_time <- function(seconds) {
    seconds <- floor(seconds) # Ensure we're working with whole seconds
    if (seconds < 60) {
      return(sprintf("%.1fs", seconds))
    } else if (seconds < 3600) {
      mins <- floor(seconds / 60)
      secs <- floor(seconds %% 60)
      return(sprintf("%dm %ds", mins, secs))
    } else {
      hours <- floor(seconds / 3600)
      mins <- floor((seconds %% 3600) / 60)
      return(sprintf("%dh %dm", hours, mins))
    }
  }  
  # Create the counter function
  function(current = NULL, force_update = FALSE) {
    # Auto-increment if no value provided
    if (is.null(current)) {
      count <<- count + 1
      current <- count
    }
    
    # Check if update is needed
    current_time <- Sys.time()
    time_diff <- as.numeric(current_time - last_update, units = "secs")
    
    if (force_update || current == total || current == 1 || 
        current %% update_freq == 0 || time_diff >= 1) {
      
      # Calculate progress
      pct <- current / total * 100
      elapsed <- as.numeric(current_time - start_time, units = "secs")
      
      # Estimate remaining time
      rate <- current / elapsed
      remaining <- (total - current) / rate
      
      # Create progress bar if needed
      if (show_bar) {
        filled <- round(width * current / total)
        bar <- paste0(
          bar_chars$left,
          strrep(bar_chars$done, filled),
          strrep(bar_chars$remaining, width - filled),
          bar_chars$right
        )
      }
      
      # Build message components
      msg_parts <- c()
      
      # Add title
      msg_parts <- c(msg_parts, sprintf("\r%s: ", title))
      
      # Add counts
      msg_parts <- c(msg_parts, sprintf("%d/%d", current, total))
      
      # Add percentage if requested
      if (show_pct) {
        msg_parts <- c(msg_parts, sprintf(" (%.1f%%)", pct))
      }
      
      # Add progress bar if requested
      if (show_bar) {
        msg_parts <- c(msg_parts, sprintf(" %s", bar))
      }
      
      # Add time information
      msg_parts <- c(
        msg_parts,
        sprintf(" [%s elapsed, ~%s remaining]",
                format_time(elapsed),
                format_time(remaining))
      )
      
      # Print message
      cat(paste(msg_parts, collapse = ""))
      
      # Add newline if complete
      if (current >= total) cat("\n")
      
      # Update last update time
      last_update <<- current_time
    }
    
    # Return invisibly
    invisible(current)
  }
}

#' Print Messages Conditionally Based on Verbose Setting
#'
#' This function prints messages to the console only when verbose mode is enabled.
#' It provides a convenient way to add optional diagnostic or progress messages
#' to scripts that can be toggled on/off based on user preference.
#'
#' @param message Character string or any object that can be converted to character.
#'   The message to be printed. If multiple arguments are provided, they will be
#'   concatenated with spaces.
#' @param verbose Logical. If TRUE, the message will be printed. If FALSE,
#'   nothing will be printed. Default is TRUE.
#' @param prefix Character string. Optional prefix to add before the message.
#'   Useful for categorizing messages (e.g., "INFO:", "WARNING:", "DEBUG:").
#'   Default is NULL (no prefix).
#' @param timestamp Logical. If TRUE, adds a timestamp prefix to the message.
#'   Default is FALSE.
#' @param file Connection or character string naming the file to print to.
#'   Default is "" (stdout). Can be used to redirect verbose output to a log file.
#' @param ... Additional arguments passed to the message. These will be
#'   concatenated with spaces to form the complete message.
#'
#' @return Invisibly returns the formatted message string (whether printed or not).
#'   This allows the function to be used in pipes or for further processing.
#'
#' @details
#' The function is designed to be a drop-in replacement for print() or cat()
#' statements in scripts where you want conditional output. When verbose=FALSE,
#' the function does nothing, making it efficient for production code.
#'
#' The timestamp format uses the system's current time in "YYYY-MM-DD HH:MM:SS"
#' format when timestamp=TRUE.
#'
#' @examples
#' # Basic usage
#' verbose_print("Starting analysis", verbose = TRUE)
#' verbose_print("This won't print", verbose = FALSE)
#'
#' # With prefix
#' verbose_print("File loaded successfully", verbose = TRUE, prefix = "INFO:")
#' verbose_print("Potential issue detected", verbose = TRUE, prefix = "WARNING:")
#'
#' # With timestamp
#' verbose_print("Process started", verbose = TRUE, timestamp = TRUE)
#'
#' # Multiple arguments
#' n_samples <- 100
#' verbose_print("Processing", n_samples, "samples", verbose = TRUE)
#'
#' # Use in a function
#' my_analysis <- function(data, verbose = FALSE) {
#'   verbose_print("Starting analysis of", nrow(data), "rows", verbose = verbose)
#'   # ... analysis code ...
#'   verbose_print("Analysis complete", verbose = verbose, prefix = "SUCCESS:")
#' }
#'
#' # Redirect to file
#' log_file <- file("analysis.log", "w")
#' verbose_print("Logging to file", verbose = TRUE, file = log_file)
#' close(log_file)
#'
#' @export
verbose_print <- function(
  message,
  ...,
  verbose = TRUE,
  prefix = NULL,
  timestamp = FALSE,
  file = ""
) {
  # If verbose is FALSE, return invisibly without printing
  if (!verbose) {
    return(invisible(paste(message, ...)))
  }

  # Combine message with additional arguments
  full_message <- paste(message, ...)

  # Add timestamp if requested
  if (timestamp) {
    time_stamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    full_message <- paste0("[", time_stamp, "] ", full_message)
  }

  # Add prefix if provided
  if (!is.null(prefix)) {
    full_message <- paste(prefix, full_message)
  }

  # Print the message
  cat(full_message, "\n", file = file)

  # Return the formatted message invisibly
  invisible(full_message)
}