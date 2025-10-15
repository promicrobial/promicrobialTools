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
