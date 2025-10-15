#' Scan Directory for Non-ASCII Characters
#'
#' This function scans all files in a specified directory for non-ASCII characters
#' using tools::showNonASCIIfile. By default, it looks in the R/ directory.
#'
#' @param dir Character string specifying the directory to scan (default: "R/")
#' @param pattern Character string containing a regular expression for filtering files (default: "\\.R$")
#' @param recursive Logical indicating whether to scan subdirectories (default: TRUE)
#' @param verbose Logical indicating whether to print additional information (default: TRUE)
#'
#' @return A list containing files with non-ASCII characters and their locations
#'
#' @examples
#' \dontrun{
#' asciiScan()
#' asciiScan("src/", pattern = "\\.cpp$")
#' }
#'
#' @export
asciiScan <- function(dir = "R/",
                          pattern = "(\\.R|\\.r)$",
                          recursive = TRUE,
                          verbose = TRUE) {
# Input validation
  if (!dir.exists(dir)) {
    stop("Directory '", dir, "' does not exist")
  }

  # Get list of files
  files <- list.files(
    path = dir,
    pattern = pattern,
    recursive = recursive,
    full.names = TRUE
  )

  if (length(files) == 0) {
    warning("No files found matching pattern '", pattern, "' in directory '", dir, "'")
    return(invisible(NULL))
  }

  # Initialize results list
  results <- list()
  files_with_non_ascii <- character(0)

  # Process each file
  for (file in files) {
    if (verbose) {
      cat("Scanning:", file, "\n")
    }

    # Capture output from showNonASCIIfile
    output <- capture.output({
      has_non_ascii <- tools::showNonASCIIfile(file)
    })

    # Remove empty strings from output
    output <- output[nzchar(output)]

    # If non-ASCII characters were found (checking actual output content)
    if (length(output) > 0) {
      files_with_non_ascii <- c(files_with_non_ascii, file)
      results[[file]] <- output
      
      if (verbose) {
        cat("Found non-ASCII characters in:", file, "\n")
        cat(paste(output, collapse = "\n"), "\n\n")
      }
    }
  }

  # Prepare return value
  result_summary <- list(
    files_checked = files,
    files_with_non_ascii = files_with_non_ascii,
    detailed_results = results,
    summary = list(
      total_files = length(files),
      files_with_issues = length(files_with_non_ascii)
    )
  )

  # Set the class for custom printing
  class(result_summary) <- "asciiScan_results"

  # Print summary if verbose
  if (verbose) {
    cat("\nSummary:\n")
    cat("Total files checked:", result_summary$summary$total_files, "\n")
    cat("Files with non-ASCII characters:", result_summary$summary$files_with_issues, "\n")
  }

  invisible(result_summary)
}

#' Print method for asciiScan results
#'
#' @param x Result object from asciiScan
#' @param ... Additional arguments passed to print
#'
#' @export
print.asciiScan_results <- function(x, ...) {
  cat("Non-ASCII Scan Results\n")
  cat("=====================\n")
  cat("Total files checked:", x$summary$total_files, "\n")
  cat("Files with non-ASCII characters:", x$summary$files_with_issues, "\n\n")
  
  if (x$summary$files_with_issues > 0) {
    cat("Files containing non-ASCII characters:\n")
    for (file in x$files_with_non_ascii) {
      cat("- ", file, "\n")
      cat(paste("  ", x$detailed_results[[file]], collapse = "\n"), "\n")
    }
  }
}