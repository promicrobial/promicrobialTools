#' Validate Data Frame Column Names
#'
#' This function performs comprehensive validation of data frame column names
#' against various naming conventions and requirements. It checks for uniqueness,
#' empty names, valid R naming conventions, length constraints, and special characters.
#'
#' @param df A data frame whose column names should be validated
#' @param requirements A named list of validation requirements. Currently supports:
#'   \itemize{
#'     \item \code{max_length}: Integer. Maximum allowed length for column names
#'     \item \code{min_length}: Integer. Minimum required length for column names (default: 1)
#'     \item \code{allow_dots}: Logical. Whether to allow dots in column names (default: TRUE)
#'     \item \code{case_sensitive}: Logical. Whether duplicate checking should be case sensitive (default: TRUE)
#'   }
#'
#' @return A list containing validation results with the following components:
#'   \itemize{
#'     \item \code{all_unique}: Logical. TRUE if all column names are unique
#'     \item \code{no_empty}: Logical. TRUE if no column names are empty or NA
#'     \item \code{valid_r_names}: Logical. TRUE if all names follow R naming conventions
#'     \item \code{max_length_ok}: Logical. TRUE if all names meet maximum length requirement
#'     \item \code{min_length_ok}: Logical. TRUE if all names meet minimum length requirement
#'     \item \code{no_special_chars}: Logical. TRUE if names contain only allowed characters
#'     \item \code{summary}: Character. Overall validation status ("PASS" or "FAIL")
#'     \item \code{issues}: Character vector. Detailed descriptions of any validation failures
#'     \item \code{problematic_names}: Character vector. Names that failed validation
#'   }
#'
#' @examples
#' # Create test data with problematic column names
#' test_df <- data.frame(
#'   `Bad Name!` = 1:3,
#'   `Another Bad` = 4:6,
#'   `duplicate` = 7:9,
#'   `duplicate` = 10:12,
#'   check.names = FALSE
#' )
#'
#' # Validate with default settings
#' results <- validateColnames(test_df)
#' print(results$summary)
#'
#' # Validate with custom requirements
#' results <- validateColnames(test_df, list(max_length = 10, min_length = 3))
#' print(results$issues)
#'
#' @export
validateColnames <- function(df, requirements = list()) {
  # Input validation
  if (!is.data.frame(df)) {
    stop("Input 'df' must be a data frame")
  }
  
  if (!is.list(requirements)) {
    stop("Input 'requirements' must be a list")
  }
  
  # Set default requirements
  requirements <- modifyList(
    list(
      max_length = NULL,
      min_length = 1,
      allow_dots = TRUE,
      case_sensitive = TRUE
    ),
    requirements
  )
  
  # Validate requirements
  if (!is.null(requirements$max_length) && 
      (!is.numeric(requirements$max_length) || requirements$max_length < 1)) {
    stop("max_length must be a positive integer")
  }
  
  if (!is.numeric(requirements$min_length) || requirements$min_length < 1) {
    stop("min_length must be a positive integer")
  }
  
  cols <- colnames(df)
  issues <- character(0)
  problematic_names <- character(0)
  
  # Check for uniqueness
  cols_for_duplicate_check <- if (requirements$case_sensitive) cols else tolower(cols)
  all_unique <- !any(duplicated(cols_for_duplicate_check))
  if (!all_unique) {
    duplicate_names <- cols[duplicated(cols_for_duplicate_check) | 
                           duplicated(cols_for_duplicate_check, fromLast = TRUE)]
    issues <- c(issues, sprintf("Duplicate column names found: %s", 
                               paste(unique(duplicate_names), collapse = ", ")))
    problematic_names <- c(problematic_names, duplicate_names)
  }
  
  # Check for empty or NA names
  empty_or_na <- cols == "" | is.na(cols) | is.null(cols)
  no_empty <- !any(empty_or_na)
  if (!no_empty) {
    empty_count <- sum(empty_or_na)
    issues <- c(issues, sprintf("%d empty or NA column name(s) found", empty_count))
    problematic_names <- c(problematic_names, cols[empty_or_na])
  }
  
  # Check for valid R names
  valid_r_names <- all(make.names(cols) == cols)
  if (!valid_r_names) {
    invalid_names <- cols[make.names(cols) != cols]
    issues <- c(issues, sprintf("Invalid R names: %s", 
                               paste(invalid_names, collapse = ", ")))
    problematic_names <- c(problematic_names, invalid_names)
  }
  
  # Check maximum length
  max_length_ok <- if (!is.null(requirements$max_length)) {
    too_long <- nchar(cols) > requirements$max_length
    if (any(too_long)) {
      long_names <- cols[too_long]
      issues <- c(issues, sprintf("Names exceeding max length (%d): %s", 
                                 requirements$max_length,
                                 paste(long_names, collapse = ", ")))
      problematic_names <- c(problematic_names, long_names)
    }
    !any(too_long)
  } else {
    TRUE
  }
  
  # Check minimum length
  too_short <- nchar(cols) < requirements$min_length
  min_length_ok <- !any(too_short)
  if (!min_length_ok) {
    short_names <- cols[too_short]
    issues <- c(issues, sprintf("Names below min length (%d): %s", 
                               requirements$min_length,
                               paste(short_names, collapse = ", ")))
    problematic_names <- c(problematic_names, short_names)
  }
  
  # Check for special characters
  pattern <- if (requirements$allow_dots) {
    "^[a-zA-Z][a-zA-Z0-9_.]*$"
  } else {
    "^[a-zA-Z][a-zA-Z0-9_]*$"
  }
  
  has_special <- !grepl(pattern, cols)
  no_special_chars <- !any(has_special)
  if (!no_special_chars) {
    special_names <- cols[has_special]
    issues <- c(issues, sprintf("Names with invalid characters: %s", 
                               paste(special_names, collapse = ", ")))
    problematic_names <- c(problematic_names, special_names)
  }
  
  # Overall summary
  all_checks_passed <- all_unique && no_empty && valid_r_names && 
                      max_length_ok && min_length_ok && no_special_chars
  
  results <- list(
    all_unique = all_unique,
    no_empty = no_empty,
    valid_r_names = valid_r_names,
    max_length_ok = max_length_ok,
    min_length_ok = min_length_ok,
    no_special_chars = no_special_chars,
    summary = ifelse(all_checks_passed, "PASS", "FAIL"),
    issues = if (length(issues) > 0) issues else "No issues found",
    problematic_names = unique(problematic_names)
  )
  
  # Add class for custom printing
  class(results) <- c("colname_validation", "list")
  
  return(results)
}

#' Print Method for Column Name Validation Results
#'
#' @param x A colname_validation object
#' @param ... Additional arguments (unused)
#' @export
print.colname_validation <- function(x, ...) {
  cat("## Column Name Validation Results\n")
  cat("Overall Status:", x$summary, "\n\n")
  
  checks <- c("all_unique", "no_empty", "valid_r_names", 
              "max_length_ok", "min_length_ok", "no_special_chars")
  
  check_labels <- c(
    "All names unique" = "all_unique",
    "No empty names" = "no_empty", 
    "Valid R names" = "valid_r_names",
    "Maximum length OK" = "max_length_ok",
    "Minimum length OK" = "min_length_ok",
    "No special characters" = "no_special_chars"
  )
  
  cat("Individual Checks:\n")
  for (i in seq_along(check_labels)) {
    label <- names(check_labels)[i]
    check <- check_labels[i]
    status <- ifelse(x[[check]], "\U0002714 PASS", "\U0002716 FAIL")
    cat(sprintf("  %-25s %s\n", paste0(label, ":"), status))
  }
  
  if (x$summary == "FAIL") {
    cat("\nIssues Found:\n")
    for (issue in x$issues) {
      cat("  \U0002022", issue, "\n")
    }
  }
}

#' Generate Quarto-Compatible Validation Report
#'
#' @param validation_results A colname_validation object
#' @param title Character string for the report title
#' @return A character string containing markdown-formatted validation results
#' @export
generate_validation_report <- function(validation_results, title = "Column Name Validation Report") {
  if (!inherits(validation_results, "colname_validation")) {
    stop("Input must be a colname_validation object")
  }
  
  # Start building the report
  report <- character()
  
  # Title
  report <- c(report, paste0("## ", title))
  report <- c(report, "")
  
  # Overall status
  status_badge <- ifelse(validation_results$summary == "PASS", 
                        "\U0001F7E2 **PASS**", 
                        "\U0001F534 **FAIL**")
  report <- c(report, paste0("**Overall Status:** ", status_badge))
  report <- c(report, "")
  
  # Results table - build manually to avoid knitr issues
  report <- c(report, "### Validation Results")
  report <- c(report, "")
  report <- c(report, "| Check | Status |")
  report <- c(report, "|:------|:-------|")
  
  # Add each check result
  checks_data <- list(
    "All names unique" = validation_results$all_unique,
    "No empty names" = validation_results$no_empty,
    "Valid R names" = validation_results$valid_r_names,
    "Maximum length OK" = validation_results$max_length_ok,
    "Minimum length OK" = validation_results$min_length_ok,
    "No special characters" = validation_results$no_special_chars
  )
  
  for (check_name in names(checks_data)) {
    status_icon <- ifelse(checks_data[[check_name]], "\U0001F7E2 PASS", "\U0001F534 FAIL")
    report <- c(report, paste0("| ", check_name, " | ", status_icon, " |"))
  }
  
  report <- c(report, "")
  
  # Issues section (only if there are failures)
  if (validation_results$summary == "FAIL" && length(validation_results$issues) > 0) {
    report <- c(report, "### Issues Found")
    report <- c(report, "")
    
    # Only show actual issues, not the "No issues found" message
    actual_issues <- validation_results$issues[validation_results$issues != "No issues found"]
    
    if (length(actual_issues) > 0) {
      for (issue in actual_issues) {
        report <- c(report, paste0("- ", issue))
      }
      report <- c(report, "")
    }
    
    # Add problematic names if any
    if (length(validation_results$problematic_names) > 0) {
      report <- c(report, "**Problematic column names:**")
      report <- c(report, "")
      problematic_list <- paste0("- `", validation_results$problematic_names, "`")
      report <- c(report, problematic_list)
      report <- c(report, "")
    }
  }
  
  # Join all parts with newlines
  return(paste(report, collapse = "\n"))
}