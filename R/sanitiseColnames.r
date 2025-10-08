#' Sanitize Data Frame Column Names
#'
#' @description
#' Cleans and standardizes data frame column names by removing problematic
#' characters, normalizing separators, and converting to a consistent format
#' suitable for programmatic use and rendering in HTML/PDF documents.
#'
#' @param df A data frame or matrix whose column names should be sanitized.
#'   Must have named columns (colnames must not be NULL).
#' @param preserve_case Logical. If TRUE, preserves original case. If FALSE 
#'   (default), converts all names to lowercase.
#' @param separator Character string to use as separator. Default is "_" 
#'   (underscore). Common alternatives include "." or "-".
#' @param remove_leading_x Logical. If TRUE (default), removes leading "X" 
#'   characters that R adds to numeric column names during import.
#' @param max_length Integer. Maximum length for column names. Names longer 
#'   than this will be truncated. Default is NULL (no limit).
#'
#' @return A data frame or matrix with sanitized column names. The structure
#'   and data content remain unchanged, only column names are modified.
#'
#' @details
#' The sanitization process applies the following transformations in order:
#' \enumerate{
#'   \item Remove leading "X" characters (if remove_leading_x = TRUE)
#'   \item Remove leading dots/periods
#'   \item Replace multiple consecutive dots with separator
#'   \item Replace whitespace (spaces, tabs, newlines) with separator
#'   \item Replace multiple consecutive separators with single separator
#'   \item Trim whitespace from both ends
#'   \item Convert to lowercase (if preserve_case = FALSE)
#'   \item Remove leading/trailing separators
#'   \item Truncate to max_length (if specified)
#'   \item Ensure uniqueness by adding numeric suffixes if needed
#' }
#'
#' Common use cases include:
#' \itemize{
#'   \item Cleaning imported CSV/Excel data with messy headers
#'   \item Preparing data for database storage
#'   \item Standardizing column names for programmatic access
#'   \item Ensuring compatibility with R's naming conventions
#' }
#'
#' @examples
#' # Basic usage with messy column names
#' df <- data.frame(
#'   `X.Index` = 1:3,
#'   `..Sample Name..` = letters[1:3],
#'   `Patient ID (Primary)` = LETTERS[1:3],
#'   `  Final Score  ` = c(85, 92, 78)
#' )
#' colnames(df)
#' sanitiseColnames(df)
#' # Returns: x1, sample_name, patient_id_primary, final_score
#'
#' # Preserve case and use different separator
#' sanitiseColnames(df, preserve_case = TRUE, separator = ".")
#' # Returns: X1, Sample.Name, Patient.ID.Primary, Final.Score
#'
#' # Set maximum length
#' sanitiseColnames(df, max_length = 10)
#' # Returns: x1, sample_nam, patient_id, final_scor
#'
#' # Real-world example with imported data
#' messy_data <- data.frame(
#'   X1 = 1:2,
#'   X2023.Jan.Sales = c(100, 200),
#'   `Very Long Column Name With Spaces` = c("A", "B"),
#'   `duplicate name` = c(1, 2),
#'   `duplicate name` = c(3, 4),
#'   check.names = FALSE
#' )
#' clean_data <- sanitiseColnames(messy_data)
#' colnames(clean_data)
#' # Returns: "x1", "x2023_jan_sales", "very_long_column_name_with_spaces", 
#' #          "duplicate_name", "duplicate_name_1"
#'
#' @seealso 
#' \code{\link{make.names}}, \code{\link{janitor::clean_names}}
#'
#' @importFrom stringr str_remove str_replace_all str_trim
#' @export
sanitiseColnames <- function(df, 
                             preserve_case = FALSE,
                             separator = "_",
                             remove_leading_x = TRUE,
                             max_length = NULL) {
  
  # Input validation
  if (missing(df)) {
    stop("Argument 'df' is missing with no default")
  }
  
  if (!is.data.frame(df) && !is.matrix(df)) {
    stop("'df' must be a data frame or matrix")
  }
  
  if (is.null(colnames(df))) {
    stop("'df' must have column names (colnames cannot be NULL)")
  }
  
  if (ncol(df) == 0) {
    warning("'df' has no columns to sanitize")
    return(df)
  }
  
  if (!is.logical(preserve_case) || length(preserve_case) != 1) {
    stop("'preserve_case' must be a single logical value (TRUE or FALSE)")
  }
  
  if (!is.character(separator) || length(separator) != 1) {
    stop("'separator' must be a single character string")
  }
  
  if (nchar(separator) == 0) {
    stop("'separator' cannot be an empty string")
  }
  
  if (!is.logical(remove_leading_x) || length(remove_leading_x) != 1) {
    stop("'remove_leading_x' must be a single logical value (TRUE or FALSE)")
  }
  
  if (!is.null(max_length)) {
    if (!is.numeric(max_length) || length(max_length) != 1 || max_length < 1) {
      stop("'max_length' must be NULL or a positive integer")
    }
    max_length <- as.integer(max_length)
  }
  
  # Check for required packages
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("Package 'stringr' is required but not installed. Please install it with: install.packages('stringr')")
  }
  
  # Get original column names
  cols <- colnames(df)
  original_cols <- cols  # Store for potential warning messages
  
  # Check if any names are empty or NA
  if (any(is.na(cols) | cols == "")) {
    empty_indices <- which(is.na(cols) | cols == "")
    warning("Found ", length(empty_indices), " empty or NA column names. ",
            "These will be replaced with 'col_N' where N is the column number.")
    cols[empty_indices] <- paste0("col_", empty_indices)
  }
  
  # Apply sanitization steps
  if (remove_leading_x) {
    cols <- stringr::str_remove(cols, "^X+")
  }
  
  # Remove leading dots/periods
  cols <- stringr::str_remove(cols, "^\\.+")
  
  # Replace multiple dots with separator
  cols <- stringr::str_replace_all(cols, "\\.+", separator)
  
  # Replace whitespace with separator
  cols <- stringr::str_replace_all(cols, "\\s+", separator)
  
  # Replace multiple separators with single separator
  separator_pattern <- paste0("\\", separator, "+")
  cols <- stringr::str_replace_all(cols, separator_pattern, separator)
  
  # Trim whitespace
  cols <- stringr::str_trim(cols, "both")
  
  # Convert case
  if (!preserve_case) {
    cols <- tolower(cols)
  }
  
  # Remove leading/trailing separators
  separator_clean <- paste0("^\\", separator, "+|\\", separator, "+$")
  cols <- stringr::str_remove_all(cols, separator_clean)
  
  # Handle empty strings after cleaning
  empty_after_clean <- which(cols == "" | is.na(cols))
  if (length(empty_after_clean) > 0) {
    cols[empty_after_clean] <- paste0("col", separator, empty_after_clean)
    warning("Some column names became empty after sanitization and were replaced with 'col", 
            separator, "N' format")
  }
  
  # Apply length limit
  if (!is.null(max_length)) {
    long_names <- nchar(cols) > max_length
    if (any(long_names)) {
      cols[long_names] <- substr(cols[long_names], 1, max_length)
      warning("Truncated ", sum(long_names), " column names to ", max_length, " characters")
    }
  }
  
  # Ensure uniqueness
  if (any(duplicated(cols))) {
    cols <- make.unique(cols, sep = separator)
    warning("Found duplicate column names after sanitization. Added numeric suffixes to ensure uniqueness.")
  }
  
  # Validate final names are syntactically valid (optional check)
  invalid_names <- !make.names(cols) == cols
  if (any(invalid_names) && !preserve_case) {
    warning("Some sanitized names may not be syntactically valid R names: ",
            paste(cols[invalid_names], collapse = ", "))
  }
  
  # Apply new column names
  colnames(df) <- cols
  
  # Add attributes for documentation
  attr(df, "sanitized_colnames") <- TRUE
  attr(df, "original_colnames") <- original_cols
  attr(df, "sanitization_info") <- list(
    preserve_case = preserve_case,
    separator = separator,
    remove_leading_x = remove_leading_x,
    max_length = max_length,
    n_duplicates_resolved = sum(duplicated(original_cols)),
    n_truncated = if (!is.null(max_length)) sum(nchar(original_cols) > max_length) else 0
  )
  
  return(df)
}

#' Print Method for Data Frames with Sanitized Column Names
#'
#' @description
#' Custom print method that shows information about column name sanitization
#' when printing data frames that have been processed by \code{\link{sanitiseColnames}}.
#'
#' @param x A data frame with sanitized column names
#' @param show_original Logical. If TRUE, shows a mapping of original to new names
#' @param ... Additional arguments passed to the default print method
#'
#' @return Invisibly returns the input data frame
#' @export
print.sanitized_colnames <- function(x, show_original = FALSE, ...) {
  
  if (show_original && !is.null(attr(x, "original_colnames"))) {
    cat("Data frame with sanitized column names:\n")
    original <- attr(x, "original_colnames")
    current <- colnames(x)
    
    # Show mapping for changed names only
    changed <- original != current
    if (any(changed)) {
      cat("Column name changes:\n")
      for (i in which(changed)) {
        cat(sprintf("  '%s' -> '%s'\n", original[i], current[i]))
      }
      cat("\n")
    }
    
    # Show sanitization info
    info <- attr(x, "sanitization_info")
    if (!is.null(info)) {
      cat("Sanitization settings:\n")
      cat(sprintf("  - Preserve case: %s\n", info$preserve_case))
      cat(sprintf("  - Separator: '%s'\n", info$separator))
      cat(sprintf("  - Remove leading X: %s\n", info$remove_leading_x))
      if (!is.null(info$max_length)) {
        cat(sprintf("  - Max length: %d\n", info$max_length))
      }
      cat("\n")
    }
  }
  
  # Remove custom class to use default print method
  class(x) <- setdiff(class(x), "sanitized_colnames")
  NextMethod()
}

#' Get Column Name Mapping
#'
#' @description
#' Retrieves the mapping between original and sanitized column names
#' from a data frame processed by \code{\link{sanitiseColnames}}.
#'
#' @param df A data frame processed by \code{\link{sanitiseColnames}}
#' @param as_dataframe Logical. If TRUE, returns a data frame. If FALSE, returns a named vector.
#'
#' @return A data frame or named vector showing the mapping from original to sanitized names
#' @export
#'
#' @examples
#' df <- data.frame(`Bad Name!` = 1:3, `Another Bad` = 4:6, check.names = FALSE)
#' clean_df <- sanitiseColnames(df)
#' get_colname_mapping(clean_df)
get_colname_mapping <- function(df, as_dataframe = TRUE) {
  
  if (!inherits(df, "data.frame") && !inherits(df, "matrix")) {
    stop("'df' must be a data frame or matrix")
  }
  
  original <- attr(df, "original_colnames")
  if (is.null(original)) {
    warning("No original column names found. Data frame may not have been processed by sanitiseColnames()")
    return(NULL)
  }
  
  current <- colnames(df)
  
  if (as_dataframe) {
    return(data.frame(
      original = original,
      sanitized = current,
      changed = original != current,
      stringsAsFactors = FALSE
    ))
  } else {
    mapping <- setNames(current, original)
    return(mapping)
  }
}

#' @rdname sanitiseColnames
#' @export
sanitizeColnames <- sanitiseColnames