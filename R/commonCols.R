#' Find Common Columns Across Multiple Data Frames with Flexible Thresholds
#'
#' @description
#' Identifies columns that are common across all data frames or a subset of them,
#' with enhanced debugging and flexible matching options.
#'
#' @param dfList A list of data frames to compare
#' @param case_sensitive Logical, whether to treat column names as case sensitive (default: TRUE)
#' @param partial_match Logical, whether to allow partial matching of column names (default: FALSE)
#' @param debug Logical, whether to print debug information (default: FALSE)
#' @param trim Logical, whether to trim whitespace from column names (default: TRUE)
#' @param clean_names Logical, whether to clean special characters from names (default: FALSE)
#' @param threshold Numeric, proportion (0-1) of data frames a column must appear in to be considered common (default: 1)
#'
#' @return A list containing:
#'   - common: Vector of common column names
#'   - summary: Data frame showing presence/absence of columns across data frames
#'   - debug_info: List of debugging information (if debug=TRUE)
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' commonCols(dfList)
#'
#' # With debugging
#' commonCols(dfList, debug = TRUE)
#' }
#' @export
commonCols <- function(dfList, 
                      case_sensitive = TRUE, 
                      partial_match = FALSE,
                      debug = FALSE,
                      trim = TRUE,
                      clean_names = FALSE,
                      threshold = 2) {
  
  # Input validation
  stopifnot(is.list(dfList), length(dfList) > 0, all(sapply(dfList, is.data.frame)))

  if (threshold < 0 || threshold > min(unlist(lapply(dfList, ncol)))) {
    stop(paste("Threshold must be a numeric value between 0 and", min(unlist(lapply(dfList, ncol)))))
  }
  
  # Initialize debug information
  debug_info <- list()
  
  # Get names of the list elements (use generic names if not named)
  df_names <- names(dfList)
  if (is.null(df_names)) {
    df_names <- paste0("DF", seq_along(dfList))
  }
  
  # Function to sanitize and process column names
  process_colnames <- function(cols) {
    if (!case_sensitive) cols <- tolower(cols)
    if (trim) cols <- trimws(cols)
    if (clean_names) {
      cols <- gsub("^X|\\.+$", "", cols)  # Remove leading 'X' and trailing dots
      cols <- gsub("\\s+|\\.+", "_", cols)  # Replace spaces and consecutive dots with underscores
    }
    cols
  }
  
  # Apply column name processing
  colnames_list <- lapply(dfList, function(df) process_colnames(colnames(df)))
  names(colnames_list) <- df_names
  
  if (debug) {
    debug_info$processed_colnames <- colnames_list
    debug_info$cleaning_params <- list(
      case_sensitive = case_sensitive,
      partial_match = partial_match,
      trim = trim,
      clean_names = clean_names,
      threshold = threshold
    )
  }
  
  # Create presence/absence matrix
  all_cols <- unique(unlist(colnames_list))
  presence_matrix <- matrix(
    FALSE, 
    nrow = length(all_cols), 
    ncol = length(dfList),
    dimnames = list(all_cols, df_names)
  )
  
  # Fill presence matrix with partial/exact matches
  for (i in seq_along(dfList)) {
    if (partial_match) {
      # For partial matching, check if column names contain each other
      for (col in all_cols) {
        presence_matrix[col, i] <- any(sapply(colnames_list[[i]], 
                                            function(x) grepl(col, x) || grepl(x, col)))
      }
    } else {
      # For exact matching
      presence_matrix[all_cols %in% colnames_list[[i]], i] <- TRUE
    }
  }
  
  common_cols <- rownames(presence_matrix)[rowSums(presence_matrix) >= threshold]
  
  if (debug) {
    debug_info$presence_matrix <- presence_matrix
    debug_info$matching_method <- if(partial_match) "partial" else "exact"
  }
  
  # Create summary data frame
  summary_df <- data.frame(
    Column = all_cols,
    Present_In = rowSums(presence_matrix),
    Present_In_All = rowSums(presence_matrix) == ncol(presence_matrix)
  )
  
  # Add presence in each data frame
  for (i in seq_along(dfList)) {
    summary_df[, df_names[i]] <- presence_matrix[, i]
  }
  
  result <- list(
    common = common_cols,
    summary = summary_df
  )
  
  if (debug) {
    result$debug_info <- debug_info
  }
  
  # Print debug information if requested
  if (debug) {
    cat("\nDebug Information:\n")
    cat("Number of data frames:", length(dfList), "\n")
    cat("Number of unique columns:", length(all_cols), "\n")
    cat("Number of common columns:", length(common_cols), " (Threshold:", threshold, ")\n")
    cat("\nCommon columns found:", paste(common_cols, collapse = ", "), "\n")
    cat("\nColumn presence summary:\n")
    print(table(summary_df$Present_In))
  }
  
  return(result)
}