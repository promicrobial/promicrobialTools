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
#' # Basic usage
#' commonCols(dfList)
#'
#' # With debugging
#' commonCols(dfList, debug = TRUE)
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

#' Convert List to Data Frame
#'
#' @description
#' Converts a list or nested list structure to a data frame with flexible options for element selection,
#' naming, and formatting. This function allows for subsetting of list elements, handling of various data
#' structures including matrices with dimnames, and customization of the resulting data frame structure.
#'
#' @param list_obj A list object to be converted to a data frame
#' @param elements Character vector specifying which list elements to include.
#'                If NULL (default), all elements are included.
#' @param prefix Character string to add as prefix to column names (default: "")
#' @param suffix Character string to add as suffix to column names (default: "")
#' @param exclude Character vector of element names to exclude from the conversion
#' @param row_names Character vector of custom row names for the resulting data frame
#'
#' @return A data frame where:
#' \itemize{
#'   \item Each list element becomes a column
#'   \item Column names are derived from list element names
#'   \item All elements must have the same length
#' }
#' 
#' @details
#' The function performs the following steps:
#' 1. Validates input parameters
#' 2. Selects specified elements
#' 3. Checks for consistent lengths
#' 4. Combines elements into a data frame
#' 5. Applies naming conventions
#' 
#' @examples
#' # Create example list
#' example_list <- list(
#'     R2 = c(0.1, 0.2, 0.3),
#'     F0 = c(1.1, 1.2, 1.3),
#'     RSS = c(0.01, 0.02, 0.03)
#' )
#'
#' # Basic usage - convert entire list
#' df1 <- list_to_df(example_list)
#'
#' # Select specific elements
#' df2 <- list_to_df(example_list, elements = c("R2", "F0"))
#'
#' # Add prefix to column names
#' df3 <- list_to_df(example_list, prefix = "stat_")
#'
#' # Exclude specific elements
#' df4 <- list_to_df(example_list, exclude = "RSS")
#'
#' # Custom row names
#' df5 <- list_to_df(example_list, 
#'                   row_names = c("Gene1", "Gene2", "Gene3"))
#' 
#' # Example with nested lists
#' nested_list <- list(
#'     metrics = list(R2 = c(0.1, 0.2), F0 = c(1.1, 1.2)),
#'     params = list(a = c(0.5, 0.6), b = c(0.7, 0.8))
#' )
#' 
#' nested_df <- list_to_df(nested_list)
#' 
#' # Complex nested lists
#' library(vegan)
#' data(dune)
#' data(dune.env) 
#' dune.dist <- vegdist(dune)
#' dune.ano <- with(dune.env, anosim(dune.dist, Management))
#' 
#' data(mite)
#' data(mite.env) 
#' mite.dist <- vegdist(mite)
#' mite.ano <- with(mite.env, anosim(mite.dist, WatrCont))
#' 
#' complex_list <- list(dune = dune.ano, mite = mite.ano)
#' complex_df <- list_to_df(complex_list, elements = c("statistic", "signif"))
#' @export
#'
#' @seealso 
#' \code{\link{data.frame}} for base R data frame creation
#' \code{\link{do.call}} for list manipulation
#'
#' @throws
#' Errors if:
#' \itemize{
#'   \item Input is not a list
#'   \item List is empty
#'   \item Requested elements don't exist
#'   \item Selected elements have different lengths
#'   \item Row names length doesn't match data
#' }
#'
#' @note
#' All selected list elements must have the same length to be combined into
#' a data frame. The function performs various validation checks to ensure
#' data integrity.
#'
#' @keywords utilities
#' @importFrom stats setNames
list_to_df <- function(list_obj, elements = NULL, prefix = "", suffix = "", exclude = NULL, row_names = NULL, transpose_matrices = FALSE, nested = FALSE) {
    
    # Input validation
    if (!is.list(list_obj)) {
        stop(sprintf("Input must be a list, got %s instead", class(list_obj)))
    }
    
    if (length(list_obj) == 0) {
        stop("Input list is empty")
    }
    
    # Special handling for single-matrix list
    if (length(list_obj) == 1 && is.matrix(list_obj[[1]])) {
        mat <- list_obj[[1]]
        if (transpose_matrices) {
            mat <- t(mat)
        }
        
        df <- as.data.frame(mat)
        
        # Preserve or create column names
        if (!is.null(colnames(mat))) {
            colnames(df) <- colnames(mat)
        } else {
            colnames(df) <- paste0("V", seq_len(ncol(mat)))
        }
        
        # Preserve or create row names
        if (!is.null(rownames(mat))) {
            rownames(df) <- rownames(mat)
        } else if (!is.null(row_names)) {
            if (length(row_names) != nrow(df)) {
                stop("Length of row_names does not match number of rows")
            }
            rownames(df) <- row_names
        }
        
        # Apply prefix/suffix to column names if specified
        if (!is.null(prefix) || !is.null(suffix)) {
            col_names <- colnames(df)
            if (!is.null(prefix)) col_names <- paste0(prefix, col_names)
            if (!is.null(suffix)) col_names <- paste0(col_names, suffix)
            colnames(df) <- col_names
        }
        
        return(df)
    }
    
    # Function to find nested elements
    find_nested_elements <- function(lst, target, parent_name = "") {
        if (!is.list(lst)) return(NULL)
        
        if (target %in% names(lst)) {
            value <- lst[[target]]
            if (is.matrix(value)) {
                value <- as.data.frame(value)
            } else if (is.list(value)) {
                value <- as.data.frame(value)
            } else {
                value <- data.frame(value)
            }
            names(value) <- if (parent_name == "") target else paste(parent_name, target, sep = "_")
            return(value)
        }
        
        result <- NULL
        for (name in names(lst)) {
            if (is.list(lst[[name]])) {
                nested_result <- find_nested_elements(lst[[name]], target, 
                                                   if (parent_name == "") name else paste(parent_name, name, sep = "_"))
                if (!is.null(nested_result)) return(nested_result)
            }
        }
        return(NULL)
    }
    
    if (nested) {
        # Handle nested list processing
        result_list <- list()
        top_names <- if (!is.null(exclude)) {
            setdiff(names(list_obj), exclude)
        } else {
            names(list_obj)
        }
        
        # If no elements specified, try to find common elements
        if (is.null(elements)) {
            elements <- unique(unlist(lapply(list_obj, function(x) names(x))))
            if (length(elements) == 0) {
                stop("No elements specified and couldn't find any nested elements")
            }
        }
        
        # Process each top-level element
        for (top_name in top_names) {
            for (elem in elements) {
                found <- find_nested_elements(list_obj[[top_name]], elem, top_name)
                if (!is.null(found)) {
                    result_list[[paste(top_name, elem, sep = "_")]] <- found
                }
            }
        }
        
        if (length(result_list) == 0) {
            stop("No matching elements found")
        }
        
        df <- do.call(cbind, result_list)
        
    } else {
        # Original non-nested processing
        if (is.null(elements)) {
            selected_elements <- names(list_obj)
        } else {
            if (!all(elements %in% names(list_obj))) {
                missing <- elements[!elements %in% names(list_obj)]
                stop("The following requested elements do not exist in the list: ", 
                     paste(missing, collapse = ", "))
            }
            selected_elements <- elements
        }
        
        if (!is.null(exclude)) {
            selected_elements <- selected_elements[!selected_elements %in% exclude]
            if (length(selected_elements) == 0) {
                stop("No elements remain after exclusions")
            }
        }
        
        sub_list <- list_obj[selected_elements]
        lengths <- sapply(sub_list, length)
        if (length(unique(lengths)) > 1) {
            stop("Selected elements have different lengths: \n",
                 paste(names(lengths), ":", lengths, collapse = "\n"))
        }
        
        df <- as.data.frame(do.call(cbind, sub_list))
        colnames(df) <- names(sub_list)
    }
    
    # Apply prefix/suffix to column names
    if (!is.null(prefix) || !is.null(suffix)) {
        col_names <- colnames(df)
        if (!is.null(prefix)) col_names <- paste0(prefix, col_names)
        if (!is.null(suffix)) col_names <- paste0(col_names, suffix)
        colnames(df) <- col_names
    }
    
    # Handle row names
    if (!is.null(row_names)) {
        if (length(row_names) != nrow(df)) {
            stop("Length of row_names does not match number of rows")
        }
        rownames(df) <- row_names
    }
    
    return(df)
}

#' Print method for list_to_df output
#'
#' @param x Object of class list_df
#' @param ... Additional arguments passed to print
#'
#' @export
#'
#' @keywords internal
print.list_df <- function(x, ...) {
    cat("Converted list to dataframe:\n")
    cat("Dimensions:", dim(x)[1], "rows,", dim(x)[2], "columns\n")
    cat("Columns:", paste(names(x), collapse = ", "), "\n\n")
    NextMethod()
}