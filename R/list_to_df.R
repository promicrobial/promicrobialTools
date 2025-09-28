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
#' mite.ano <- with(mite.env, anosim(mite.dist, Shrub))
#' 
#' complex_list <- list(dune = dune.ano, mite = mite.ano)
#' complex_df <- list_to_df(complex_list, elements = c("statistic", "signif"), nested = TRUE)
#' @export
#'
#' @seealso 
#' \code{\link{data.frame}} for base R data frame creation
#' \code{\link{do.call}} for list manipulation
#'
#' @returns
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
list_to_df <- function(list_obj, elements = NULL, prefix = "", suffix = "", exclude = NULL, row_names = NULL, col_names = NULL, transpose_matrices = FALSE, nested = FALSE) {
    
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
        
        # Override, preserve or create column names
        if (!is.null(colnames(mat))) {
            colnames(df) <- colnames(mat)
        } else if (!is.null(col_names)) {
            if (length(col_names) != ncol(df)) {
                stop("Length of col_names does not match number of columns")
            }
            colnames(df) <- col_names
        }
        
        # Override, preserve or create row names
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
    find_nested_elements <- function(lst, target) {
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
            names(value) <- target
            return(value)
        }
        
        result <- NULL
        for (name in names(lst)) {
            if (is.list(lst[[name]])) {
                nested_result <- find_nested_elements(lst[[name]], target)
                if (!is.null(nested_result)) return(nested_result)
            }
        }
        return(NULL)
    }
    
    if (nested) {
        # Check if all elements are lists
        all_lists <- all(sapply(list_obj, is.list))
        
        if (all_lists) {
            # Get all unique keys from nested lists
            all_keys <- unique(unlist(lapply(list_obj, names)))
            
            # Create a matrix to store results
            result_matrix <- matrix(NA, 
                                  nrow = length(all_keys), 
                                  ncol = length(list_obj),
                                  dimnames = list(all_keys, names(list_obj)))
            
            # Fill the matrix
            for (top_name in names(list_obj)) {
                for (key in names(list_obj[[top_name]])) {
                    result_matrix[key, top_name] <- 
                        paste(list_obj[[top_name]][[key]], collapse = ", ")
            }
        }
        
            # Convert to data frame
            df <- as.data.frame(result_matrix, stringsAsFactors = FALSE)
            
        } else {
            # Original nested processing for other cases
            # [Previous nested processing code here]
        }
        
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
      # Handle column names
      if (!is.null(col_names)) {
          if (length(col_names) != ncol(df)) {
              stop("Length of col_names does not match number of columns")
          }
          colnames(df) <- col_names
      } else {
        col_names <- colnames(df)
      }
        if (!is.null(prefix)) col_names <- paste0(prefix, col_names)
        if (!is.null(suffix)) col_names <- paste0(col_names, suffix)
        colnames(df) <- col_names
    } else {
      if (!is.null(col_names)) {
          if (length(col_names) != ncol(df)) {
              stop("Length of col_names does not match number of columns")
          }
          colnames(df) <- col_names
      }
    }
  
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