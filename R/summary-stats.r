#' Calculate Basic Summary Statistics
#'
#' @description Internal function to calculate basic summary statistics for a numeric vector
#'
#' @param x A data frame containing the variable to summarize
#' @param col Name of the column to summarize
#' @param digits Number of decimal places for rounding (default = 3)
#'
#' @return A data frame with summary statistics (n, mean, sd, SEM, median, max, min)
#' @noRd
calc_summary_stats <- function(x, col, digits = 3) {
    if (all(is.na(x[[col]]))) {
        return(data.frame(
            n = NA_integer_,
            mean = NA_real_,
            sd = NA_real_,
            SEM = NA_real_,
            median = NA_real_,
            max = NA_real_,
            min = NA_real_
        ))
    }
    
    n <- sum(!is.na(x[[col]]))
    sd_val <- stats::sd(x[[col]], na.rm = TRUE)
    mean_val <- mean(x[[col]], na.rm = TRUE)
    
    data.frame(
        n = n,
        mean = round(mean_val, digits),
        sd = round(sd_val, digits),
        SEM = round(sd_val / sqrt(n), digits),
        median = round(stats::median(x[[col]], na.rm = TRUE), digits),
        max = round(max(x[[col]], na.rm = TRUE), digits),
        min = round(min(x[[col]], na.rm = TRUE), digits)
    )
}

#' Calculate Summary Statistics for Numeric Data
#'
#' @description Calculates summary statistics (mean, standard deviation, standard error,
#' median, maximum, and minimum) for numeric data. Works with vectors, matrices, or 
#' data frames, with optional grouping variables.
#'
#' @param data A numeric vector, matrix, or data frame
#' @param varname Character string specifying the variable name to summarize. 
#' If NULL and data is a vector, uses the entire vector.
#' @param groupnames Character vector of column names to group by. 
#' Optional for data frames, ignored for vectors and matrices.
#' @param na.rm Logical, whether to remove NA values from grouping variables (default = TRUE)
#' @param digits Number of decimal places for rounding (default = 3)
#'
#' @return A data frame containing summary statistics:
#' \itemize{
#'   \item n (sample size)
#'   \item mean
#'   \item sd (standard deviation)
#'   \item SEM (standard error of the mean)
#'   \item median
#'   \item max (maximum value)
#'   \item min (minimum value)
#' }
#'
#' @examples
#' # Vector example
#' vec <- rnorm(100)
#' data_summary(vec)
#'
#' # Data frame example with grouping
#' df <- data.frame(
#'   value = rnorm(100),
#'   group = rep(c("A", "B"), each = 50)
#' )
#' data_summary(df, "value", "group")
#'
#' @export
data_summary <- function(data, varname = NULL, groupnames = NULL, 
                        na.rm = TRUE, digits = 3) {
    # Input validation and conversion
    if (is.vector(data)) {
        if (!is.numeric(data)) stop("Vector must be numeric")
        data <- data.frame(value = data)
        varname <- "value"
    } else if (is.matrix(data)) {
        if (!is.numeric(data)) stop("Matrix must be numeric")
        data <- as.data.frame(data)
        if (is.null(varname)) {
            varname <- colnames(data)[1]
        }
    } else if (!is.data.frame(data)) {
        stop("Input must be a vector, matrix, or data frame")
    }

    # Input validation
    validate_inputs(data, varname, groupnames)

    # Handle NA values in grouping variables
    if (!is.null(groupnames) && na.rm) {
        data <- remove_na_groups(data, groupnames)
    }

    # Calculate summaries
    if (is.null(groupnames)) {
        # No grouping - calculate overall summary
        data_sum <- calc_summary_stats(data, varname, digits)
    } else {
        # With grouping
        data_sum <- do_grouped_summary(data, varname, groupnames, digits)
    }

    # Convert row names to numbers
    row.names(data_sum) <- NULL

    return(data_sum)
}

#' Validate Input Parameters
#'
#' @param data A data frame
#' @param varname Variable name to summarize
#' @param groupnames Grouping variables
#' @noRd
validate_inputs <- function(data, varname, groupnames) {
    if (is.null(data)) stop("Input data cannot be NULL")
    
    if (!is.null(varname) && !(varname %in% colnames(data))) {
        stop("Variable name not found in data")
    }
    
    if (!is.null(groupnames) && !all(groupnames %in% colnames(data))) {
        stop("One or more grouping variables not found in data")
    }
}

#' Remove NA Values from Grouping Variables
#'
#' @param data A data frame
#' @param groupnames Names of grouping variables
#' @noRd
remove_na_groups <- function(data, groupnames) {
    na_groups <- apply(data[groupnames], 1, function(x) any(is.na(x)))
    if (any(na_groups)) {
        warning(sprintf("Removed %d rows with NA in grouping variables", 
                       sum(na_groups)))
        data <- data[!na_groups, ]
    }
    return(data)
}

#' Perform Grouped Summary Statistics
#'
#' @param data A data frame
#' @param varname Variable to summarize
#' @param groupnames Grouping variables
#' @param digits Number of decimal places
#' @noRd
do_grouped_summary <- function(data, varname, groupnames, digits) {
    # Calculate summaries by group
    data_sum <- do.call(rbind, 
        by(data, 
           data[groupnames], 
           function(x) calc_summary_stats(x, varname, digits)))
    
    # Add grouping variables back
    group_data <- unique(data[groupnames])
    if (nrow(group_data) == nrow(data_sum)) {
        data_sum <- cbind(group_data, data_sum)
    } else {
        warning("Group sizes don't match summary statistics. Check for NA values.")
    }
    
    return(data_sum)
}
