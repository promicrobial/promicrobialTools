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
            iqr = NA_real_,
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
        iqr = round(stats::IQR(x[[col]], na.rm = TRUE), digits),
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

#' Check Variable Overdispersion
#'
#' @description 
#' Determines if variables are overdispersed by comparing variance to mean.
#' Handles single vectors, multiple variables, and data frames.
#'
#' @param x Numeric vector, list of vectors, or data frame
#' @param digits Number of decimal places for rounding (default = 4)
#' @param threshold Ratio threshold for determining overdispersion (default = 1)
#' @param na.rm Logical, whether to remove NA values (default = TRUE)
#' @param verbose Logical, whether to print detailed output (default = TRUE)
#' @param vars Variables to check in data frame (default: all numeric columns)
#'
#' @return Depends on input:
#'   - Vector: List of statistics
#'   - Multiple variables/data frame: Data frame of results
#'
#' @examples
#' # Single vector
#' x <- rpois(100, lambda = 5)
#' varDisp(x)
#'
#' # Multiple variables
#' vars <- list(
#'   a = rpois(100, 5),
#'   b = rnbinom(100, 5, 0.5)
#' )
#' varDisp(vars)
#'
#' # Data frame
#' df <- data.frame(
#'   a = rpois(100, 5),
#'   b = rnbinom(100, 5, 0.5),
#'   c = rnorm(100, 10, 2)
#' )
#' varDisp(df)
#'
#' @export
varDisp <- function(x, ...) {
  result <- UseMethod("varDisp")
}

#' Check Variable Overdispersion
#'
#' @description 
#' Determines if variables are overdispersed by comparing variance to mean.
#' Returns an object of class "overdispersion".
#'
varDisp <- function(x, ...) {
  result <- UseMethod("varDisp")
  return(result)  # Remove this line - let methods handle class assignment
}

#' @export
varDisp.default <- function(x, digits = 4, threshold = 1, 
                                       na.rm = TRUE, verbose = TRUE) {
  if (!is.numeric(x)) {
    stop("Input must be numeric")
  }
  
  # Calculate statistics
  mean_val <- mean(x, na.rm = na.rm)
  var_val <- var(x, na.rm = na.rm)
  ratio <- var_val / mean_val
  
  # Create result
  result <- list(
    mean = round(mean_val, digits),
    variance = round(var_val, digits),
    ratio = round(ratio, digits),
    is_overdispersed = ratio > threshold,
    magnitude = dplyr::case_when(
      ratio < threshold ~ "underdispersed",
      ratio >= threshold & ratio < threshold * 2 ~ "mildly overdispersed",
      ratio >= threshold * 2 & ratio < threshold * 5 ~ "moderately overdispersed",
      ratio >= threshold * 5 ~ "highly overdispersed"
    )
  )
  
  # Assign class
  class(result) <- c("varDisp", "list")
  
  # Print output if verbose
  if (verbose) {
    print(result)
  }
  
  return(result)
}

#' @export
varDisp.data.frame <- function(x, digits = 4, threshold = 1, 
                                          na.rm = TRUE, verbose = TRUE, 
                                          vars = NULL) {
  # If no variables specified, use all numeric columns
  if (is.null(vars)) {
    vars <- names(x)[sapply(x, is.numeric)]
  }
  
  # Check each variable
  results <- lapply(vars, function(var) {
    if (!is.numeric(x[[var]])) {
      return(NULL)
    }
    result <- varDisp(x[[var]], 
                                 digits = digits,
                                 threshold = threshold,
                                 na.rm = na.rm,
                                 verbose = FALSE)
    c(variable = var,
      mean = result$mean,
      variance = result$variance,
      ratio = result$ratio,
      is_overdispersed = result$is_overdispersed,
      magnitude = result$magnitude)
  })
  
  # Remove NULL results and convert to data frame
  results <- results[!sapply(results, is.null)]
  results_df <- do.call(rbind, results) %>%
    as.data.frame() %>%
    dplyr::mutate(across(c(mean, variance, ratio), as.numeric),
                  is_overdispersed = as.logical(is_overdispersed))
  
  # Sort by ratio
  results_df <- results_df[order(-results_df$ratio), ]
  
  # Assign class
  class(results_df) <- c("varDisp", "data.frame")
  
  # Print summary if verbose
  if (verbose) {
    print_summary(results_df, threshold)
  }
  
  return(results_df)
}

# Print method for overdispersion class
#' @export
print.varDisp <- function(x, ...) {
  if (inherits(x, "data.frame")) {
    cat("\nOverdispersion Analysis Results:\n")
    cat("-----------------------------\n")
    print(as.data.frame(x), ...)
  } else {
    cat("\nOverdispersion Analysis Result:\n")
    cat("---------------------------\n")
    cat(sprintf("Mean: %g\n", x$mean))
    cat(sprintf("Variance: %g\n", x$variance))
    cat(sprintf("Variance-to-Mean Ratio: %g\n", x$ratio))
    cat(sprintf("Status: %s\n", x$magnitude))
  }
}

# Summary method for overdispersion class
#' @export
summary.varDisp <- function(object, ...) {
  if (inherits(object, "data.frame")) {
    n_vars <- nrow(object)
    n_overdispersed <- sum(object$is_overdispersed)
    
    cat("\nOverdispersion Summary:\n")
    cat("--------------------\n")
    cat(sprintf("Total variables analyzed: %d\n", n_vars))
    cat(sprintf("Overdispersed variables: %d (%.1f%%)\n", 
                n_overdispersed, 100 * n_overdispersed/n_vars))
    
    # Magnitude breakdown
    mag_table <- table(object$magnitude)
    cat("\nMagnitude breakdown:\n")
    print(mag_table)
    
  } else {
    print.varDisp(object)
  }
}

#' Plot method for overdispersion results
#'
#' @param x Output from varDisp
#' @param type Plot type ("histogram" or "dot", default = "histogram")
#' @param threshold Overdispersion threshold (default = 1)
#' @param show_labels Logical, whether to show variable labels (default = TRUE)
#' @param use_ggplot Logical, whether to use ggplot2 or base R (default = FALSE)
#' @param ... Additional arguments passed to plotting functions
#'
#' @export
plot.varDisp <- function(x, 
                              type = c("histogram", "dot"),
                              threshold = 1,
                              show_labels = TRUE,
                              use_ggplot = FALSE,
                              ...) {
  
  # Ensure x has the correct structure
  if (!inherits(x, "varDisp")) {
    stop("Object must be of class 'varDisp'")
  }
  
  # Match plot type argument
  type <- match.arg(type)
  
  # Extract ratio data
  ratios <- x$ratio
      
  if (type == "histogram") {
    # Create histogram
    hist(ratios,
         main = "Distribution of Variance-to-Mean Ratios",
         xlab = "Variance-to-Mean Ratio",
         ylab = "Frequency",
         breaks = "FD",  # Freedman-Diaconis rule for bin width
         ...)
    
    # Add threshold line
    abline(v = threshold, 
           lty = 2, 
           lwd = 2)
    
    # Add legend
    legend("topright",
           legend = c("Threshold", 
                     sprintf("Overdispersed (n=%d)", 
                             sum(ratios > threshold))),
           lty = c(2, 1))
    
  } else if (type == "dot") {
    # Create dot plot
    plot(ratios,
         main = "Variance-to-Mean Ratios by Variable",
         ylab = "Variance-to-Mean Ratio",
         xlab = "Variable Index",
         pch = 19,
         ...)
    
    # Add threshold line
    abline(h = threshold, 
           lty = 2, 
           lwd = 2)
    
    # Add variable labels if requested
    if (show_labels && !is.null(x$variable)) {
      text(1:length(ratios),
           ratios,
           labels = x$variable,
           pos = 4,
           cex = 0.8)
    }
    
    # Add legend
    legend("topright",
           legend = c("Threshold",
                     "Normal",
                     "Overdispersed"),
           pch = c(NA, 19, 19),
           lty = c(2, NA, NA))
  }
  
  # Add summary text
  mtext(sprintf("Total variables: %d, Overdispersed: %d (%.1f%%)",
                length(ratios),
                sum(ratios > threshold),
                100 * mean(ratios > threshold)),
        side = 3,
        line = 0)
}



