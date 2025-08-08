#' Convert Data Frame to Delimited Text Format
#'
#' @description
#' Converts a data frame or matrix to a delimited text format (CSV, TSV, or custom separator).
#' Similar to print() or kable() but returns a string formatted for direct writing to file
#' or displaying in console with chosen delimiter.
#'
#' @param x A data frame or matrix to convert
#' @param format Output format, one of "csv", "tsv", or "custom" (default: "csv")
#' @param sep Custom separator to use when format="custom" (default: ",")
#' @param row.names Logical indicating whether to include row names (default: FALSE)
#' @param col.names Logical indicating whether to include column names (default: TRUE)
#' @param quote Logical indicating whether to quote strings (default: TRUE)
#' @param na String to use for NA values (default: "")
#' @param trim Logical indicating whether to trim whitespace from values (default: TRUE)
#' @param scientific Logical indicating whether to use scientific notation (default: FALSE)
#' @param digits Integer indicating number of decimal places for numeric values (default: NULL)
#'
#' @return A character string containing the formatted data
#'
#' @examples
#' # Create example data frame
#' df <- data.frame(
#'   A = c(1, 2, 3),
#'   B = c("a", "b", "c"),
#'   C = c(1.234, 5.678, 9.012)
#' )
#'
#' # CSV format
#' delim(df)
#'
#' # TSV format
#' delim(df, format = "tsv")
#'
#' # Custom separator
#' delim(df, format = "custom", sep = "|")
#'
#' # With row names and 2 decimal places
#' delim(df, row.names = TRUE, digits = 2)
#'
#' @export
delim <- function(x,
                       format = c("csv", "tsv", "custom"),
                       sep = ",",
                       row.names = FALSE,
                       col.names = TRUE,
                       quote = TRUE,
                       na = "",
                       trim = TRUE,
                       scientific = FALSE,
                       digits = NULL) {
    
    # Input validation
    format <- match.arg(format)
    if (!is.data.frame(x) && !is.matrix(x)) {
        stop("Input must be a data frame or matrix")
    }
    
    # Convert matrix to data frame
    if (is.matrix(x)) {
        x <- as.data.frame(x)
    }
    
    # Determine separator
    sep <- switch(format,
                 csv = ",",
                 tsv = "\t",
                 custom = sep)
    
    # Format numeric values
    if (!is.null(digits)) {
        for (i in seq_along(x)) {
            if (is.numeric(x[[i]])) {
                x[[i]] <- round(x[[i]], digits)
            }
        }
    }
    
    # Handle scientific notation
    if (!scientific) {
        for (i in seq_along(x)) {
            if (is.numeric(x[[i]])) {
                x[[i]] <- format(x[[i]], scientific = FALSE)
            }
        }
    }
    
    # Convert NA values
    x[] <- lapply(x, function(col) {
        ifelse(is.na(col), na, as.character(col))
    })
    
    # Trim whitespace if requested
    if (trim) {
        x[] <- lapply(x, trimws)
    }
    
    # Add row names if requested
    if (row.names) {
        x <- cbind(row.names = rownames(x), x)
    }
    
    # Quote values if requested
    if (quote) {
        x[] <- lapply(x, function(col) {
            needs_quotes <- grepl(sep, col) | grepl('"', col) | grepl('\n', col)
            ifelse(needs_quotes,
                  sprintf('"%s"', gsub('"', '""', col)),
                  col)
        })
    }
    
    # Create header
    header <- if (col.names) paste(colnames(x), collapse = sep) else NULL
    
    # Create rows
    rows <- apply(x, 1, paste, collapse = sep)
    
    # Combine header and rows
    result <- if (col.names) {
        paste(c(header, rows), collapse = "\n")
    } else {
        paste(rows, collapse = "\n")
    }
    
    # Add class for potential method dispatch
    class(result) <- c("delim_output", "character")
    
    return(result)
}

csv <- function(x,
                       format = "csv",
                       sep = ",",
                       row.names = FALSE,
                       col.names = TRUE,
                       quote = TRUE,
                       na = "",
                       trim = TRUE,
                       scientific = FALSE,
                       digits = NULL) {
    
    # Input validation
    format <- match.arg(format)
    if (!is.data.frame(x) && !is.matrix(x)) {
        stop("Input must be a data frame or matrix")
    }
    
    # Convert matrix to data frame
    if (is.matrix(x)) {
        x <- as.data.frame(x)
    }
       
    # Format numeric values
    if (!is.null(digits)) {
        for (i in seq_along(x)) {
            if (is.numeric(x[[i]])) {
                x[[i]] <- round(x[[i]], digits)
            }
        }
    }
    
    # Handle scientific notation
    if (!scientific) {
        for (i in seq_along(x)) {
            if (is.numeric(x[[i]])) {
                x[[i]] <- format(x[[i]], scientific = FALSE)
            }
        }
    }
    
    # Convert NA values
    x[] <- lapply(x, function(col) {
        ifelse(is.na(col), na, as.character(col))
    })
    
    # Trim whitespace if requested
    if (trim) {
        x[] <- lapply(x, trimws)
    }
    
    # Add row names if requested
    if (row.names) {
        x <- cbind(row.names = rownames(x), x)
    }
    
    # Quote values if requested
    if (quote) {
        x[] <- lapply(x, function(col) {
            needs_quotes <- grepl(sep, col) | grepl('"', col) | grepl('\n', col)
            ifelse(needs_quotes,
                  sprintf('"%s"', gsub('"', '""', col)),
                  col)
        })
    }
    
    # Create header
    header <- if (col.names) paste(colnames(x), collapse = sep) else NULL
    
    # Create rows
    rows <- apply(x, 1, paste, collapse = sep)
    
    # Combine header and rows
    result <- if (col.names) {
        paste(c(header, rows), collapse = "\n")
    } else {
        paste(rows, collapse = "\n")
    }
    
    # Add class for potential method dispatch
    class(result) <- c("delim_output", "character")
    
    return(result)
}

tsv <- function(x,
                       format = "tsv",
                       sep = "\t",
                       row.names = FALSE,
                       col.names = TRUE,
                       quote = TRUE,
                       na = "",
                       trim = TRUE,
                       scientific = FALSE,
                       digits = NULL) {
    
    # Input validation
    format <- match.arg(format)
    if (!is.data.frame(x) && !is.matrix(x)) {
        stop("Input must be a data frame or matrix")
    }
    
    # Convert matrix to data frame
    if (is.matrix(x)) {
        x <- as.data.frame(x)
    }
       
    # Format numeric values
    if (!is.null(digits)) {
        for (i in seq_along(x)) {
            if (is.numeric(x[[i]])) {
                x[[i]] <- round(x[[i]], digits)
            }
        }
    }
    
    # Handle scientific notation
    if (!scientific) {
        for (i in seq_along(x)) {
            if (is.numeric(x[[i]])) {
                x[[i]] <- format(x[[i]], scientific = FALSE)
            }
        }
    }
    
    # Convert NA values
    x[] <- lapply(x, function(col) {
        ifelse(is.na(col), na, as.character(col))
    })
    
    # Trim whitespace if requested
    if (trim) {
        x[] <- lapply(x, trimws)
    }
    
    # Add row names if requested
    if (row.names) {
        x <- cbind(row.names = rownames(x), x)
    }
    
    # Quote values if requested
    if (quote) {
        x[] <- lapply(x, function(col) {
            needs_quotes <- grepl(sep, col) | grepl('"', col) | grepl('\n', col)
            ifelse(needs_quotes,
                  sprintf('"%s"', gsub('"', '""', col)),
                  col)
        })
    }
    
    # Create header
    header <- if (col.names) paste(colnames(x), collapse = sep) else NULL
    
    # Create rows
    rows <- apply(x, 1, paste, collapse = sep)
    
    # Combine header and rows
    result <- if (col.names) {
        paste(c(header, rows), collapse = "\n")
    } else {
        paste(rows, collapse = "\n")
    }
    
    # Add class for potential method dispatch
    class(result) <- c("delim_output", "character")
    
    return(result)
}

#' Print method for delim_output
#'
#' @param x Object of class delim_output
#' @param ... Additional arguments passed to print
#'
#' @export
print.delim_output <- function(x, ...) {
    cat(x, "\n")
}

#' Format numeric values for delimited output
#'
#' @param x Numeric vector to format
#' @param digits Number of decimal places
#' @param scientific Whether to use scientific notation
#' @return Character vector of formatted numbers
#' @keywords internal
format_numeric <- function(x, digits = NULL, scientific = FALSE) {
    if (is.null(digits)) {
        format(x, scientific = scientific)
    } else {
        format(round(x, digits), scientific = scientific)
    }
}