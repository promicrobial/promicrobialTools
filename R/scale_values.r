#' Scale Numeric Values to [0,1] Range
#'
#' @description
#' Scales numeric values to the [0,1] range using min-max normalization. The function
#' handles missing values, infinite values, and edge cases while preserving the relative
#' distribution of the data.
#'
#' @param x Numeric vector, matrix, or data frame to be scaled
#' @param na.rm Logical. Should missing values be removed when computing min/max? (default: TRUE)
#' @param preserve_zero Logical. Should zero values remain zero after scaling? (default: FALSE)
#' @param tolerance Numeric. Values below this threshold are considered zero (default: 1e-10)
#'
#' @return An object of the same type and dimensions as the input, with values scaled to [0,1].
#'   Returns NA for constant vectors (where min = max).
#'
#' @details
#' The function applies the following transformation:
#' \deqn{x_{scaled} = \frac{x - min(x)}{max(x) - min(x)}}
#'
#' Special cases:
#' \itemize{
#'   \item If all values are identical, returns NA with a warning
#'   \item Handles infinite values by converting them to NA
#'   \item Can preserve zero values if preserve_zero = TRUE
#' }
#'
#' @examples
#' # Simple vector scaling
#' x <- c(1, 2, 3, 4, 5)
#' scale_values(x)
#'
#' # Handling missing values
#' x_na <- c(1, 2, NA, 4, 5)
#' scale_values(x_na, na.rm = TRUE)
#'
#' # Preserving zeros
#' x_zeros <- c(0, 1, 2, 3, 0)
#' scale_values(x_zeros, preserve_zero = TRUE)
#'
#' # Scaling a matrix
#' mat <- matrix(1:9, nrow = 3)
#' scale_values(mat)
#'
#' @seealso
#' \code{\link{scale}}, \code{\link{normalize}}
#'
#' @export
scale_values <- function(x, 
                        na.rm = TRUE, 
                        preserve_zero = FALSE,
                        tolerance = 1e-10) {
  
  # Input validation
  if (!is.numeric(x) && !is.matrix(x) && !is.data.frame(x)) {
    stop("Input must be numeric, matrix, or data frame")
  }
  
  if (!is.logical(na.rm)) {
    stop("'na.rm' must be logical (TRUE/FALSE)")
  }
  
  if (!is.logical(preserve_zero)) {
    stop("'preserve_zero' must be logical (TRUE/FALSE)")
  }
  
  if (!is.numeric(tolerance) || tolerance <= 0) {
    stop("'tolerance' must be a positive numeric value")
  }
  
  # Handle different input types
  if (is.data.frame(x)) {
    return(as.data.frame(lapply(x, function(col) {
      if (is.numeric(col)) {
        scale_values(col, na.rm, preserve_zero, tolerance)
      } else {
        col
      }
    })))
  }
  
  # Convert to numeric vector if matrix
  if (is.matrix(x)) {
    original_dim <- dim(x)
    x <- as.vector(x)
  }
  
  # Remove infinite values
  x[is.infinite(x)] <- NA
  
  # Get min and max
  x_min <- min(x, na.rm = na.rm)
  x_max <- max(x, na.rm = na.rm)
  
  # Check for constant vector
  if (abs(x_max - x_min) < tolerance) {
    warning("Input has zero variance (constant values)")
    return(rep(NA, length(x)))
  }
  
  # Perform scaling
  if (preserve_zero) {
    # Identify zeros (within tolerance)
    zeros <- abs(x) < tolerance
    
    # Scale non-zero values
    result <- x
    result[!zeros] <- (x[!zeros] - x_min) / (x_max - x_min)
    result[zeros] <- 0
  } else {
    result <- (x - x_min) / (x_max - x_min)
  }
  
  # Ensure results are in [0,1]
  result[result < 0] <- 0
  result[result > 1] <- 1
  
  # Restore matrix structure if needed
  if (exists("original_dim")) {
    result <- matrix(result, nrow = original_dim[1], ncol = original_dim[2])
  }
  
  # Add attributes for documentation
  attr(result, "original_range") <- c(x_min, x_max)
  attr(result, "scaling_info") <- list(
    min = x_min,
    max = x_max,
    na.rm = na.rm,
    preserve_zero = preserve_zero,
    tolerance = tolerance
  )
  
  class(result) <- c("scaled_values", class(result))
  
  return(result)
}

#' Print method for scaled_values objects
#'
#' @param x A scaled_values object
#' @param ... Additional arguments passed to print
#'
#' @export
print.scaled_values <- function(x, ...) {
  cat("Scaled values (range: [0,1])\n")
  cat("Original range:", paste(attr(x, "original_range"), collapse = " to "), "\n\n")
  NextMethod()
}

#' Custom knit_print method for scaled_values objects
#'
#' @param x A scaled_values object
#' @param ... Additional arguments passed to knitr
#'
#' @export
knit_print.scaled_values <- function(x, ...) {
  if (requireNamespace("knitr", quietly = TRUE)) {
    # Create a formatted output for knitr
    scaling_info <- attr(x, "scaling_info")
    header <- sprintf(
      "Scaled values (original range: %.2f to %.2f)",
      scaling_info$min,
      scaling_info$max
    )
    
    # Return formatted output
    return(knitr::asis_output(
      paste(header, "\n", paste(x, collapse = " "))
    ))
  }
  print(x)
}