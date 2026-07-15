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
#' \code{\link[https://cran.r-project.org/doc/manuals/r-release/fullrefman.pdf#Rfn.scale.1]{scale}}, \code[https://cran.r-project.org/doc/manuals/r-release/fullrefman.pdf#Rfn.sweep.1]{\link{sweep}}
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

#' Backtransform Scaled Values to Original Scale
#'
#' @description
#' This function reverses the scaling transformation applied by `scale_values()`,
#' converting values from a 0-1 scale back to their original scale using the 
#' range and minimum of the original data or scaling attributes.
#'
#' @param scaled_value A numeric vector, matrix, or \code{scaled_values} object 
#'   containing scaled values (typically between 0 and 1) to be backtransformed.
#' @param original_data A numeric vector, matrix, or data frame representing the 
#'   original data used for scaling. Can be omitted if \code{scaled_value} is a 
#'   \code{scaled_values} object with attributes.
#' @param na.rm Logical. Should missing values be removed when computing min/max? 
#'   (default: TRUE)
#' @param tolerance Numeric. Values below this threshold are considered zero 
#'   (default: 1e-10)
#'
#' @return A numeric vector, matrix, or data frame of backtransformed values on 
#'   the original scale, maintaining the same structure as the input.
#'
#' @details
#' The backtransformation uses the formula: 
#' \deqn{x_{original} = x_{scaled} \times (max - min) + min}
#' 
#' The function can extract scaling parameters from:
#' \itemize{
#'   \item Attributes of \code{scaled_values} objects (preferred method)
#'   \item The provided \code{original_data} parameter
#' }
#'
#' @examples
#' # Using scale_values and backtransform together
#' original <- c(10, 20, 30, 40, 50)
#' scaled <- scale_values(original)
#' backtransformed <- backtransform_scaled_values(scaled)
#' all.equal(original, backtransformed)  # Should be TRUE
#' 
#' # Manual specification of original data
#' scaled_manual <- c(0, 0.25, 0.5, 0.75, 1)
#' original_data <- c(10, 20, 30, 40, 50)
#' backtransform_scaled_values(scaled_manual, original_data)
#' 
#' # With matrices
#' mat <- matrix(c(1:6), nrow = 2)
#' scaled_mat <- scale_values(mat)
#' backtransform_scaled_values(scaled_mat)
#'
#' @seealso `scale_values()`, `get_scaled_value()`
#'
#' @export
backtransform_scaled_values <- function(scaled_value, 
                                       original_data = NULL, 
                                       na.rm = TRUE,
                                       tolerance = 1e-10) {
  
  # Input validation
  if (!is.numeric(scaled_value) && !is.matrix(scaled_value) && !is.data.frame(scaled_value)) {
    stop("scaled_value must be numeric, matrix, or data frame")
  }
  
  if (!is.logical(na.rm)) {
    stop("'na.rm' must be logical (TRUE/FALSE)")
  }
  
  if (!is.numeric(tolerance) || tolerance <= 0) {
    stop("'tolerance' must be a positive numeric value")
  }
  
  # Try to extract scaling info from attributes first
  scaling_info <- attr(scaled_value, "scaling_info")
  original_range_attr <- attr(scaled_value, "original_range")
  
  if (!is.null(scaling_info)) {
    # Use attributes from scaled_values object
    min_val <- scaling_info$min
    max_val <- scaling_info$max
    original_range <- max_val - min_val
  } else if (!is.null(original_range_attr)) {
    # Use original_range attribute
    min_val <- original_range_attr[1]
    max_val <- original_range_attr[2]
    original_range <- max_val - min_val
  } else if (!is.null(original_data)) {
    # Use provided original_data
    if (!is.numeric(original_data) && !is.matrix(original_data) && !is.data.frame(original_data)) {
      stop("original_data must be numeric, matrix, or data frame")
    }
    
    if (is.data.frame(original_data)) {
      # For data frames, only use numeric columns
      numeric_cols <- sapply(original_data, is.numeric)
      if (!any(numeric_cols)) {
        stop("original_data must contain at least one numeric column")
      }
      original_data <- as.matrix(original_data[numeric_cols])
    }
    
    if (length(original_data) == 0) {
      stop("original_data cannot be empty")
    }
    
    if (any(is.infinite(original_data))) {
      original_data[is.infinite(original_data)] <- NA
      warning("original_data contained infinite values, converted to NA")
    }
    
    min_val <- min(original_data, na.rm = na.rm)
    max_val <- max(original_data, na.rm = na.rm)
    original_range <- max_val - min_val
  } else {
    stop("Either original_data must be provided or scaled_value must have scaling attributes")
  }
  
  # Check for zero range
  if (abs(original_range) < tolerance) {
    warning("Original data has zero range; returning constant values")
    if (is.matrix(scaled_value)) {
      return(matrix(min_val, nrow = nrow(scaled_value), ncol = ncol(scaled_value)))
    } else if (is.data.frame(scaled_value)) {
      result <- scaled_value
      result[] <- min_val
      return(result)
    } else {
      return(rep(min_val, length(scaled_value)))
    }
  }
  
  # Handle different input types
  if (is.data.frame(scaled_value)) {
    result <- as.data.frame(lapply(scaled_value, function(col) {
      if (is.numeric(col)) {
        col * original_range + min_val
      } else {
        col
      }
    }))
    return(result)
  }
  
  # Store original structure for matrices
  if (is.matrix(scaled_value)) {
    original_dim <- dim(scaled_value)
    original_dimnames <- dimnames(scaled_value)
    scaled_vector <- as.vector(scaled_value)
  } else {
    scaled_vector <- scaled_value
  }
  
  # Perform backtransformation
  backtransformed_value <- scaled_vector * original_range + min_val
  
  # Restore matrix structure if needed
  if (exists("original_dim")) {
    backtransformed_value <- matrix(backtransformed_value, 
                                   nrow = original_dim[1], 
                                   ncol = original_dim[2])
    dimnames(backtransformed_value) <- original_dimnames
  }
  
  return(backtransformed_value)
}

#' Scale New Values Using Reference Data Parameters
#'
#' @description
#' Scales new values to a 0-1 range using the minimum and range of reference data
#' or scaling parameters. This is useful when you need to scale values that weren't 
#' part of the original dataset using the same transformation parameters from 
#' \code{\link{scale_values}}.
#'
#' @param value A numeric vector, matrix, or data frame of values to be scaled.
#' @param data A numeric vector, matrix, data frame, or \code{scaled_values} object 
#'   used to determine scaling parameters. Can also be omitted if scaling parameters 
#'   are provided directly.
#' @param min_val Numeric. Minimum value for scaling (optional, extracted from data if not provided)
#' @param max_val Numeric. Maximum value for scaling (optional, extracted from data if not provided)
#' @param na.rm Logical. Should missing values be removed when computing min/max? (default: TRUE)
#' @param tolerance Numeric. Values below this threshold are considered zero (default: 1e-10)
#'
#' @return A numeric vector, matrix, or data frame of scaled values, typically between 0 and 1.
#'   Values outside the original data range may fall outside 0-1.
#'
#' @details
#' The scaling uses the formula: 
#' \deqn{x_{scaled} = \frac{x - min}{max - min}}
#' 
#' The function can extract scaling parameters from:
#' \itemize{
#'   \item Attributes of \code{scaled_values} objects
#'   \item The provided reference \code{data}
#'   \item Direct specification via \code{min_val} and \code{max_val}
#' }
#' 
#' Note that if \code{value} contains values outside the range of the reference 
#' data, the resulting scaled values may be outside the 0-1 range.
#'
#' @examples
#' # Using with scale_values output
#' reference_data <- c(10, 20, 30, 40, 50)
#' scaled_ref <- scale_values(reference_data)
#' new_values <- c(15, 25, 35, 45)
#' get_scaled_value(new_values, scaled_ref)
#' 
#' # Using reference data directly
#' get_scaled_value(c(5, 25, 45), reference_data)
#' 
#' # Using explicit min/max values
#' get_scaled_value(c(15, 35), min_val = 10, max_val = 50)
#' 
#' # With matrices
#' ref_matrix <- matrix(1:6, nrow = 2)
#' scaled_matrix <- scale_values(ref_matrix)
#' new_matrix <- matrix(c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5), nrow = 2)
#' get_scaled_value(new_matrix, scaled_matrix)
#'
#' @seealso \code{\link{scale_values}}, \code{\link{backtransform_scaled_values}}
#'
#' @export
get_scaled_value <- function(value, 
                            data = NULL, 
                            min_val = NULL, 
                            max_val = NULL,
                            na.rm = TRUE,
                            tolerance = 1e-10) {
  
  # Input validation
  if (!is.numeric(value) && !is.matrix(value) && !is.data.frame(value)) {
    stop("value must be numeric, matrix, or data frame")
  }
  
  if (!is.logical(na.rm)) {
    stop("'na.rm' must be logical (TRUE/FALSE)")
  }
  
  if (!is.numeric(tolerance) || tolerance <= 0) {
    stop("'tolerance' must be a positive numeric value")
  }
  
  # Extract scaling parameters
  if (!is.null(min_val) && !is.null(max_val)) {
    # Use provided min/max values
    if (!is.numeric(min_val) || !is.numeric(max_val)) {
      stop("min_val and max_val must be numeric")
    }
    if (length(min_val) != 1 || length(max_val) != 1) {
      stop("min_val and max_val must be single values")
    }
  } else {
    # Extract from data
    if (is.null(data)) {
      stop("Either 'data' or both 'min_val' and 'max_val' must be provided")
    }
    
    # Try to extract from attributes first
    scaling_info <- attr(data, "scaling_info")
    original_range_attr <- attr(data, "original_range")
    
    if (!is.null(scaling_info)) {
      min_val <- scaling_info$min
      max_val <- scaling_info$max
    } else if (!is.null(original_range_attr)) {
      min_val <- original_range_attr[1]
      max_val <- original_range_attr[2]
    } else {
      # Extract from raw data
      if (!is.numeric(data) && !is.matrix(data) && !is.data.frame(data)) {
        stop("data must be numeric, matrix, data frame, or scaled_values object")
      }
      
      if (is.data.frame(data)) {
        numeric_cols <- sapply(data, is.numeric)
        if (!any(numeric_cols)) {
          stop("data must contain at least one numeric column")
        }
        data <- as.matrix(data[numeric_cols])
      }
      
      if (length(data) == 0) {
        stop("data cannot be empty")
      }
      
      if (any(is.infinite(data))) {
        data[is.infinite(data)] <- NA
        warning("data contained infinite values, converted to NA")
      }
      
      min_val <- min(data, na.rm = na.rm)
      max_val <- max(data, na.rm = na.rm)
    }
  }
  
  # Calculate range
  data_range <- max_val - min_val
  
  # Check for zero range
  if (abs(data_range) < tolerance) {
    warning("Reference data has zero range; scaling is undefined. Returning NA values.")
    if (is.matrix(value)) {
      return(matrix(NA_real_, nrow = nrow(value), ncol = ncol(value)))
    } else if (is.data.frame(value)) {
      result <- value
      result[sapply(result, is.numeric)] <- NA_real_
      return(result)
    } else {
      return(rep(NA_real_, length(value)))
    }
  }
  
  # Check for values outside reference range
  if (any(value < min_val | value > max_val, na.rm = TRUE)) {
    warning("Some values are outside the reference data range and will be scaled outside [0,1]")
  }
  
  # Handle different input types
  if (is.data.frame(value)) {
    result <- as.data.frame(lapply(value, function(col) {
      if (is.numeric(col)) {
        (col - min_val) / data_range
      } else {
        col
      }
    }))
    return(result)
  }
  
  # Store original structure for matrices
  if (is.matrix(value)) {
    original_dim <- dim(value)
    original_dimnames <- dimnames(value)
    value_vector <- as.vector(value)
  } else {
    value_vector <- value
  }
  
  # Perform scaling
  scaled_value <- (value_vector - min_val) / data_range
  
  # Restore matrix structure if needed
  if (exists("original_dim")) {
    scaled_value <- matrix(scaled_value, 
                          nrow = original_dim[1], 
                          ncol = original_dim[2])
    dimnames(scaled_value) <- original_dimnames
  }
  
  return(scaled_value)
}