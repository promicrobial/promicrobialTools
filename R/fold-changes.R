#' Calculate Fold Change from Raw or Log-Transformed Values
#'
#' @description
#' Calculates fold change between two values or from a log-transformed value.
#' Returns signed fold changes where negative values indicate decreases and
#' positive values indicate increases.
#'
#' @param a Numeric. For raw counts: first value to compare. For log-transformed
#'   data: the log fold change value.
#' @param b Numeric. Optional second value for raw count comparison. Default: NULL
#' @param log_base Numeric. Optional base of logarithm for log-transformed data.
#'   Default: NULL
#'
#' @return Numeric. Signed fold change value:
#'   \itemize{
#'     \item Positive values indicate increase (e.g., 4 means 4-fold increase)
#'     \item Negative values indicate decrease (e.g., -4 means 4-fold decrease)
#'     \item \code{Inf} or \code{-Inf} for zero denominators
#'     \item \code{NA} for missing values
#'     \item \code{NaN} for 0/0 cases
#'   }
#'
#' @details
#' The function handles two types of calculations:
#' \enumerate{
#'   \item Raw counts: When \code{b} is provided, calculates \code{a/b} or \code{-b/a}
#'   \item Log-transformed: When \code{log_base} is provided, calculates
#'     \code{\u00B1log_base^|a|}
#' }
#'
#' If both \code{b} and \code{log_base} are provided, uses raw counts and issues
#' a warning.
#'
#' @examples
#' # Raw counts
#' fc(100, 25)      # Returns 4 (4-fold increase)
#' fc(25, 100)      # Returns -4 (4-fold decrease)
#' fc(0, 100)       # Returns -Inf
#' fc(100, 0)       # Returns Inf
#'
#' # Log2 transformed data
#' fc(2, log_base = 2)     # Returns 4 (from log2(4) = 2)
#' fc(-2, log_base = 2)    # Returns -4 (from log2(4) = -2)
#'
#' # Log10 transformed data
#' fc(1, log_base = 10)    # Returns 10 (from log10(10) = 1)
#' fc(-1, log_base = 10)   # Returns -10 (from log10(10) = -1)
#'
#' @seealso
#' \code{\link{log}}, \code{\link{sign}}
#'
#' @export
fc <- function(a, b = NULL, log_base = NULL) {
  # Input validation
  if (!is.null(a) && !is.numeric(a)) {
    stop("Value 'a' should be numeric")
  }
  if (is.null(b) && is.null(log_base)) {
    stop("Either 'b' or 'log_base' must be provided")
  }
  # When all arguments are supplied, warn and use raw counts
  if (!is.null(b) && !is.null(log_base)) {
    warning("Both 'b' and 'log_base' supplied; using raw counts (ignoring log_base)")
    log_base <- NULL
  }
  if (!is.null(log_base)) {
    if (!is.numeric(log_base) || log_base <= 0) {
      stop("log_base must be a positive number")
    }
  }
  
  # Handle NA values
  if (is.null(log_base)) {
    if (is.na(a) || is.na(b)) return(NA)
  } else {
    if (is.na(a)) return(NA)
  }
  
  # Calculate fold change
  if (is.null(log_base)) {
    # Raw counts
    if (b == 0) return(Inf * sign(a))
    if (a == 0) return(-Inf * sign(b))
    return(if(a > b) a/b else -b/a)
  } else {
    # Log-transformed data
    fc <- log_base^(abs(a))
    return(if(a >= 0) fc else -fc)
  }
}
# Helper function for NULL coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x