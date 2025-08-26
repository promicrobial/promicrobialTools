#' Create Tertiles from a Continuous Variable
#'
#' @description
#' Divides a continuous variable into three groups (tertiles) while ensuring that
#' equal values are not split across different tertiles. Returns both the categorized
#' data and a summary of the tertile distribution.
#'
#' @param continuous_variable A numeric vector to be divided into tertiles
#'
#' @return A list containing:
#'   \itemize{
#'     \item tertiles: An ordered factor with levels "Low", "Medium", "High"
#'     \item summary: A data frame with columns:
#'       \itemize{
#'         \item Tertile: Character indicating tertile level
#'         \item N: Integer count of observations
#'         \item Percent: Numeric percentage of observations
#'       }
#'     \item breaks: Numeric vector of break points (excluding -Inf and Inf)
#'   }
#'
#' @examples
#' set.seed(123)
#' x <- rnorm(100)
#' result <- tertiles(x)
#'
#' @export
#'
#' @note
#' The function ensures that equal values are not split across tertiles,
#' which may result in groups that deviate from exact thirds.
#'
#' @seealso
#' \code{\link{cut}}, \code{\link{quantile}}
#'
#' @importFrom stats quantile
#' @importFrom knitr kable
#'
tertiles <- function(continuous_variable) {
  # Input validation
  if (!is.numeric(continuous_variable)) {
    stop("Input must be a numeric vector")
  }
  
  if (length(continuous_variable) < 3) {
    stop("Input vector must have at least 3 values to create tertiles")
  }
  
  # Remove NA values
  x <- continuous_variable[!is.na(continuous_variable)]
  
  if (length(x) == 0) {
    stop("No non-NA values in input vector")
  }
  
  # Get unique values and sort them
  unique_values <- sort(unique(x))
  
  # Calculate cumulative percentages for unique values
  cum_pct <- cumsum(table(x))/length(x)
  
  # Initialize tertile breaks
  breaks <- c(-Inf, Inf)  # Default breaks if logic fails
  
  # Find the closest values to 33.33% and 66.67% that don't split equal values
  if (length(unique_values) > 2) {
    # Find break points that don't split equal values
    lower_third <- unique_values[which.min(abs(cum_pct - 1/3))]
    upper_third <- unique_values[which.min(abs(cum_pct - 2/3))]
    
    breaks <- c(-Inf, lower_third, upper_third, Inf)
  }
  
  # Create tertiles
  tertiles <- cut(continuous_variable, 
                  breaks = breaks,
                  labels = c("Low", "Medium", "High"),
                  include.lowest = TRUE,
                  ordered = TRUE)  # Make it an ordered factor
  
  # Create summary statistics
  summary_stats <- data.frame(
    Tertile = c("Low", "Medium", "High"),
    N = as.numeric(table(tertiles)),
    Percent = round(as.numeric(prop.table(table(tertiles))*100), 1)
  )
    
    # Print summary
    print("Tertile Distribution:")
    print(summary_stats)
    print("Break Points:")
    print(breaks[-c(1,length(breaks))])  # Don't show -Inf and Inf
    
    return(tertiles)
}
