#' Calculate Matrix or Vector Sparsity Proportions
#'
#' @description Calculates and displays the proportion of zero and non-zero elements 
#' in a matrix or numeric vector. This function is useful for determining the sparsity level, 
#' which can be important for computational efficiency and storage considerations.
#'
#' @param x A numeric matrix or vector
#' @param digits Integer indicating the number of decimal places to round to (default: 2)
#' @param silent Logical; if TRUE, returns results instead of printing them (default: FALSE)
#'
#' @return If silent=TRUE, returns a named list with components:
#' \itemize{
#'   \item density - proportion of non-zero elements
#'   \item sparsity - proportion of zero elements
#' }
#' Otherwise prints the percentages and returns invisible(NULL).
#'
#' @details The function calculates sparsity by counting the number of zero elements 
#' and dividing by the total number of elements. The density (proportion of non-zero 
#' elements) is calculated as 1 minus the sparsity.
#'
#' @examples
#' # Create a sparse matrix
#' m <- matrix(c(0,0,1,0,2,0,0,0,3), nrow=3)
#' sporp(m)
#' 
#' # Using with a vector
#' v <- c(0, 1, 0, 0, 2, 0)
#' sporp(v)
#' 
#' # Return values instead of printing
#' result <- sporp(m, silent=TRUE)
#' print(result$density)
#' 
#' # Using with random sparse matrix
#' m2 <- matrix(sample(c(0,1), 100, replace=TRUE, prob=c(0.8,0.2)), nrow=10)
#' sporp(m2, digits=3)
#'
#' @seealso \code{\link{matrix}}, \code{\link{vector}}
#'
#' @export
sporp <- function(x, digits = 2, silent = FALSE){
    # Input validation
    if (!is.numeric(x)) 
        stop("input must be numeric")
    
    # Calculate proportions
    nonZeros <- sum(x != 0)
    n <- length(x)
    density <- nonZeros/n
    sparsity <- 1 - density
    
    # Round results
    density <- round(density * 100, digits)
    sparsity <- round(sparsity * 100, digits)
    
    if (silent) {
        return(list(
            density = density/100,
            sparsity = sparsity/100
        ))
    } else {
        cat(sprintf("non-zero values = %.*f%%\nzero values = %.*f%%\n",
                    digits, density,
                    digits, sparsity))
        return(invisible(NULL))
    }
}