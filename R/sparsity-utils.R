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

#' Generate Random Sparse Matrix or Vector
#'
#' @description Creates a random sparse matrix or vector with user-specified density 
#' and value range. Values are uniformly distributed between min and max, with zeros 
#' inserted to achieve the desired sparsity.
#'
#' @param n numeric. Total number of observations to generate
#' @param min numeric. Minimum value for random numbers (default: 0)
#' @param max numeric. Maximum value for random numbers (default: 1)
#' @param d numeric. Desired density as a proportion (e.g., 10 means 10% non-zero values) (default: 10)
#' @param mat logical. If TRUE returns a matrix, if FALSE returns a vector (default: TRUE)
#' @param nrow numeric. Number of rows for matrix output. If NULL, defaults to 10 (default: NULL)
#'
#' @return If mat=TRUE, returns a matrix with dimensions nrow x (n/nrow). 
#' If mat=FALSE, returns a vector of length n. Both contain random values between 
#' min and max, with d% of elements being nonzero.
#'
#' @examples
#' # Generate a sparse vector with 100 elements, 10% nonzeros
#' randspar(n = 100, mat = FALSE)
#'
#' # Generate a 5x20 sparse matrix with 20% nonzeros
#' randspar(n = 100, d = 20, nrow = 5)
#'
#' # Generate a sparse matrix with values between -1 and 1
#' randspar(n = 100, min = -1, max = 1)
#'
#' @export
randspar <- function(n, min = 0, max = 1, d = 10, mat = TRUE, nrow = NULL){
    # Validate inputs
    if (!is.numeric(n) || n <= 0) 
        stop("'n' must be a positive number")
    if (!is.numeric(d) || d < 0 || d > 100) 
        stop("'d' must be between 0 and 100")
    if (min >= max) 
        stop("'min' must be less than 'max'")
    if (!is.null(nrow) && (nrow <= 0 || n %% nrow != 0))
        stop("'nrow' must be positive and 'n' must be divisible by 'nrow'")

    rand <- runif(n = n, min = min, max = max)
    d <- d/100 #density proportion i.e. proportion of non-zero values
    z <- n-(n*d)  #number of zeros needed to create matrix with proportion, p
    zeros <- rep.int(0,z)

    if(mat == TRUE) {
        if(!is.null(nrow)){
            if(n %% nrow != 0) {
                stop("'n' must be divisible by 'nrow' to create a matrix")
            }
            nrow <- nrow
        }
        else {
            nrow <- 10
            if(n %% nrow != 0) {
                stop("'n' must be divisible by default nrow (10). To change this set `nrow` manually.")
            }
        }
        m <- matrix(sample(c(sample(rand, size=n-z), zeros)), nrow = nrow)
    } 
    else {
        m <- sample(c(sample(rand, size=n-z), zeros))
    }
    
    mat[mat==0] <- 0 #make zero formatting consistent across matrix
    
    return(m)
}