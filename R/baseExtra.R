#' Enhanced Names Function
#'
#' @description Gets names from an object, handling both regular R objects and S4 
#' objects. For S4 objects, it returns both slot names and any regular names.
#'
#' @param x An R object
#' @param verbose Logical; if TRUE, prints the object's structure type (default: TRUE)
#'
#' @return A list containing names and/or slot names, with attribute indicating structure type
#'
#' @examples
#' # Regular list
#' x <- list(a=1, b=2)
#' get_names(x)
#'
#' # S4 object example
#' setClass("Person", slots=list(name="character", age="numeric"))
#' john <- new("Person", name="John", age=30)
#' get_names(john)
#'
#' @export
get_names <- function(x, verbose=TRUE) {
    # Initialize result list
    result <- list()
    
    # Determine object type
    is_s4 <- isS4(x)
    has_names <- !is.null(names(x))
    
    # Get appropriate names
    if (is_s4) {
        result$slot_names <- slotNames(x)
        if (has_names) result$names <- names(x)
        attr(result, "type") <- "S4"
    } else {
        if (has_names) {
            result$names <- names(x)
            attr(result, "type") <- "regular"
        } else {
            result <- NULL
            attr(result, "type") <- "unnamed"
        }
    }
    
    # Print information if verbose
    if (verbose) {
        type <- attr(result, "type")
        cat(sprintf("Object type: %s\n", type))
        
        if (type == "S4") {
            cat("Slot names:", paste(result$slot_names, collapse=", "), "\n")
            if (has_names) cat("Names:", paste(result$names, collapse=", "), "\n")
        } else if (type == "regular") {
            cat("Names:", paste(result$names, collapse=", "), "\n")
        } else {
            cat("No names found\n")
        }
    }
    
    return(invisible(result))
}

#from toupper doc examples
capwords <- function(s, strict = FALSE) {
    s <- as.character(s)
    
    cap <- function(s) paste(toupper(substring(s, 1, 1)),
                  {s <- substring(s, 2); if(strict) tolower(s) else s},
                             sep = "", collapse = " " )
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}