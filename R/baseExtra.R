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

#' Capitalize Words in a String
#'
#' @description
#' Capitalizes the first letter of each word in a string or vector of strings.
#'
#' @param s A character vector whose words are to be capitalized.
#' @param strict Logical. If TRUE, converts remaining letters to lowercase.
#'              If FALSE (default), preserves the case of remaining letters.
#' @param split Character string containing a regular expression to use for 
#'             splitting. Default is " " (space).
#' @param preserve Character vector of words to preserve case for (e.g., "PhD").
#'
#' @return A character vector of the same length as the input with words capitalized.
#'
#' @examples
#' capwords("hello world")  # returns "Hello World"
#' capwords("hello WORLD", strict = TRUE)  # returns "Hello World"
#' capwords("hello-world", split = "-")  # returns "Hello-World"
#' capwords("PhD student", preserve = "PhD")  # returns "PhD Student"
#'
#' @export
#'
#' @seealso \code{\link[base]{toupper}}, \code{\link[base]{tolower}}
capwords <- function(s, strict = FALSE, split = " ", preserve = character()) {
    if (!is.character(s) && !is.factor(s)) {
        s <- as.character(s)
    }
    if (!is.logical(strict)) {
        stop("'strict' must be logical (TRUE/FALSE)")
    }
    
    cap <- function(words) {
        # Handle each word separately
        result <- sapply(words, function(word) {
            # Check if word should be preserved
            if (length(preserve) > 0 && word %in% preserve) {
                return(word)
            }
            # Capitalize first letter and handle the rest
            paste0(
                toupper(substring(word, 1, 1)),
                if(strict) tolower(substring(word, 2)) else substring(word, 2)
            )
        })
        paste(result, collapse = split)
    }
    
    # Handle NA values
    if (any(is.na(s))) {
        nas <- is.na(s)
        s[!nas] <- sapply(strsplit(s[!nas], split = split), 
                         cap, USE.NAMES = !is.null(names(s)))
        return(s)
    }
    
    sapply(strsplit(s, split = split), cap, USE.NAMES = !is.null(names(s)))
}