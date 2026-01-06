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

#' Safely Load R Packages with Graceful Error Handling
#'
#' This function attempts to load R packages while providing informative warnings
#' for missing packages instead of stopping execution. It's designed to work well
#' in reproducible analysis environments and with document rendering systems like
#' Quarto or R Markdown.
#'
#' @param package_name Character vector of package names to load. Can be a single
#'   package name or multiple package names.
#' @param quietly Logical. If \code{TRUE}, suppresses startup messages from packages.
#'   Default is \code{TRUE}.
#' @param warn_missing Logical. If \code{TRUE}, issues warnings for missing packages.
#'   Default is \code{TRUE}.
#' @param install_hint Logical. If \code{TRUE}, includes installation instructions
#'   in warning messages. Default is \code{TRUE}.
#' @param return_status Logical. If \code{TRUE}, returns detailed status information.
#'   Default is \code{FALSE} for backwards compatibility.
#'
#' @return If \code{return_status = FALSE}: Logical vector indicating success/failure
#'   for each package (invisibly returned to avoid cluttering output).
#'   If \code{return_status = TRUE}: A named list with components:
#'   \itemize{
#'     \item \code{loaded}: Character vector of successfully loaded packages
#'     \item \code{missing}: Character vector of missing packages
#'     \item \code{success}: Named logical vector indicating status for each package
#'   }
#'
#' @details
#' This function is particularly useful in shared analysis scripts or documents
#' where package availability may vary across systems. It allows scripts to
#' continue running even when some packages are unavailable, with appropriate
#' warnings to users.
#'
#' The function handles various edge cases including empty inputs, non-character
#' inputs, and already-loaded packages.
#'
#' @examples
#' \dontrun{
#' # Load a single package
#' safe_library("dplyr")
#'
#' # Load multiple packages
#' safe_library(c("dplyr", "ggplot2", "nonexistent_package"))
#'
#' # Load packages with detailed status
#' result <- safe_library(c("dplyr", "ggplot2"), return_status = TRUE)
#' print(result$missing)
#'
#' # Load packages quietly without install hints
#' safe_library("dplyr", install_hint = FALSE)
#' }
#'
#' @seealso \code{\link[base]{library}}, \code{\link[base]{requireNamespace}}
#'
#' @author Your Name
#' @export
safe_library <- function(
    package_name,
    quietly = TRUE,
    warn_missing = TRUE,
    install_hint = TRUE,
    return_status = FALSE
) {
    # Input validation
    if (missing(package_name) || is.null(package_name)) {
        stop("package_name cannot be missing or NULL", call. = FALSE)
    }

    if (!is.character(package_name)) {
        stop("package_name must be a character vector", call. = FALSE)
    }

    if (length(package_name) == 0) {
        warning("No packages specified", call. = FALSE)
        if (return_status) {
            return(list(
                loaded = character(0),
                missing = character(0),
                success = logical(0)
            ))
        }
        return(invisible(logical(0)))
    }

    # Remove empty strings and duplicates
    package_name <- unique(package_name[nzchar(package_name)])

    if (length(package_name) == 0) {
        warning("No valid package names provided", call. = FALSE)
        if (return_status) {
            return(list(
                loaded = character(0),
                missing = character(0),
                success = logical(0)
            ))
        }
        return(invisible(logical(0)))
    }

    # Validate other parameters
    stopifnot(
        "quietly must be logical" = is.logical(quietly) && length(quietly) == 1,
        "warn_missing must be logical" = is.logical(warn_missing) &&
            length(warn_missing) == 1,
        "install_hint must be logical" = is.logical(install_hint) &&
            length(install_hint) == 1,
        "return_status must be logical" = is.logical(return_status) &&
            length(return_status) == 1
    )

    # Initialize results
    success <- setNames(logical(length(package_name)), package_name)
    loaded_packages <- character(0)
    missing_packages <- character(0)

    # Process each package
    for (pkg in package_name) {
        if (requireNamespace(pkg, quietly = TRUE)) {
            # Package is available, try to load it
            tryCatch(
                {
                    if (quietly) {
                        suppressWarnings(suppressMessages(library(
                            pkg,
                            character.only = TRUE
                        )))
                    } else {
                        library(pkg, character.only = TRUE)
                    }
                    success[pkg] <- TRUE
                    loaded_packages <- c(loaded_packages, pkg)
                },
                error = function(e) {
                    success[pkg] <- FALSE
                    missing_packages <- c(missing_packages, pkg)
                    if (warn_missing) {
                        msg <- paste(
                            "Failed to load package",
                            pkg,
                            "-",
                            e$message
                        )
                        warning(msg, call. = FALSE, immediate. = TRUE)
                    }
                }
            )
        } else {
            # Package not available
            success[pkg] <- FALSE
            missing_packages <- c(missing_packages, pkg)

            if (warn_missing) {
                msg <- paste("Package '", pkg, "' is not installed.", sep = "")
                if (install_hint) {
                    msg <- paste(
                        msg,
                        "Install with: install.packages('",
                        pkg,
                        "')",
                        sep = " "
                    )
                }
                msg <- paste(msg, "Some functions may not work properly.")
                warning(msg, call. = FALSE, immediate. = TRUE)
            }
        }
    }

    # Return results
    if (return_status) {
        result <- list(
            loaded = loaded_packages,
            missing = missing_packages,
            success = success
        )
        return(result)
    } else {
        return(invisible(success))
    }
}

#' Load Multiple Packages with Summary Report
#'
#' A wrapper around \code{safe_library} that provides a formatted summary
#' of package loading results, suitable for inclusion in rendered documents.
#'
#' @param packages Character vector of package names to load.
#' @param print_summary Logical. If \code{TRUE}, prints a formatted summary.
#'   Default is \code{TRUE}.
#' @param return_details Logical. If \code{TRUE}, returns detailed results.
#'   Default is \code{FALSE}.
#' @param ... Additional arguments passed to \code{safe_library}.
#'
#' @return Invisibly returns the result from \code{safe_library} with
#'   \code{return_status = TRUE}.
#'
#' @examples
#' \dontrun{
#' # Load packages with summary (good for Quarto/R Markdown documents)
#' load_packages(c("dplyr", "ggplot2", "tidyr"))
#'
#' # Load packages silently
#' load_packages(c("dplyr", "ggplot2"), print_summary = FALSE)
#' }
#'
#' @export
load_packages <- function(
    packages,
    print_summary = TRUE,
    return_details = FALSE,
    ...
) {
    result <- safe_library(packages, return_status = TRUE, ...)

    if (print_summary) {
        cat("\n")
        cat("📦 Package Loading Summary\n")
        cat("========================\n")

        if (length(result$loaded) > 0) {
            cat("✅ Successfully loaded:", length(result$loaded), "packages\n")
            cat("   ", paste(result$loaded, collapse = ", "), "\n")
        }

        if (length(result$missing) > 0) {
            cat("❌ Missing packages:", length(result$missing), "\n")
            cat("   ", paste(result$missing, collapse = ", "), "\n")
            cat(
                "   Install with: install.packages(c(",
                paste(paste0("'", result$missing, "'"), collapse = ", "),
                "))\n"
            )
        }

        if (length(result$missing) == 0) {
            cat("🎉 All packages loaded successfully!\n")
        }
        cat("\n")
    }

    if (return_details) {
        return(result)
    } else {
        return(invisible(result))
    }
}