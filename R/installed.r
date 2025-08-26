#' List Installed R Packages with Version Information
#'
#' @description
#' Creates a formatted table of installed R packages, their versions, and R build
#' information. The output can be customized for different rendering contexts
#' (e.g., console, HTML, PDF) and is compatible with Quarto/RMarkdown documents.
#'
#' @param kable Logical. If TRUE, returns a formatted knitr_kable object. If FALSE,
#'   returns a data frame (default: FALSE)
#' @param format Character. Output format when kable=TRUE: "markdown", "html", "latex",
#'   or "rst" (default: "markdown")
#' @param include_base Logical. If TRUE, includes base R packages. If FALSE, shows
#'   only user-installed packages (default: TRUE)
#' @param caption Character. Optional caption for the table (default: NULL)
#' @param digits Integer. Number of digits for numeric columns (default: 0)
#'
#' @return If kable=TRUE, returns a knitr_kable object suitable for rendering in 
#'   Quarto/RMarkdown documents. Otherwise, returns a data frame with columns:
#'   \itemize{
#'     \item Package: Name of the installed package
#'     \item Version: Package version number
#'     \item Built_Under_R: R version the package was built under
#'     \item Type: Installation type (base, recommended, or user-installed)
#'   }
#'
#' @examples
#' # Get data frame of installed packages
#' pkg_df <- installed()
#'
#' # Create formatted table for HTML output
#' pkg_table <- installed(
#'   kable = TRUE,
#'   format = "html",
#'   include_base = FALSE,
#'   caption = "User-installed R Packages"
#' )
#'
#' @note
#' The function automatically handles special characters and encoding issues that
#' might affect rendering in different output formats.
#'
#' @seealso
#' \code{\link[utils]{installed.packages}}, \code{\link[knitr]{kable}}
#'
#' @export
#' @importFrom knitr kable
installed <- function(kable = FALSE,
                     format = "markdown",
                     include_base = TRUE,
                     caption = NULL,
                     digits = 0) {
  # Input validation
  if (!is.logical(kable)) {
    stop("'kable' must be logical (TRUE/FALSE)")
  }
  
  if (!is.logical(include_base)) {
    stop("'include_base' must be logical (TRUE/FALSE)")
  }
  
  format <- match.arg(format, c("markdown", "html", "latex", "rst"))
  
  # Get installed packages
  pkgs <- installed.packages()
  
  # Create initial data frame
  simplified_pkgs <- data.frame(
    Package = pkgs[, "Package"],
    Version = pkgs[, "Version"],
    Built_Under_R = pkgs[, "Built"],
    Type = pkgs[, "Priority"],
    stringsAsFactors = FALSE
  )
  
  # Clean up Type field
  simplified_pkgs$Type <- ifelse(is.na(simplified_pkgs$Type),
                                "user-installed",
                                simplified_pkgs$Type)
  
  # Filter base packages if requested
  if (!include_base) {
    simplified_pkgs <- simplified_pkgs[simplified_pkgs$Type == "user-installed", ]
  }
  
  # Sort alphabetically by package name
  simplified_pkgs <- simplified_pkgs[order(simplified_pkgs$Package), ]
  
  # Reset row names
  rownames(simplified_pkgs) <- NULL
  
  # Format output
  if (kable) {
    # Handle special characters for different output formats
    if (format %in% c("latex", "html")) {
      simplified_pkgs$Package <- gsub("_", "\\_", simplified_pkgs$Package)
    }
    
    # Create kable object with appropriate formatting
    formatted_pkgs <- knitr::kable(
      simplified_pkgs,
      format = format,
      caption = caption,
      digits = digits,
      booktabs = TRUE,
      escape = FALSE,
      row.names = FALSE
    )
    
    # Add class for potential method dispatch
    class(formatted_pkgs) <- c("package_table", class(formatted_pkgs))
    
    return(formatted_pkgs)
  } else {
    return(simplified_pkgs)
  }
}

#' Print method for package_table objects
#'
#' @param x A package_table object
#' @param ... Additional arguments passed to print
#'
#' @export
print.package_table <- function(x, ...) {
  cat("Installed R Packages Table:\n\n")
  NextMethod()
}

#' Custom knit_print method for package_table objects
#'
#' @param x A package_table object
#' @param ... Additional arguments passed to knitr::kable
#'
#' @export
knit_print.package_table <- function(x, ...) {
  if (requireNamespace("knitr", quietly = TRUE)) {
    return(x)
  }
  print(x)
}
