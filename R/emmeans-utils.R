#' Bind Multiple Estimated Marginal Means into a Table
#'
#' @description
#' Combines multiple emmeans objects into a single formatted table, with model names
#' as identifiers. The output is compatible with knitr/quarto rendering.
#'
#' @param emm List of emmeans objects. Names of the list elements are used as model identifiers.
#' @param digits Integer. Number of decimal places for numeric columns (default: 2)
#' @param format Character. Output format: "markdown", "html", or "latex" (default: "markdown")
#' @param caption Character. Optional table caption
#' @param col.names Character vector. Custom column names
#'
#' @return A knitr_kable object that can be directly used in R Markdown or Quarto
#'
#' @examples
#' \dontrun{
#' # Create multiple emmeans objects
#' emm1 <- emmeans(model1, ~treatment)
#' emm2 <- emmeans(model2, ~treatment)
#'
#' # Combine into table
#' emm_table <- emm_bind(
#'   list("Model 1" = emm1, "Model 2" = emm2),
#'   digits = 3,
#'   caption = "Estimated Marginal Means Comparison"
#' )
#'
#' # Print table
#' emm_table
#' }
#'
#' @export
#' @importFrom knitr kable
#' @importFrom emmeans emmeans
emm_bind <- function(
  emm,
  digits = 2,
  format = "markdown",
  caption = NULL,
  col.names = NULL,
  kable = TRUE
) {
  # Input validation
  if (!is.list(emm)) {
    stop("'emm' must be a list of emmeans objects")
  }

  if (length(emm) == 0) {
    stop("Empty list provided")
  }

  if (!all(sapply(emm, inherits, "emmGrid"))) {
    stop("All elements must be emmeans objects (class 'emmGrid')")
  }

  # Match format argument
  format <- match.arg(format)

  # Convert each emmeans object to data frame
  df_list <- lapply(emm, function(x) {
    tryCatch(
      {
        as.data.frame(x)
      },
      error = function(e) {
        stop("Error converting emmeans to data frame: ", e$message)
      }
    )
  })

  # Check for consistent columns
  col_names <- lapply(df_list, colnames)
  if (length(unique(lapply(col_names, length))) > 1) {
    stop("Inconsistent number of columns in emmeans objects")
  }

  # Create results data frame
  results <- data.frame(
    Model = rep(names(df_list), sapply(df_list, nrow)),
    do.call(rbind, df_list),
    check.names = FALSE
  )

  # Round numeric columns to specified digits
  numeric_cols <- sapply(results, is.numeric)
  results[numeric_cols] <- round(results[numeric_cols], digits)

  # Use custom column names if provided
  if (!is.null(col.names)) {
    if (length(col.names) != ncol(results)) {
      stop("Length of col.names must match number of columns")
    }
    colnames(results) <- col.names
  }

  # Create kable with appropriate format
  if(kable){
    table <- knitr::kable(
      results,
      format = format,
      digits = digits,
      caption = caption,
      booktabs = TRUE
    )
  } else {
    table <- results
  }

  # Add class for potential method dispatch
  class(table) <- c("emm_table", class(table))

  return(table)
}

#' Extract Model Matrix from emmeans Contrast Object
#'
#' @description
#' Extracts and returns the model matrix from an emmeans contrast object. This function
#' provides a convenient way to access the underlying model matrix used in the contrast
#' calculations.
#'
#' @param model An object of class 'emmGrid' resulting from a call to emmeans::contrast()
#'
#' @return A matrix containing the model matrix from the emmeans contrast.
#'   If the model matrix is not available, returns NULL with a warning.
#'
#' @details
#' The function accesses the model matrix stored in the 'model.info' slot of an emmeans
#' contrast object. The output is useful for further statistical analysis or visualization
#' purposes and can be easily integrated into both HTML and PDF outputs when using Quarto.
#'
#' @examples
#' \dontrun{
#' # Fit a model
#' mod <- lm(weight ~ group, data = PlantGrowth)
#' 
#' # Calculate emmeans and contrasts
#' library(emmeans)
#' emm <- emmeans(mod, "group")
#' contrasts <- contrast(emm)
#' 
#' # Extract model matrix
#' matrix <- get_model_matrix(contrasts)
#' }
#'
#' @export
#' @importFrom methods is
get_model_matrix <- function(model) {
  # Input validation
  if (is.null(model)) {
    stop("Input model cannot be NULL")
  }
  
  if (!methods::is(model, "emmGrid")) {
    stop("Input must be an emmeans contrast object (class 'emmGrid')")
  }
  
  # Check if model.info exists and contains model.matrix
  if (!exists("model.info", model) || 
      is.null(model@model.info) || 
      is.null(model@model.info$model.matrix)) {
    warning("Model matrix not found in the provided emmeans contrast object")
    return(NULL)
  }
  
  # Convert to a standard matrix if not already
  matrix <- as.matrix(model@model.info$model.matrix)
  
  # Add attributes for better printing in various formats
  attr(matrix, "title") <- "Model Matrix from emmeans Contrast"
  class(matrix) <- c("model_matrix", class(matrix))
  
  return(matrix)
}

#' Print method for model_matrix objects
#'
#' @param x A model_matrix object
#' @param ... Additional arguments passed to print
#'
#' @export
print.model_matrix <- function(x, ...) {
  cat("Model Matrix from emmeans Contrast:\n\n")
  NextMethod()
}

#' Format method for model_matrix objects in knitr
#'
#' @param x A model_matrix object
#' @param ... Additional arguments passed to knitr::kable
#'
#' @export
knit_print.model_matrix <- function(x, ...) {
  if (requireNamespace("knitr", quietly = TRUE)) {
    return(knitr::kable(x, caption = attr(x, "title"), ...))
  }
  print(x)
}