#' Test for Normality of Data Frame Columns
#'
#' @description Performs Shapiro-Wilk normality test on a specified numeric column
#' of a data frame and returns formatted results suitable for Quarto/RMarkdown documents.
#'
#' @param df A data frame containing the data to test
#' @param j Column name or index to test for normality
#' @param format Output format for the table. One of "markdown",
#' "html", "latex" (default: "markdown")
#' @param digits Number of decimal places for p-value (default: 4)
#' @param alpha Significance level for normality test (default: 0.05)
#'
#' @return A formatted kable object containing test results. The table includes:
#' \itemize{
#'   \item Name: Name of the tested variable
#'   \item W: Shapiro-Wilk test statistic
#'   \item P_value: P-value of the test
#' }
#'
#' @details
#' The function performs a Shapiro-Wilk test for normality on the specified column.
#' Results are formatted using kable() for clean output in RMarkdown/Quarto documents.
#' The null hypothesis is that the data is normally distributed.
#'
#' @note
#' The Shapiro-Wilk test is most appropriate for sample sizes between 3 and 5000.
#'
#' @examples
#' # Create example data
#' df <- data.frame(
#'   normal = rnorm(100),
#'   uniform = runif(100),
#'   categorical = factor(rep(1:4, 25))
#' )
#'
#' # Test normally distributed data
#' norm_test(df, "normal")
#'
#' # Test non-normal data
#' norm_test(df, "uniform")
#'
#' # Test with different format
#' norm_test(df, "normal", format = "html")
#'
#' @importFrom stats shapiro.test
#' @importFrom knitr kable
#' @export
norm_test <- function(df, j, format = "markdown", digits = 4, alpha = 0.05) {
    # Input validation
    if (!is.data.frame(df)) {
        stop("First argument must be a data frame")
    }

    if (is.character(j) && !(j %in% names(df))) {
        stop("Column '", j, "' not found in data frame")
    }

    if (is.numeric(j) && (j < 1 || j > ncol(df))) {
        stop("Column index out of bounds")
    }

    # Extract column
    y <- df[[j]]
    col_name <- if(is.character(j)) j else names(df)[j]

    # Check if numeric
    if (!is.numeric(y)) {
        warning("Column '", col_name, "' is not numeric. Skipping test.")
        return(NULL)
    }

    # Check sample size
    if (length(y) < 3 || length(y) > 5000) {
        warning("Sample size (", length(y), ") is outside recommended range (3-5000) for Shapiro-Wilk test")
    }

    # Remove NA values
    y <- na.omit(y)
    if (length(y) == 0) {
        warning("No non-missing values in column '", col_name, "'")
        return(NULL)
    }

    # Perform test
    norm_test <- shapiro.test(y)

    # Create results data frame
    norm_stats <- data.frame(
        "Variable" = col_name,
        "n" = length(y),
        "W" = norm_test$statistic,
        "P_value" = round(norm_test$p.value, digits),
        "Distribution" = ifelse(norm_test$p.value > alpha,
                              "Likely Normal",
                              "Likely Non-normal"),
        check.names = FALSE
    )

    # Create caption
    caption <- sprintf(
        "Shapiro-Wilk Normality Test Results (α = %.2f)",
        alpha
    )

    # Format table
    table <- knitr::kable(norm_stats,
        format = format,
        caption = caption,
        align = c('l', 'r', 'r', 'r', 'l'),
        booktabs = TRUE,
        digits = digits
    )

    # Add styling if format is latex and kableExtra is available
    if (format == "latex") {
        if (requireNamespace("kableExtra", quietly = TRUE)) {
            table <- kableExtra::kable_styling(table,
                bootstrap_options = c("striped", "hover"),
                full_width = FALSE
            )
        }
    }

    return(table)
}
