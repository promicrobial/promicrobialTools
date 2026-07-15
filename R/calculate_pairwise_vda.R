#' Calculate Pairwise Vargha-Delaney A Effect Sizes
#'
#' @description
#' Calculates the Vargha-Delaney A (VDA) effect size measure for all pairwise
#' combinations of groups in a dataset. VDA is a non-parametric effect size measure
#' that represents the probability that a randomly selected value from one group
#' will be greater than a randomly selected value from another group.
#'
#' @param data A data frame containing the measurements and group assignments
#' @param value_col Character. Name of the column containing numeric measurements
#' @param group_col Character. Name of the column containing group identifiers
#' @param digits Integer. Number of decimal places for VDA values (default: 3)
#' @param format Character. Output format: "default", "kable", or "gt" (default: "default")
#' @param include_n Logical. Whether to include sample sizes in output (default: TRUE)
#'
#' @return A data frame (or formatted table if specified) containing:
#' \itemize{
#'   \item group1: First group in comparison
#'   \item group2: Second group in comparison
#'   \item n1: Sample size of group1 (if include_n = TRUE)
#'   \item n2: Sample size of group2 (if include_n = TRUE)
#'   \item VDA: Vargha-Delaney A measure (0-1)
#'   \item effect_size: Interpretation of effect size
#' }
#'
#' @details
#' The VDA measure ranges from 0 to 1, where:
#' \itemize{
#'   \item 0.5 indicates stochastic equality
#'   \item >0.5 indicates group1 tends to have larger values
#'   \item <0.5 indicates group2 tends to have larger values
#' }
#'
#' Effect size thresholds:
#' \itemize{
#'   \item ≥0.71: Large positive
#'   \item ≥0.64: Medium positive
#'   \item ≥0.56: Small positive
#'   \item 0.44-0.56: Negligible
#'   \item ≤0.44: Small negative
#'   \item ≤0.36: Medium negative
#'   \item ≤0.29: Large negative
#' }
#'
#' @examples
#' # Create example data
#' df <- data.frame(
#'   value = c(rnorm(30), rnorm(30, 1)),
#'   group = rep(c("Control", "Treatment"), each = 30)
#' )
#'
#' # Basic usage
#' results <- calculate_pairwise_vda(df, "value", "group")
#'
#' # Formatted output for Quarto/RMarkdown
#' results_table <- calculate_pairwise_vda(
#'   df, "value", "group",
#'   format = "kable",
#'   digits = 2
#' )
#'
#' @references
#' Vargha, A., & Delaney, H. D. (2000). A critique and improvement of the CL
#' common language effect size statistics of McGraw and Wong. Journal of
#' Educational and Behavioral Statistics, 25(2), 101-132.
#'
#' @importFrom dplyr case_when
#' @export
calculate_pairwise_vda <- function(
  data,
  value_col,
  group_col,
  digits = 3,
  format = "default",
  include_n = TRUE
) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  if (!all(c(value_col, group_col) %in% names(data))) {
    stop("Specified columns not found in data frame")
  }

  if (!is.numeric(data[[value_col]])) {
    stop("value_col must contain numeric values")
  }

  # Check for missing values
  if (any(is.na(data[[value_col]]) | is.na(data[[group_col]]))) {
    warning("Data contains missing values. They will be removed.")
    data <- data[complete.cases(data[c(value_col, group_col)]), ]
  }

  # Get unique groups
  groups <- unique(data[[group_col]])
  n_groups <- length(groups)

  if (n_groups < 2) {
    stop("At least two groups are required for comparison")
  }

  # Create empty results dataframe
  results <- data.frame(
    group1 = character(),
    group2 = character(),
    n1 = integer(),
    n2 = integer(),
    VDA = numeric(),
    effect_size = character(),
    stringsAsFactors = FALSE
  )

  # Calculate VDA for each pair
  for (i in 1:(n_groups - 1)) {
    for (j in (i + 1):n_groups) {
      group1_data <- data[data[[group_col]] == groups[i], ][[value_col]]
      group2_data <- data[data[[group_col]] == groups[j], ][[value_col]]

      # Calculate VDA
      score <- 0
      for (x1 in group1_data) {
        for (x2 in group2_data) {
          if (x1 > x2) {
            score <- score + 1
          }
          if (x1 == x2) score <- score + 0.5
        }
      }

      vda <- score / (length(group1_data) * length(group2_data))

      # Determine effect size
      effect <- dplyr::case_when(
        vda >= 0.71 ~ "large positive",
        vda >= 0.64 ~ "medium positive",
        vda >= 0.56 ~ "small positive",
        vda <= 0.29 ~ "large negative",
        vda <= 0.36 ~ "medium negative",
        vda <= 0.44 ~ "small negative",
        TRUE ~ "negligible"
      )

      # Add to results
      results <- rbind(
        results,
        data.frame(
          group1 = groups[i],
          group2 = groups[j],
          n1 = length(group1_data),
          n2 = length(group2_data),
          VDA = round(vda, digits),
          effect_size = effect
        )
      )
    }
  }

  # Remove sample size columns if not requested
  if (!include_n) {
    results$n1 <- NULL
    results$n2 <- NULL
  }

  # Format output
  if (format == "kable") {
    if (!requireNamespace("knitr", quietly = TRUE)) {
      stop("Package 'knitr' needed for kable output")
    }
    return(knitr::kable(
      results,
      caption = "Vargha-Delaney A Effect Sizes",
      align = c('l', 'l', 'r', 'r', 'r', 'l'),
      booktabs = TRUE
    ))
  } else if (format == "gt") {
    if (!requireNamespace("gt", quietly = TRUE)) {
      stop("Package 'gt' needed for gt output")
    }
    return(
      gt::gt(results) %>%
        gt::tab_header(title = "Vargha-Delaney A Effect Sizes") %>%
        gt::fmt_number(columns = "VDA", decimals = digits) %>%
        gt::tab_style(
          style = gt::cell_fill(color = "#E0E0E0"),
          locations = gt::cells_body(
            columns = "effect_size",
            rows = results$effect_size != "negligible"
          )
        )
    )
  }

  return(results)
}