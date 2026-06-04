#' Calculate Effect Sizes for Mann-Whitney U Test
#'
#' @description
#' Calculates multiple effect size measures for Mann-Whitney U test (Wilcoxon rank-sum test)
#' including rank-biserial correlation, probability of superiority, and Common Language
#' Effect Size (CLES). Works with output from wilcox.test(), coin::wilcox_test(),
#' and rstatix::wilcox_test(). Output is optimized for Quarto rendering.
#'
#' @param test_result Result object from wilcox.test(), coin::wilcox_test(), or rstatix::wilcox_test()
#' @param x Numeric vector for group 1 (optional, for enhanced calculations)
#' @param y Numeric vector for group 2 (optional, for enhanced calculations)
#' @param n1 Sample size of group 1 (required if x not provided)
#' @param n2 Sample size of group 2 (required if y not provided)
#' @param alternative Direction of alternative hypothesis ("two.sided", "greater", "less")
#' @param verbose Logical, whether to print formatted results (default = TRUE)
#'
#' @return A list containing effect size measures, interpretations, and formatted markdown output
#'
#' @examples
#' # For Quarto documents
#' effect_results <- mannEff(test_result, x = group1, y = group2)
#'
#' # Access individual components
#' effect_results$rank_biserial_r
#' effect_results$tables$effect_sizes
#'
#' @export
mannEff <- function(
    test_result,
    x = NULL,
    y = NULL,
    n1 = NULL,
    n2 = NULL,
    alternative = "two.sided",
    alpha = 0.05,
    verbose = TRUE
) {
    # Load required packages
    if (!requireNamespace("knitr", quietly = TRUE)) {
        message("Package 'knitr' required for table formatting")
    }

    # Determine test type and extract information
    test_classes <- class(test_result)

    # Initialize variables
    W_statistic <- NULL
    U_statistic <- NULL
    p_value <- NULL
    method <- NULL
    z_statistic <- NULL

    if ("htest" %in% test_classes) {
        # Standard wilcox.test() output
        W_statistic <- as.numeric(test_result$statistic)
        p_value <- test_result$p.value
        method <- test_result$method
    } else if ("IndependenceTest" %in% test_classes) {
        # coin package wilcox_test() output
        W_statistic <- as.numeric(coin::statistic(test_result))
        p_value <- coin::pvalue(test_result)
        method <- "Wilcoxon rank sum test (coin package)"
        z_statistic <- W_statistic # In coin, this is already the z-statistic
    } else if (any(c("rstatix_test", "wilcox_test") %in% test_classes)) {
        # rstatix package wilcox_test() output
        if ("statistic" %in% names(test_result)) {
            W_statistic <- as.numeric(test_result$statistic)
        } else if ("W" %in% names(test_result)) {
            W_statistic <- as.numeric(test_result$W)
        } else {
            stop("Could not find test statistic in rstatix output")
        }

        p_value <- test_result$p
        method <- "Wilcoxon rank sum test (rstatix package)"

        # Extract additional information if available
        if ("n1" %in% names(test_result) && is.null(n1)) {
            n1 <- test_result$n1
        }
        if ("n2" %in% names(test_result) && is.null(n2)) {
            n2 <- test_result$n2
        }
    } else {
        stop(
            "Unsupported test result type. Use wilcox.test(), coin::wilcox_test(), or rstatix::wilcox_test()"
        )
    }

    # Determine sample sizes
    if (!is.null(x) && !is.null(y)) {
        n1 <- length(x[!is.na(x)])
        n2 <- length(y[!is.na(y)])
        has_raw_data <- TRUE
    } else if (!is.null(n1) && !is.null(n2)) {
        has_raw_data <- FALSE
    } else {
        # Try to extract from test result
        if (any(c("rstatix_test", "wilcox_test") %in% test_classes)) {
            if ("n1" %in% names(test_result) && "n2" %in% names(test_result)) {
                n1 <- test_result$n1
                n2 <- test_result$n2
                has_raw_data <- FALSE
            } else {
                stop(
                    "Sample sizes not available. Provide either raw data (x, y) or sample sizes (n1, n2)"
                )
            }
        } else {
            stop(
                "Sample sizes not available. Provide either raw data (x, y) or sample sizes (n1, n2)"
            )
        }
    }

    # Total sample size
    N <- n1 + n2

    # Calculate U statistic from W
    if ("htest" %in% test_classes) {
        U1 <- W_statistic
        U2 <- n1 * n2 - U1
        U_statistic <- min(U1, U2)
    } else if ("IndependenceTest" %in% test_classes) {
        if (has_raw_data) {
            combined_data <- c(x, y)
            ranks <- rank(combined_data)
            sum_ranks_x <- sum(ranks[1:n1])
            U1 <- sum_ranks_x - n1 * (n1 + 1) / 2
            U2 <- n1 * n2 - U1
            U_statistic <- min(U1, U2)
        } else {
            z_stat <- W_statistic
            expected_U <- n1 * n2 / 2
            se_U <- sqrt(n1 * n2 * (N + 1) / 12)
            U_calc <- expected_U - z_stat * se_U
            U_statistic <- max(0, min(U_calc, n1 * n2 - U_calc))
        }
    } else if (any(c("rstatix_test", "wilcox_test") %in% test_classes)) {
        U1 <- W_statistic
        U2 <- n1 * n2 - U1
        U_statistic <- min(U1, U2)

        if (W_statistic > n1 * n2 / 2) {
            if (has_raw_data) {
                combined_data <- c(x, y)
                ranks <- rank(combined_data)
                sum_ranks_x <- sum(ranks[1:n1])
                U1 <- sum_ranks_x - n1 * (n1 + 1) / 2
                U2 <- n1 * n2 - U1
                U_statistic <- min(U1, U2)
            } else {
                U_statistic <- min(W_statistic, n1 * n2 - W_statistic)
            }
        }
    }

    U_statistic <- max(0, min(U_statistic, n1 * n2))

    # Calculate effect sizes
    rank_biserial_r <- 1 - (2 * U_statistic) / (n1 * n2)
    probability_superiority <- U_statistic / (n1 * n2)
    cles <- probability_superiority

    # Z-based effect size
    z_based_r <- NULL
    if ("IndependenceTest" %in% test_classes) {
        z_based_r <- abs(z_statistic) / sqrt(N)
    } else {
        expected_U <- n1 * n2 / 2
        var_U <- n1 * n2 * (N + 1) / 12

        if (has_raw_data) {
            combined_data <- c(x, y)
            tie_correction <- calculate_tie_correction(combined_data)
            var_U <- var_U * tie_correction
        }

        z_stat <- (U_statistic - expected_U) / sqrt(var_U)
        z_based_r <- abs(z_stat) / sqrt(N)
        z_statistic <- z_stat
    }

    # Approximate Cohen's d
    cohen_d_approx <- NULL
    if (!is.null(z_based_r) && abs(rank_biserial_r) < 0.95) {
        cohen_d_approx <- 2 * abs(rank_biserial_r) / sqrt(1 - rank_biserial_r^2)
    }

    # Effect size interpretations
    interpretation <- interpret_effect_sizes(
        rank_biserial_r,
        probability_superiority,
        z_based_r
    )

    # Create formatted tables
    tables <- create_tables(
        rank_biserial_r,
        probability_superiority,
        cles,
        z_based_r,
        cohen_d_approx,
        interpretation,
        U_statistic,
        W_statistic,
        z_statistic,
        p_value,
        n1,
        n2,
        method
    )

    # Compile results
    results <- list(
        tables = lapply(tables, kable, row.names = FALSE)
    )

    class(results) <- "mannEff"
    return(results)
}

#' Calculate Tie Correction Factor for Mann-Whitney U Test
#'
#' @description
#' Calculates the tie correction factor for the Mann-Whitney U test variance
#' calculation when tied ranks are present in the data. The correction adjusts
#' the standard error of the U statistic to account for tied observations.
#'
#' @param data Numeric vector containing the combined data from both groups
#'
#' @return Numeric value representing the tie correction factor. Returns 1 if
#'   no ties are present, or a value between 0 and 1 if ties exist.
#'
#' @details
#' The tie correction factor is calculated as:
#' \deqn{correction = 1 - \frac{\sum(t^3 - t)}{n^3 - n}}
#'
#' where \eqn{t} represents the frequency of each tied value and \eqn{n} is the
#' total sample size. This correction is applied to the variance of the U statistic
#' to provide more accurate p-values when ties are present.
#'
#' @examples
#' # Data without ties
#' data_no_ties <- c(1, 2, 3, 4, 5, 6)
#' calculate_tie_correction(data_no_ties)  # Returns 1
#'
#' # Data with ties
#' data_with_ties <- c(1, 2, 2, 3, 3, 3, 4)
#' calculate_tie_correction(data_with_ties)  # Returns < 1
#'
#' @seealso \code{\link{mannEff}} for the main effect size calculation function
#'
#' @keywords internal
calculate_tie_correction <- function(data) {
  tab <- table(data)
  ties <- tab[tab > 1]
  if (length(ties) == 0) {
    return(1)
  }

  n <- length(data)
  tie_term <- sum(ties^3 - ties)
  correction <- 1 - tie_term / (n^3 - n)
  return(correction)
}

#' Interpret Effect Sizes for Mann-Whitney U Test
#'
#' @description
#' Provides verbal interpretations for various effect size measures from the
#' Mann-Whitney U test based on established guidelines from Cohen (1988) and
#' other effect size literature.
#'
#' @param r Numeric. Rank-biserial correlation coefficient (range: -1 to 1)
#' @param ps Numeric. Probability of superiority (range: 0 to 1)
#' @param z_r Numeric or NULL. Z-statistic based effect size (optional)
#'
#' @return A list containing character strings with interpretations:
#'   \describe{
#'     \item{rank_biserial}{Interpretation for rank-biserial correlation}
#'     \item{probability_superiority}{Interpretation for probability of superiority}
#'     \item{z_based}{Interpretation for z-based effect size (NULL if z_r not provided)}
#'   }
#'
#' @details
#' Interpretation guidelines used:
#' \itemize{
#'   \item \strong{Rank-biserial |r|}: < 0.10 (Negligible), 0.10-0.30 (Small),
#'     0.30-0.50 (Medium), ≥ 0.50 (Large)
#'   \item \strong{Probability of Superiority}: Based on deviation from 0.5:
#'     < 0.06 (Negligible), 0.06-0.14 (Small), 0.14-0.21 (Medium), ≥ 0.21 (Large)
#'   \item \strong{Z-based effect size}: Same thresholds as rank-biserial correlation
#' }
#'
#' @examples
#' # Small effect
#' interpret_effect_sizes(r = 0.2, ps = 0.6)
#'
#' # Large effect with z-statistic
#' interpret_effect_sizes(r = -0.6, ps = 0.3, z_r = 0.55)
#'
#' # Negligible effect
#' interpret_effect_sizes(r = 0.05, ps = 0.51)
#'
#' @references
#' Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd ed.).
#' Lawrence Erlbaum Associates.
#'
#' @seealso \code{\link{mannEff}} for the main effect size calculation function
#'
#' @keywords internal
interpret_effect_sizes <- function(r, ps, z_r = NULL) {
  # Rank-biserial correlation interpretation
  r_abs <- abs(r)
  if (r_abs < 0.10) {
    r_interp <- "Negligible"
  } else if (r_abs < 0.30) {
    r_interp <- "Small"
  } else if (r_abs < 0.50) {
    r_interp <- "Medium"
  } else {
    r_interp <- "Large"
  }

  # Probability of superiority interpretation
  ps_centered <- abs(ps - 0.5)
  if (ps_centered < 0.06) {
    ps_interp <- "Negligible"
  } else if (ps_centered < 0.14) {
    ps_interp <- "Small"
  } else if (ps_centered < 0.21) {
    ps_interp <- "Medium"
  } else {
    ps_interp <- "Large"
  }

  # Z-based interpretation (if available)
  z_interp <- NULL
  if (!is.null(z_r)) {
    if (z_r < 0.10) {
      z_interp <- "Negligible"
    } else if (z_r < 0.30) {
      z_interp <- "Small"
    } else if (z_r < 0.50) {
      z_interp <- "Medium"
    } else {
      z_interp <- "Large"
    }
  }

  return(list(
    rank_biserial = r_interp,
    probability_superiority = ps_interp,
    z_based = z_interp
  ))
}

#' Create Formatted Tables for Mann-Whitney U Effect Sizes
#'
#' @description
#' Creates formatted data frames containing effect size measures and test
#' information for Mann-Whitney U test results, suitable for display in
#' reports and publications.
#'
#' @param rank_biserial_r Numeric. Rank-biserial correlation coefficient
#' @param probability_superiority Numeric. Probability of superiority
#' @param cles Numeric. Common Language Effect Size
#' @param z_based_r Numeric or NULL. Z-statistic based effect size (optional)
#' @param cohen_d_approx Numeric or NULL. Approximate Cohen's d (optional)
#' @param interpretation List. Output from \code{\link{interpret_effect_sizes}}
#' @param U_statistic Numeric. Mann-Whitney U statistic
#' @param W_statistic Numeric. Wilcoxon W statistic
#' @param z_statistic Numeric or NULL. Z-statistic from test (optional)
#' @param p_value Numeric. p-value from the test
#' @param n1 Integer. Sample size of group 1
#' @param n2 Integer. Sample size of group 2
#' @param method Character. Description of the test method used
#'
#' @return A list containing two data frames:
#'   \describe{
#'     \item{effect_sizes}{Data frame with effect size measures, values, and interpretations}
#'     \item{test_info}{Data frame with test statistics and sample information}
#'   }
#'
#' @details
#' The function creates two formatted tables:
#' \itemize{
#'   \item \strong{Effect Sizes Table}: Contains all calculated effect size measures
#'     with their values formatted to 3 decimal places and verbal interpretations
#'   \item \strong{Test Information Table}: Contains test statistics, sample sizes,
#'     and other relevant test information
#' }
#'
#' Optional measures (z-based effect size, Cohen's d) are included only if provided.
#' All numeric values are formatted consistently for presentation.
#'
#' @examples
#' \dontrun{
#' # Example usage within mannEff function
#' interp <- interpret_effect_sizes(0.3, 0.65, 0.25)
#' tables <- create_tables(
#'   rank_biserial_r = 0.3,
#'   probability_superiority = 0.65,
#'   cles = 0.65,
#'   z_based_r = 0.25,
#'   cohen_d_approx = 0.62,
#'   interpretation = interp,
#'   U_statistic = 150,
#'   W_statistic = 300,
#'   z_statistic = -2.5,
#'   p_value = 0.012,
#'   n1 = 20,
#'   n2 = 25,
#'   method = "Wilcoxon rank sum test"
#' )
#' }
#'
#' @seealso
#' \code{\link{mannEff}} for the main effect size calculation function
#' \code{\link{interpret_effect_sizes}} for effect size interpretations
#'
#' @keywords internal
create_tables <- function(
  rank_biserial_r,
  probability_superiority,
  cles,
  z_based_r,
  cohen_d_approx,
  interpretation,
  U_statistic,
  W_statistic,
  z_statistic,
  p_value,
  n1,
  n2,
  method
) {
  # Effect sizes table
  effect_measures <- c(
    "Rank-biserial correlation (r)",
    "Probability of Superiority (PS)",
    "Common Language Effect Size"
  )
  effect_values <- c(
    sprintf("%.3f", rank_biserial_r),
    sprintf("%.3f", probability_superiority),
    sprintf("%.3f", cles)
  )
  effect_interpretations <- c(
    interpretation$rank_biserial,
    interpretation$probability_superiority,
    interpretation$probability_superiority
  )

  # Add optional measures
  if (!is.null(z_based_r)) {
    effect_measures <- c(effect_measures, "Z-based effect size (r)")
    effect_values <- c(effect_values, sprintf("%.3f", z_based_r))
    effect_interpretations <- c(
      effect_interpretations,
      interpretation$z_based
    )
  }

  if (!is.null(cohen_d_approx)) {
    effect_measures <- c(effect_measures, "Approximate Cohen's d")
    effect_values <- c(effect_values, sprintf("%.3f", cohen_d_approx))
    effect_interpretations <- c(
      effect_interpretations,
      classify_cohen_d(cohen_d_approx)
    )
  }

  effect_table <- data.frame(
    `Effect Size Measure` = effect_measures,
    Value = effect_values,
    Interpretation = effect_interpretations,
    stringsAsFactors = FALSE
  )

  # Test information table
  test_info_table <- data.frame(
    Statistic = c(
      "Method",
      "Sample sizes",
      "U statistic",
      "W statistic",
      "p-value"
    ),
    Value = c(
      method,
      sprintf("n₁ = %d, n₂ = %d", n1, n2),
      sprintf("%.3f", U_statistic),
      sprintf("%.3f", W_statistic),
      format_pvalue(p_value)
    ),
    stringsAsFactors = FALSE
  )

  # Add Z statistic if available
  if (!is.null(z_statistic)) {
    test_info_table <- rbind(
      test_info_table[1:4, ],
      data.frame(
        Statistic = "Z statistic",
        Value = sprintf("%.3f", z_statistic),
        stringsAsFactors = FALSE
      ),
      test_info_table[5, ]
    )
  }

  return(list(
    effect_sizes = effect_table,
    test_info = test_info_table
  ))
}

#' Classify Cohen's d Effect Size
#'
#' @description
#' Provides verbal interpretation of Cohen's d effect size measure based on
#' Cohen's (1988) conventional guidelines.
#'
#' @param d Numeric. Cohen's d value (can be positive or negative)
#'
#' @return Character string indicating the magnitude of the effect:
#'   "Negligible", "Small", "Medium", or "Large"
#'
#' @details
#' Classification follows Cohen's (1988) conventions:
#' \itemize{
#'   \item |d| < 0.2: Negligible effect
#'   \item 0.2 ≤ |d| < 0.5: Small effect
#'   \item 0.5 ≤ |d| < 0.8: Medium effect
#'   \item |d| ≥ 0.8: Large effect
#' }
#'
#' The sign of Cohen's d indicates direction but does not affect the magnitude
#' classification.
#'
#' @examples
#' classify_cohen_d(0.1)   # "Negligible"
#' classify_cohen_d(0.3)   # "Small"
#' classify_cohen_d(-0.6)  # "Medium"
#' classify_cohen_d(1.2)   # "Large"
#'
#' @references
#' Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd ed.).
#' Lawrence Erlbaum Associates.
#'
#' @seealso \code{\link{mannEff}} for the main effect size calculation function
#'
#' @keywords internal
classify_cohen_d <- function(d) {
  d_abs <- abs(d)
  if (d_abs < 0.2) {
    return("Negligible")
  }
  if (d_abs < 0.5) {
    return("Small")
  }
  if (d_abs < 0.8) {
    return("Medium")
  }
  return("Large")
}

#' Format p-values for Display
#'
#' @description
#' Formats p-values according to common statistical reporting conventions,
#' handling very small p-values appropriately for publication.
#'
#' @param p Numeric. p-value to format (should be between 0 and 1)
#' @param digits Integer. Number of decimal places for rounding (default: 3)
#'
#' @return Character string with formatted p-value
#'
#' @details
#' Formatting rules:
#' \itemize{
#'   \item p < 0.001: Returns "< 0.001"
#'   \item 0.001 ≤ p < 0.01: Returns value with 3 decimal places
#'   \item p ≥ 0.01: Returns value with specified number of digits
#' }
#'
#' @examples
#' format_pvalue(0.0001)    # "< 0.001"
#' format_pvalue(0.005)     # "0.005"
#' format_pvalue(0.043)     # "0.043"
#' format_pvalue(0.1234)    # "0.123"
#' format_pvalue(0.1234, digits = 4)  # "0.1234"
#'
#' @seealso \code{\link{mannEff}} for the main effect size calculation function
#'
#' @keywords internal
format_pvalue <- function(p, digits = 3) {
  if (p < 0.001) {
    return("< 0.001")
  }
  if (p < 0.01) {
    return(sprintf("%.3f", p))
  }
  return(sprintf("%.3f", p))
}

#' Summary Method for mannEff Objects
#'
#' @description
#' Provides a concise summary of Mann-Whitney U test effect sizes in markdown
#' format, suitable for inclusion in reports and documentation.
#'
#' @param object An object of class "mannEff" created by \code{\link{mannEff}}
#' @param ... Additional arguments (currently ignored)
#'
#' @return NULL (invisible). Prints formatted summary to console.
#'
#' @details
#' The summary includes:
#' \itemize{
#'   \item Rank-biserial correlation with interpretation
#'   \item Probability of superiority with interpretation
#' }
#'
#' Output is formatted in markdown style with bold headers for easy integration
#' into reports.
#'
#' @examples
#' \dontrun{
#' # Assuming you have a mannEff result
#' test_result <- wilcox.test(group1, group2)
#' effects <- mannEff(test_result, x = group1, y = group2, verbose = FALSE)
#' summary(effects)
#' }
#'
#' @seealso
#' \code{\link{mannEff}} for the main effect size calculation function
#' \code{\link{print.mannEff}} for full formatted output
#'
#' @method summary mannEff
#' @export
summary.mannEff <- function(object, ...) {
  cat("**Mann-Whitney U Effect Size Summary**\n\n")
  cat(
    "- **Rank-biserial r:** ",
    round(object$rank_biserial_r, 3),
    " (",
    object$interpretation$rank_biserial,
    ")\n"
  )
  cat(
    "- **Probability of Superiority:** ",
    round(object$probability_superiority, 3),
    " (",
    object$interpretation$probability_superiority,
    ")\n"
  )
}