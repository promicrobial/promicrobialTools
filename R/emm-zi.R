#' Convert Odds Ratio to Cohen's d Effect Size
#'
#' @description
#' Converts odds ratios to Cohen's d effect sizes using the probit transformation
#' method. This is particularly useful for zero-inflated models and other analyses
#' where odds ratios are the primary effect size measure.
#'
#' @param OR Numeric. Odds ratio value(s) to convert
#' @param P0 Numeric. Baseline probability (proportion) in reference group (0-1)
#'
#' @return Returns numeric vector of Cohen's d values.
#'
#' @details
#' Effect size interpretations:
#' \itemize{
#'   \item |d| < 0.2: negligible
#'   \item 0.2 ≤ |d| < 0.5: small
#'   \item 0.5 ≤ |d| < 0.8: medium
#'   \item |d| ≥ 0.8: large
#' }
#'
#' The conversion uses the following steps:
#' 1. Calculate P1 from OR and P0
#' 2. Convert probabilities to z-scores using probit transformation
#' 3. Calculate d as difference between z-scores
#'
#' @examples
#' # Single OR conversion
#' or_to_cohens_d(OR = 2.5, P0 = 0.3)
#'
#' # Multiple ORs
#' or_to_cohens_d(OR = c(1.5, 2.0, 2.5), P0 = 0.3)
#'
#'
#' @section Usage in Meta-Analysis:
#' Particularly useful for meta-analyses combining studies reporting different 
#' effect size measures:
#' ```r
#' # Convert odds ratios from multiple studies
#' study_ORs <- c(1.5, 2.0, 2.5)
#' study_P0s <- c(0.3, 0.25, 0.35)
#' d_values <- mapply(or_to_cohens_d, study_ORs, study_P0s)
#' ```
#'
#' @references
#' Borenstein, M., Hedges, L. V., Higgins, J. P., & Rothstein, H. R. (2009).
#' Converting Among Effect Sizes. In Introduction to Meta-Analysis (pp. 45-49).
#' John Wiley & Sons, Ltd.
#'
#' @importFrom stats qnorm
#' @export
or_to_cohens_d <- function(OR, P0) {
    # Input validation
    if (!is.numeric(OR) || any(OR <= 0)) {
        stop("OR must be positive numeric value(s)")
    }
    
    if (!is.numeric(P0) || any(P0 <= 0) || any(P0 >= 1)) {
        stop("P0 must be between 0 and 1")
    }
    
    if (length(P0) != 1 && length(P0) != length(OR)) {
        stop("P0 must be of length 1 or same length as OR")
    }
    
    # Calculate Cohen's d
    calculate_d <- function(or, p0) {
        tryCatch({
            # Calculate P1 from OR and P0
            p1 <- (or * p0)/(1 - p0 + (or * p0))
            
            # Convert to z-scores and calculate d
            d <- qnorm(p1) - qnorm(p0)
            
            return(d)
        }, error = function(e) {
            warning("Error in d calculation: ", e$message)
            return(NA)
        })
    }
    
    # Calculate point estimates
    d_values <- mapply(calculate_d, OR, P0)
    
    # Return vector of d values
    return(d_values)
}

#' Calculate Effect Sizes for Zero-Inflated Model Contrasts
#'
#' @description
#' Converts zero-inflated model contrasts to interpretable effect sizes including
#' odds ratios and Cohen's d. Designed for analyzing group differences in 
#' zero-inflation probabilities from zero-inflated models.
#'
#' @param emm_zi An emmeans object from zero-inflated model component
#' @param contrasts_zi A pairs object from emmeans contrasts
#' @param model The original zero-inflated model object
#' @param effect_sizes Character vector. Effect sizes to calculate: "or" (odds ratio), 
#'        "d" (Cohen's d), or "both" (default: "both")
#' @param ci_level Numeric. Confidence interval level (default: 0.95)
#' @param format Character. Output format: "default", "kable", or "gt" (default: "default")
#'
#' @return A data frame (or formatted table) containing:
#' \itemize{
#'   \item contrast: Group comparison
#'   \item estimate: Log odds ratio estimate
#'   \item SE: Standard error
#'   \item df: Degrees of freedom
#'   \item OR: Odds ratio
#'   \item OR_lower: Lower CI bound for OR
#'   \item OR_upper: Upper CI bound for OR
#'   \item cohens_d: Cohen's d effect size
#'   \item magnitude: Effect size interpretation
#'   \item p.value: P-value for contrast
#' }
#'
#' @details
#' Effect size interpretations:
#' \itemize{
#'   \item |d| < 0.2: negligible
#'   \item 0.2 ≤ |d| < 0.5: small
#'   \item 0.5 ≤ |d| < 0.8: medium
#'   \item |d| ≥ 0.8: large
#' }
#'
#' @examples
#' \dontrun{
#' # Fit zero-inflated model
#' library(glmmTMB)
#' zi_model <- glmmTMB(count ~ mined + (1|site), zi=~mined, family=poisson, data=Salamanders)
#'
#' # Get emmeans and contrasts
#' library(emmeans)
#' emm_zi <- emmeans(zi_model, specs = "mined", component = "zero")
#' contrasts_zi <- pairs(emm_zi)
#'
#' # Get summary with effect sizes
#' results <- summarise_zi_contrasts(emm_zi, contrasts_zi, zi_model)
#'
#' # Format for publication
#' results_table <- summarise_zi_contrasts(emm_zi, contrasts_zi, zi_model,
#'                                        format = "kable")
#' }
#'
#' @section Usage in Quarto/RMarkdown:
#' ```{r}
#' #| label: zi-effects
#' #| tbl-cap: "Zero-Inflation Component Effect Sizes"
#'
#' summarise_zi_contrasts(emm_zi, contrasts_zi, model, 
#'                       format = "kable")
#' ```
#'
#' @importFrom dplyr mutate case_when
#' @importFrom stats confint qnorm
#' @importFrom emmeans emmeans pairs
#' @export
summarise_zi_contrasts <- function(emm_zi, 
                                  contrasts_zi, 
                                  model,
                                  effect_sizes = "both",
                                  ci_level = 0.95,
                                  format = "default") {
    
    # Input validation
    if (!inherits(model, c("zeroinfl", "hurdle", "zerotrunc", "glmmTMB"))) {
        stop("Model must be a zero-inflated model object")
    }
    
    if (!inherits(emm_zi, "emmGrid")) {
        stop("emm_zi must be an emmeans object")
    }
    
    if (!ci_level > 0 && !ci_level < 1) {
        stop("ci_level must be between 0 and 1")
    }
        
    # Get baseline probability
    y <- as.numeric(model$frame[,model$modelInfo$respCol])
    P0 <- sporp(y, silent = TRUE)$sparsity
    
    # Calculate contrasts and CIs
    contrast_summary <- tryCatch({
        conf <- confint(contrasts_zi, level = ci_level, component = "zi") %>%
            as.data.frame()
        
        conf %>%
            mutate(
                OR = exp(estimate),
                OR_lower = exp(asymp.LCL),
                OR_upper = exp(asymp.UCL),
                P0 = P0,
                cohens_d = mapply(or_to_cohens_d, .data$OR, .data$P0),
                magnitude = case_when(
                    abs(cohens_d) < 0.2 ~ "negligible",
                    abs(cohens_d) < 0.5 ~ "small",
                    abs(cohens_d) < 0.8 ~ "medium",
                    TRUE ~ "large"
                )
            )
    }, error = function(e) {
        stop("Error calculating contrasts: ", e$message)
    })
    
    # Format output
    if (format == "kable") {
        if (!requireNamespace("knitr", quietly = TRUE)) {
            stop("Package 'knitr' needed for kable output")
        }
        
        return(knitr::kable(contrast_summary,
                           digits = 3,
                           caption = "Zero-Inflation Component Effect Sizes",
                           align = c('l', rep('r', ncol(contrast_summary)-1)),
                           booktabs = TRUE))
        
    } else if (format == "gt") {
        if (!requireNamespace("gt", quietly = TRUE)) {
            stop("Package 'gt' needed for gt output")
        }
        
        return(gt::gt(contrast_summary)
               %>% gt::tab_header(title = "Zero-Inflation Component Effect Sizes")
               %>% gt::fmt_number(columns = where(is.numeric), decimals = 3)
               %>% gt::tab_style(
                   style = gt::cell_fill(color = "#E0E0E0"),
                   locations = gt::cells_body(
                       columns = "magnitude",
                       rows = magnitude != "negligible"
                   )
               ))
    }
    
    # Add class for potential method dispatch
    class(contrast_summary) <- c("zi_contrasts", class(contrast_summary))
    
    return(contrast_summary)
}
#' @rdname summarise_zi_contrasts
#' @export
summarize_zi_contrasts <- summarise_zi_contrasts