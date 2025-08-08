#' Create Diagnostic Plots for Model Residuals
#'
#' @description
#' Creates a comprehensive set of diagnostic plots for model residuals, including:
#' residuals vs. fitted values, Q-Q plot, histogram with normal curve, and 
#' Shapiro-Wilk test results. Optionally performs Cholesky decomposition for
#' mixed-effects models.
#'
#' @param model A fitted model object (lm, glm, or lme)
#' @param model_name Character. Name to display in plot title (default: names(model))
#' @param render Logical. Whether to render plots in current device (TRUE) or new window (FALSE)
#' @param chol Logical. Whether to perform Cholesky decomposition (default: TRUE)
#' @param theme Character. Plot theme: "light", "dark", or "classic" (default: "light")
#' @param colors List. Custom colors for plot elements (optional)
#'
#' @return A grid arrangement of four diagnostic plots:
#' \itemize{
#'   \item Residuals vs. Fitted Values plot
#'   \item Normal Q-Q plot
#'   \item Histogram with normal curve
#'   \item Shapiro-Wilk test results
#' }
#'
#' @details
#' The function performs several diagnostic checks:
#' \itemize{
#'   \item Linearity assumption via residuals vs. fitted plot
#'   \item Normality via Q-Q plot and Shapiro-Wilk test
#'   \item Distribution shape via histogram
#'   \item Homoscedasticity via spread in residuals plot
#' }
#'
#' @section Cholesky Decomposition:
#' When chol = TRUE, performs Cholesky decomposition on the residuals'
#' covariance matrix, which is particularly useful for mixed-effects models.
#'
#' @examples
#' \dontrun{
#' # Linear model example
#' model <- lm(mpg ~ wt + hp, data = mtcars)
#' residual_plots(model)
#'
#' # Mixed effects model
#' if(require(nlme)) {
#'   model_mixed <- lme(distance ~ age, data = Orthodont, random = ~1|Subject)
#'   residual_plots(model_mixed, chol = TRUE)
#' }
#' }
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth geom_hline stat_qq 
#'             stat_qq_line geom_histogram stat_function theme labs
#' @importFrom gridExtra grid.arrange
#' @importFrom stats residuals predict shapiro.test sd
#' @export
residual_plots <- function(model, 
  model_name = NULL,
  render = TRUE,
  chol = TRUE) {
    # Input validation
    if (!inherits(model, c("lm", "glm", "lme", "merMod"))) {
      stop("Model must be of class 'lm', 'glm', 'lme', or 'merMod'")
    }
    
    if (is.null(model_name)) {
      model_name <- deparse(substitute(model))
    } 
    
    if (chol && inherits(model, "lm")) {
      warning("Cholesky decomposition cannot be performed on residuals from model of class `lm`. Using standardised Pearson residuals instead.")
      chol = FALSE
    } 

    # Calculate residuals
    if (chol && inherits(model, "lme")) {
      tryCatch({
        rawRes <- residuals(model, type = "pearson")
        estCov <- extract.lme.cov(model, model$data)
        
        message("Performing Cholesky decomposition on residuals from ", model_name)
        
        residuals <- solve(t(chol(estCov))) %*% rawRes
        
      }, error = function(e) {
        warning("Cholesky decomposition failed. Using raw residuals instead.")
        residuals <- residuals(model, type = "pearson")
      })
    } else {
      residuals <- residuals(model, type = "pearson")
    }
    
    # Create plots
    xy_plot <- create_residual_fitted_plot(model, residuals)
    
    qq_plot <- create_qq_plot(residuals)
    
    hist_plot <- create_hist_plot(residuals)
    
    norm_test_plot <- create_normality_test_plot(residuals)
    
    # Arrange plots
    if (!render) {
      if (capabilities("X11")) {
        X11(title = model_name)
      } else {
        warning("X11 device not available. Using current device.")
      }
    }
    
    plots <- gridExtra::grid.arrange(
      xy_plot, qq_plot, hist_plot, norm_test_plot,
      ncol = 2,
      nrow = 2,
      top = grid::textGrob(paste("Diagnostic Plots for", model_name),
      gp = grid::gpar(fontsize = 12, font = 2))
    )
    
    # Add class for potential method dispatch
    class(plots) <- c("residual_diagnostic", class(plots))
    
    return(invisible(plots))
}
  
  # Helper functions (to be defined in same file)
  create_residual_fitted_plot <- function(model, residuals, theme) {
    ggplot(data.frame(x = predict(model), y = residuals),
    aes(x = x, y = y)) +
    geom_point(shape = 1, size = 2, color = colors$point) +
    geom_smooth(method = "loess", color = colors$line) +
    geom_hline(yintercept = 0, color = colors$line) +
    labs(x = "Fitted Values",
    y = "Model Residuals",
    title = "Residuals vs Fitted Values") +
    theme
  }
  
  residual_norm_test <- function(model_list) {
    norm_test <- lapply(names(model_list), function(model_name) {
      model <- model_list[[model_name]]
      st <- shapiro.test(residuals(model))
      data.frame(Model = model_name, W = st$statistic, P_value = st$p.value)
    })
    norm_df <- do.call(rbind, norm_test)
    return(norm_df)
  }