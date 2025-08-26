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

#' Create Standardized EMMs with Flexible Specifications
#'
#' This function creates emmeans objects with flexible specifications for different
#' study designs. It handles custom transformations, level labeling, and various
#' emmeans parameters.
#'
#' @param model A fitted model object (e.g., from glmmTMB, lme4, etc.)
#' @param specs Formula or character string specifying the emmeans structure
#' @param at_values Named list of values at which to evaluate emmeans (default: NULL)
#' @param adjust_method Character string specifying p-value adjustment method (default: "sidak" (the same as the in the emmeans package))
#' @param weights Character string specifying weighting method (default: "proportional")
#' @param type Character string specifying the scale for emmeans (default: "response")
#' @param component Character string specifying model component for zero-inflated models (default: "cond")
#' @param transform Character string specifying transformation to apply (default: NULL)
#' @param level_labels Named list of custom labels for factor levels (default: NULL)
#'
#' @return An emmGrid object
#'
#' @details
#' The function creates emmeans objects with consistent parameter handling across
#' different study designs. Custom level labels can be applied to make results
#' more interpretable.
#'
#' @examples
#' warp.lm <- lm(breaks ~ wool * tension, data = warpbreaks)
#' 
#' # Simple comparison
#' create_emmeans(
#'   model = warp.lm,
#'   specs = ~ wool | tension,
#'   by = "tension",
#'   level_labels = list(tension = c("Low", "Medium", "High"))
#' )
#' \dontrun{
#'
#' # Complex interaction with zero-inflated component
#' emm_zi <- create_emmeans(
#'   model = zi_model,
#'   specs = ~ treatment | time,
#'   component = "zi",
#'   transform = "logit"
#' )
#' }
#'
#' @seealso \code{\link[emmeans]{emmeans}}, \code{\link[emmeans]{regrid}}
#' @export
create_emmeans <- function(model, 
                          specs, 
                          by = NULL,
                          at_values = NULL,
                          adjust_method = "sidak",
                          weights = "proportional",
                          type = "response",
                          component = "cond",
                          transform = NULL,
                          level_labels = NULL) {
  
  # Create emmeans object
  emm <- emmeans(
    model,
    specs = specs,
    by = by,
    at = at_values,
    adjust = adjust_method,
    weights = weights,
    type = type,
    component = component
  )
  
  # Apply transformation if specified
  if (!is.null(transform)) {
    emm <- regrid(emm, transform = transform)
  }
  
  # Apply custom level labels if provided
  if (!is.null(level_labels)) {
    for (var_name in names(level_labels)) {
      if (var_name %in% names(emm@levels)) {
        levels(emm)[[var_name]] <- level_labels[[var_name]]
      }
    }
  }
  
  return(emm)
}

#' Create Flexible Contrasts for Different Study Designs
#'
#' This function creates various types of contrasts from emmeans objects,
#' supporting different contrast types commonly used in research.
#'
#' @param emm_object An emmGrid object from emmeans
#' @param contrast_type Character string specifying contrast type: "pairwise", 
#'   "sequential", "vs_control", "vs_reference", or "custom" (default: "pairwise")
#' @param by_vars Character vector of variables to group contrasts by (default: NULL)
#' @param method Character string specifying contrast method (default: "revpairwise")
#' @param reverse Logical indicating whether to reverse contrast direction (default: TRUE)
#' @param custom_contrasts Custom contrast matrix or list for custom contrasts (default: NULL)
#' @param adjust_method Character string specifying p-value adjustment method (default: "sidak")
#'
#' @return A contrast object (emmGrid)
#'
#' @details
#' The function supports multiple contrast types:
#' \itemize{
#'   \item \code{pairwise}: All pairwise comparisons
#'   \item \code{sequential}: Sequential/consecutive comparisons  
#'   \item \code{vs_control}: Compare all levels to control (first level)
#'   \item \code{vs_reference}: Compare all levels to reference (Dunnett's test)
#'   \item \code{custom}: Use custom contrast matrix
#' }
#'
#' @examples
#' warp.lm <- lm(breaks ~ wool * tension, data = warpbreaks)
#' 
#' # Simple comparison
#' emm <- create_emmeans(
#'   model = warp.lm,
#'   specs = ~ wool | tension,
#'   level_labels = list(tension = c("Low", "Medium", "High"))
#' )
#' \dontrun{
#' # Simple pairwise contrasts
#' contrasts <- create_contrasts(emm, contrast_type = "pairwise")
#'
#' # Treatment contrasts by time
#' contrasts <- create_contrasts(
#'   emm, 
#'   contrast_type = "pairwise",
#'   by_vars = "time"
#' )
#'
#' # Custom contrasts
#' custom_matrix <- list(
#'   "Treatment_vs_Control" = c(-1, 1, 0, 0),
#'   "Time_effect" = c(-1, 0, 1, 0)
#' )
#' contrasts <- create_contrasts(
#'   emm,
#'   contrast_type = "custom",
#'   custom_contrasts = custom_matrix
#' )
#' }
#'
#' @seealso \code{\link[emmeans]{contrast}}, \code{\link[emmeans]{pairs}}
#' @export
create_contrasts <- function(emm_object,
                           contrast_type = "pairwise",
                           by_vars = NULL,
                           reverse = TRUE,
                           custom_contrasts = NULL,
                           adjust_method = "sidak") {
  
  if (!is.null(custom_contrasts)) {
    # Use custom contrast matrix or list
    contrasts <- contrast(emm_object, custom_contrasts, adjust = adjust_method)
  } else if (!is.null(contrast_type)) {
    # Standard pairwise comparisons
    if (!is.null(by_vars)) {
      contrasts <- contrast(emm_object, method = contrast_type, by = by_vars, 
                          reverse = reverse, adjust = adjust_method)
    } else {
      contrasts <- contrast(emm_object, method = contrast_type, 
                          reverse = reverse, adjust = adjust_method)
    }
  } else {
    stop("Contrast method not supplied. Please set custom_contrast or contrast_type.")
  } 
  return(contrasts)
}

#' Create Trends (Slopes) with Flexible Specifications and perform pairwise comparisons thereof
#'
#' This function creates emtrends objects for analyzing slopes/trends in
#' continuous variables with flexible grouping and labeling options.
#'
#' @param model A fitted model object
#' @param trend_var Character string specifying the variable to compute trends for
#' @param specs Formula or character string specifying the trend structure (default: NULL)
#' @param at_values Named list of values at which to evaluate trends (default: NULL)
#' @param adjust_method Character string specifying p-value adjustment method (default: "sidak")
#' @param level_labels Named list of custom labels for factor levels (default: NULL)
#'
#' @return An emtrends object (emmGrid)
#'
#' @details
#' This function wraps emtrends to provide consistent parameter handling and
#' custom labeling capabilities. When specs is NULL, it defaults to computing
#' trends across all observations.
#'
#' @examples
#' \dontrun{
#' fiber.lm <- lm(strength ~ diameter*machine, data=fiber)
#' # Simple time trends
#' trends <- create_trends(
#'   fiber.lm, 
#'   trend_var = "diameter"
#'   specs = ~ machine
#' )
#'
#' # Treatment-specific time trends
#' trends <- create_trends(
#'   model,
#'   trend_var = "time",
#'   specs = ~ treatment,
#'   level_labels = list(treatment = c("Control", "Intervention"))
#' )
#'
#' # Age-specific trends at specific time points
#' trends <- create_trends(
#'   model,
#'   trend_var = "time",
#'   specs = ~ age_group,
#'   at_values = list(age_group = c("young", "old"))
#' )
#' }
#'
#' @seealso \code{\link[emmeans]{emtrends}}
#' @export
create_trends <- function(model,
                         trend_var,
                         specs = NULL,
                         at_values = NULL,
                         adjust_method = "sidak",
                         level_labels = NULL) {
  
  # Create emtrends object
    trends <- emtrends(model, specs = specs, var = trend_var, 
                      at = at_values, adjust = adjust_method)
 
  # Apply custom level labels if provided
  if (!is.null(level_labels)) {
    for (var_name in names(level_labels)) {
      if (var_name %in% names(trends@levels)) {
        levels(trends)[[var_name]] <- level_labels[[var_name]]
      }
    }
  }
  pairs <- pairs(trends)
  return(list(trends = trends, pairs = pairs))
}

#' Process EMM Results into Data Frames
#'
#' Internal function that processes raw emmeans results into data frames,
#' merges with taxonomic information, and identifies significant results.
#'
#' @param analysis_results List containing raw emmeans objects
#' @param taxa_data Data frame containing taxonomic information (default: NULL)
#' @param significance_threshold Numeric threshold for significance (default: 0.05)
#'
#' @return List containing processed results with 'all' and 'significant' data frames
#'
#' @details
#' This function converts emmGrid objects to data frames, adds taxonomic information
#' if provided, and separates significant from non-significant results.
#'
#' @keywords internal
process_emm_results <- function(analysis_results, taxa_data = NULL, significance_threshold = 0.05) {
  
  processed <- list()
  
  # Process contrasts
  if (!is.null(analysis_results$contrasts)) {
    contrast_df <- emm_bind(analysis_results$contrasts, kable = FALSE)
    
    if (!is.null(taxa_data)) {
      contrast_df <- contrast_df %>%
        left_join(rownames_to_column(taxa_data, "Model"), by = "Model") %>%
        mutate(Species = ifelse(Species == "", Genus, Species))
    }
    
    # Identify significant results
    significant_contrasts <- contrast_df %>%
      filter(p.value <= significance_threshold)
    
    processed$contrasts <- list(
      all = contrast_df,
      significant = significant_contrasts
    )
  }
  
  # Process trend contrasts
  if (!is.null(analysis_results$trend_contrasts)) {
    trend_contrast_df <- emm_bind(analysis_results$trend_contrasts, kable = FALSE)
    
    if (!is.null(taxa_data)) {
      trend_contrast_df <- trend_contrast_df %>%
        left_join(rownames_to_column(taxa_data, "Model"), by = "Model") %>%
        mutate(Species = ifelse(Species == "", Genus, Species))
    }
    
    # Identify significant results
    significant_trends <- trend_contrast_df %>%
      filter(p.value <= significance_threshold)
    
    processed$trend_contrasts <- list(
      all = trend_contrast_df,
      significant = significant_trends
    )
  }
  
  # Store raw EMM objects for further analysis
  processed$raw_emms <- analysis_results$emms
  processed$raw_trends <- analysis_results$trends
  
  return(processed)
}

#' Add Effect Sizes to EMM Results
#'
#' Internal function that calculates and adds effect sizes to processed emmeans results.
#'
#' @param processed_results List of processed results from process_emm_results
#' @param models Named list of fitted model objects
#' @param effect_config List containing effect size configuration parameters
#'
#' @return Updated processed_results list with effect sizes added
#'
#' @details
#' Effect size calculation methods supported:
#' \itemize{
#'   \item \code{random_effects}: Uses random effect variance for sigma
#'   \item \code{residual}: Uses residual standard error for sigma
#' }
#'
#' @keywords internal
add_effect_sizes <- function(processed_results, models, effect_config) {
  
  # Calculate effect sizes for contrasts
  if (!is.null(processed_results$contrasts)) {
    
    # Get contrast objects
    contrast_objects <- lapply(names(models), function(model_name) {
      if (!is.null(processed_results$raw_emms[[model_name]])) {
        pairs(processed_results$raw_emms[[model_name]], reverse = TRUE)
      }
    })
    names(contrast_objects) <- names(models)
    contrast_objects <- contrast_objects[!sapply(contrast_objects, is.null)]
    
    # Calculate effect sizes
    effect_sizes <- lapply(1:length(contrast_objects), function(i) {
      model_name <- names(contrast_objects)[i]
      model <- models[[model_name]]
      
      # Determine sigma based on effect_config
      if (effect_config$sigma_method == "random_effects") {
        sigma_val <- attr(VarCorr(model)$cond$subject, "stddev") * sqrt(2)
      } else {
        sigma_val <- sigma(model)
      }
      
      eff_size(
        contrast_objects[[i]],
        sigma = sigma_val,
        edf = df.residual(model),
        method = effect_config$method %||% "identity"
      )
    })
    
    names(effect_sizes) <- names(contrast_objects)
    
    # Bind effect sizes
    effect_df <- emm_bind(effect_sizes, kable = FALSE)
    
    # Add magnitude categories
    effect_df <- effect_df %>%
      mutate(
        magnitude = case_when(
          abs(effect.size) < 0.2 ~ "negligible",
          abs(effect.size) >= 0.2 & abs(effect.size) < 0.5 ~ "small",
          abs(effect.size) >= 0.5 & abs(effect.size) < 0.8 ~ "medium",
          abs(effect.size) >= 0.8 ~ "large"
        )
      )
    
    # Merge with contrast results
    processed_results$contrasts$all <- processed_results$contrasts$all %>%
      left_join(select(effect_df, Model, contrast, effect.size, magnitude), 
                by = c("Model", "contrast"))
    
    processed_results$contrasts$significant <- processed_results$contrasts$significant %>%
      left_join(select(effect_df, Model, contrast, effect.size, magnitude), 
                by = c("Model", "contrast"))
  }
  
  return(processed_results)
}

#' Save EMM Results to Files
#'
#' Internal function that saves processed emmeans results to CSV files.
#'
#' @param processed_results List of processed results
#' @param analysis_name Character string specifying the analysis name
#' @param results_dir Character string specifying the output directory
#'
#' @details
#' Creates analysis-specific subdirectories and saves both complete and
#' significant results as separate CSV files.
#'
#' @keywords internal
save_emm_results <- function(processed_results, analysis_name, results_dir) {
  
  # Create analysis-specific directory
  analysis_dir <- file.path(results_dir, paste0("emm-", analysis_name))
  dir.create(analysis_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Save contrast results
  if (!is.null(processed_results$contrasts)) {
    write.csv(
      processed_results$contrasts$all,
      file.path(analysis_dir, paste0(analysis_name, "-all-contrasts.csv")),
      row.names = FALSE
    )
    
    write.csv(
      processed_results$contrasts$significant,
      file.path(analysis_dir, paste0(analysis_name, "-significant-contrasts.csv")),
      row.names = FALSE
    )
  }
  
  # Save trend results
  if (!is.null(processed_results$trend_contrasts)) {
    write.csv(
      processed_results$trend_contrasts$all,
      file.path(analysis_dir, paste0(analysis_name, "-all-trends.csv")),
      row.names = FALSE
    )
    
    write.csv(
      processed_results$trend_contrasts$significant,
      file.path(analysis_dir, paste0(analysis_name, "-significant-trends.csv")),
      row.names = FALSE
    )
  }
}

#' Null Coalescing Operator
#'
#' Internal utility function that returns the first non-NULL value.
#'
#' @param x First value to check
#' @param y Value to return if x is NULL
#'
#' @return x if not NULL, otherwise y
#'
#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x

# =============================================================================
# EXAMPLE DESIGN CONFIGURATION FUNCTIONS
# =============================================================================

#' Create Time Comparison Configuration
#'
#' Creates a design configuration for simple time comparisons (e.g., baseline vs endpoint).
#'
#' @param time_points Numeric vector of time points to compare (default: c(0, 1))
#' @param time_labels Character vector of labels for time points (default: c("Baseline", "Endpoint"))
#' @param significance_threshold Numeric threshold for significance (default: 0.05)
#'
#' @return Named list containing design configuration
#'
#' @examples
#' \dontrun{
#' config <- create_time_comparison_config()
#' results <- run_flexible_emm_analysis(models, config, taxa_data)
#' }
#'
#' @export
create_time_comparison_config <- function(time_points = c(0, 1),
                                        time_labels = c("Baseline", "Endpoint"),
                                        significance_threshold = 0.05) {
  list(
    time_comparison = list(
      emmeans = list(
        specs = ~ time,
        at_values = list(time = time_points),
        level_labels = list(time = time_labels)
      ),
      contrasts = list(
        type = "pairwise",
        reverse = TRUE
      ),
      effect_sizes = list(
        calculate = TRUE,
        sigma_method = "random_effects",
        method = "identity"
      ),
      significance_threshold = significance_threshold
    )
  )
}

#' Create Treatment-Time Interaction Configuration
#'
#' Creates a design configuration for treatment comparisons with time interactions.
#'
#' @param time_points Numeric vector of time points (default: c(0, 1))
#' @param time_labels Character vector of labels for time points (default: c("Baseline", "Endpoint"))
#' @param treatment_labels Character vector of treatment labels (default: c("Control", "Intervention"))
#' @param significance_threshold Numeric threshold for significance (default: 0.05)
#'
#' @return Named list containing design configuration
#'
#' @examples
#' \dontrun{
#' config <- create_treatment_time_config(
#'   treatment_labels = c("Placebo", "Drug_A", "Drug_B")
#' )
#' results <- run_flexible_emm_analysis(models, config, taxa_data)
#' }
#'
#' @export
create_treatment_time_config <- function(time_points = c(0, 1),
                                       time_labels = c("Baseline", "Endpoint"),
                                       treatment_labels = c("Control", "Intervention"),
                                       significance_threshold = 0.05) {
  list(
    treatment_by_time = list(
      emmeans = list(
        specs = ~ treatment | time,
        at_values = list(time = time_points),
        level_labels = list(
          time = time_labels,
          treatment = treatment_labels
        )
      ),
      contrasts = list(
        type = "pairwise",
        by_vars = "time",
        reverse = TRUE
      ),
      trends = list(
        var = "time",
        specs = ~ treatment,
        contrasts = list(
          type = "pairwise",
          reverse = TRUE
        )
      ),
      effect_sizes = list(
        calculate = TRUE,
        sigma_method = "random_effects"
      ),
      significance_threshold = significance_threshold
    )
  )
}

#' Create Age Group Comparison Configuration
#'
#' Creates a design configuration for age group comparisons across time.
#'
#' @param age_labels Character vector of age group labels (default: c("12mths", "15mths", "18mths"))
#' @param time_labels Character vector of time labels (default: c("Baseline", "Endpoint"))
#' @param significance_threshold Numeric threshold for significance (default: 0.05)
#'
#' @return Named list containing design configuration
#'
#' @examples
#' \dontrun{
#' config <- create_age_group_config(
#'   age_labels = c("Young", "Middle", "Old")
#' )
#' results <- run_flexible_emm_analysis(models, config, taxa_data)
#' }
#'
#' @export
create_age_group_config <- function(age_labels = c("12mths", "15mths", "18mths"),
                                   time_labels = c("Baseline", "Endpoint"),
                                   significance_threshold = 0.05) {
  list(
    age_comparison = list(
      emmeans = list(
        specs = ~ age_group | time,
        level_labels = list(
          age_group = age_labels,
          time = time_labels
        )
      ),
      contrasts = list(
        type = "pairwise",
        by_vars = "time",
        method = "revpairwise"
      ),
      effect_sizes = list(
        calculate = TRUE,
        sigma_method = "random_effects"
      ),
      significance_threshold = significance_threshold
    )
  )
}
