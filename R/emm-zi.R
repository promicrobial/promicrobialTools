#estimated marginal means for zero inflated models

# Assuming you have your zero-inflated model:
# zi_model <- zeroinfl(count ~ treatment + covariate | treatment, data = data)

# For the zero-inflation component:
# 1. Get emmeans for zero-inflation probabilities
#emm_zi <- emmeans(zi_model, specs = "treatment", component = "zero")

# 2. Get contrasts
#contrasts_zi <- pairs(emm_zi)

# 3. Convert to odds ratios
#or_zi <- exp(contrasts_zi)

# 4. To convert to Cohen's d using the table approach:
# First get the baseline probability (P₀) for reference group
#P0 <- as.data.frame(emm_zi)[1, 3]  # assuming first level is reference

# Function to convert OR to Cohen's d using linear interpolation
or_to_cohens_d <- function(OR, P0) {
  p1 <- (OR * P0)/(1 - P0 + (OR * P0))
  d <- qnorm(p1) - qnorm(P0)
  return(d)
}

# Comprehensive output function
summarize_zi_contrasts <- function(emm_zi, contrasts_zi, model) {
  y <- model$frame$y
  P0 <- sporp(y, silent = TRUE)$sparsity

  # Get contrasts with CIs
  contrast_summary <- confint(contrasts_zi, component = "zi") %>%
    as.data.frame() %>%
    mutate(
      OR = exp(estimate),
      OR_lower = exp(asymp.LCL),
      OR_upper = exp(asymp.UCL),
      P0 = P0,
      cohens_d = mapply(or_to_cohens_d,.data$OR, .data$P0)
    )
  
  # Add effect size interpretation
  contrast_summary <- contrast_summary %>%
    mutate(
      OR_magnitude = case_when(
        abs(cohens_d) < 0.2 ~ "negligible",
        abs(cohens_d) < 0.5 ~ "small",
        abs(cohens_d) < 0.8 ~ "medium",
        TRUE ~ "large"
      )
    )
  
  return(contrast_summary)
}