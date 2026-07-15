#' Process Wide-Format Rating Data to Long Format
#'
#' @description
#' Transforms data from wide format (with multiple x-y pairs) to long format,
#' optionally preserving a grouping variable. The function handles paired x-y ratings
#' from multiple sources (e.g. replicates) and creates a standardized ID.
#'
#' @param data A data frame containing paired x-y ratings in wide format.
#'             Column names should follow the pattern 'x1', 'y1', 'x2', 'y2', etc.
#' @param group_var Optional. The name of the grouping variable column (unquoted).
#'                 If NULL (default), no grouping is preserved.
#'
#' @return A tibble in long format with columns:
#'         - x: x-values from the original pairs
#'         - y: y-values from the original pairs
#'         - id: Replicate ID in the format "R1", "R2", etc.
#'         - group: (if group_var is specified) The grouping variable values
#'
#' @examples
#' \dontrun{
#' # Without grouping
#' result1 <- process_data(data)
#'
#' # With grouping
#' result2 <- process_data(data, group)
#' }
#'
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate select 
#' @importFrom tibble rowid_to_column
#' @importFrom rlang enquo is_null quo_name
#'
#' @export
process_data <- function(data, group_var = NULL) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("Input must be a data frame or tibble")
  }

  # Check for empty data
  if (nrow(data) == 0 || ncol(data) == 0) {
    stop("Input data frame is empty")
  }

  # Validate column names format
  x_cols <- grep("^x\\d+$", names(data))
  y_cols <- grep("^y\\d+$", names(data))

  if (length(x_cols) == 0 || length(y_cols) == 0) {
    stop("Data must contain columns named 'x1', 'y1', etc.")
  }

  if (length(x_cols) != length(y_cols)) {
    stop("Unequal number of x and y columns")
  }

  # Check for proper x-y pairing
  x_numbers <- as.numeric(gsub("x", "", names(data)[x_cols]))
  y_numbers <- as.numeric(gsub("y", "", names(data)[y_cols]))

  if (!all(sort(x_numbers) == sort(y_numbers))) {
    stop("x and y columns must be properly paired (x1-y1, x2-y2, etc.)")
  }

  # Handle grouping variable
  group_quo <- enquo(group_var)

  if (!is_null(group_var)) {
    if (!quo_name(group_quo) %in% names(data)) {
      stop("Specified grouping variable not found in data")
    }
  }

  # Process data
  if (is_null(group_var)) {
    result <- data %>%
      rowid_to_column("observation") %>%
      pivot_longer(
        cols = -c(observation),
        names_to = c(".value", "id"),
        names_pattern = "([xy])(\\d+)"
      ) %>%
      mutate(id = paste0("R", id)) %>%
      select(!observation)
  } else {
    result <- data %>%
      rowid_to_column("observation") %>%
      pivot_longer(
        cols = -c(observation, !!group_quo),
        names_to = c(".value", "id"),
        names_pattern = "([xy])(\\d+)"
      ) %>%
      mutate(id = paste0("R", id)) %>%
      select(!observation)
  }

  return(result)
}