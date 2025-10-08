# Function to clean data variables
#' Sanitize Data Frame Columns and Column Names
#'
#' @description
#' Applies consistent cleaning to both column names and character column contents
#' in a single pass. Handles whitespace, special characters, and case normalization.
#'
#' @param df A data frame to sanitize
#' @param convert_spaces_to Character to replace spaces with (default: "_")
#' @param case Function to apply for case conversion (default: tolower)
#' @param trim_whitespace Logical, whether to trim whitespace (default: TRUE)
#'
#' @return A data frame with sanitized column names and character column contents
#'
#' @import stringr
#' @import dplyr
#' 
#' @examples
#' df <- data.frame(
#'   "Column Name" = c(" TEXT ", "More TEXT"),
#'   "Another.Column" = c("A B", "C D")
#' )
#' sanitiseData(df)
#' @export
sanitiseData <- function(df, 
                        convert_spaces_to = "_", 
                        case = tolower,
                        trim_whitespace = TRUE) {
  
  # Input validation
  if (!is.data.frame(df)) {
    stop("Input must be a data frame")
  }
  
  # Helper function to clean text consistently
  cleanText <- function(x) {
    if (is.character(x)) {
      # Trim whitespace if requested
      if (trim_whitespace) {
        x <- str_trim(x, "both")
      }
      
      # Convert spaces and special characters
      x <- str_replace_all(x, "\\s+", convert_spaces_to)  # Replace spaces
      x <- str_replace_all(x, "[^[:alnum:]_-]", "")       # Remove special characters except _ and -
      
      # Apply case conversion
      x <- case(x)
    }
    return(x)
  }
  
  # Clean column names
  names(df) <- names(df) %>%
    str_remove("^X") %>%                    # Remove leading X
    str_remove("^\\.+") %>%                 # Remove leading dots
    str_trim("both") %>%                    # Remove leading and trailing whitespace
    str_replace_all("\\.+", convert_spaces_to) %>%  # Replace dots with specified character
    str_replace_all("\\s+", convert_spaces_to) %>%  # Replace spaces with specified character
    str_replace_all("[^[:alnum:]_-]", "") %>%      # Remove other special characters
    case()                                          # Apply case conversion
  
  # Clean character columns
  df <- df %>%
    mutate(
      across(
        where(is.character),
        cleanText
      )
    )
  
  return(df)
}

#' @rdname sanitiseData
#' @export
sanitizeData <- sanitiseData