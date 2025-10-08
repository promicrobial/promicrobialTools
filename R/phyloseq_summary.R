#' Create a formatted summary table for phyloseq objects
#'
#' This function generates a professional-looking summary table of phyloseq object
#' characteristics using the microbiome package and gt table formatting. The output
#' is compatible with both HTML and PDF rendering in Quarto documents.
#'
#' @param phyloseq_obj A phyloseq object containing microbiome data
#' @param title Character string specifying the table title. Default is "Phyloseq Object Summary"
#' @param subtitle Character string specifying the table subtitle. Default is "Dataset characteristics and quality metrics"
#' @param format Character string specifying output format. One of "gt" (default), "kable", or "df".
#'   "gt" produces a styled gt table (best for HTML), "kable" produces a knitr::kable table
#'   (better for PDF), and "df" returns a plain data frame.
#' @param add_sample_vars Logical indicating whether to include sample variables information.
#'   Default is TRUE. When TRUE, sample variables are added as a source note.
#' @param decimal_places Integer specifying number of decimal places for percentages and averages.
#'   Default is 1.
#'
#' @return Depending on the format parameter:
#' \itemize{
#'   \item "gt": A gt table object
#'   \item "kable": A knitr::kable object
#'   \item "df": A data frame with Metric and Value columns
#' }
#'
#' @examples
#' \dontrun{
#' # Basic usage with default gt formatting
#' phyloseq_summary(my_phyloseq)
#' 
#' # For PDF rendering, use kable format
#' phyloseq_summary(my_phyloseq, format = "kable")
#' 
#' # Custom title and subtitle
#' phyloseq_summary(
#'   my_phyloseq, 
#'   title = "Gut Microbiome Analysis",
#'   subtitle = "16S rRNA sequencing results"
#' )
#' 
#' # Return plain data frame for further processing
#' summary_df <- phyloseq_summary(my_phyloseq, format = "df")
#' }
#'
#' @details
#' The function extracts the following metrics from the phyloseq object:
#' \itemize{
#'   \item Read count statistics (min, max, total, average, median)
#'   \item Data quality metrics (sparsity, singletons)
#'   \item Sample variables information (optional)
#' }
#' 
#' For optimal rendering:
#' \itemize{
#'   \item Use format = "gt" for HTML output (default)
#'   \item Use format = "kable" for PDF output
#'   \item The gt format includes styling that may not render properly in PDF
#' }
#'
#' @seealso \code{\link[microbiome]{summarize_phyloseq}}, \code{\link[gt]{gt}}, \code{\link[knitr]{kable}}
#'
#' @importFrom microbiome summarize_phyloseq
#' @importFrom gt gt tab_header tab_row_group cols_label cols_align tab_options tab_style cell_text cells_column_labels tab_source_note
#' @importFrom knitr kable
#' @importFrom dplyr %>%
#'
#' @export
phyloseq_summary <- function(phyloseq_obj, 
                                         title = "Phyloseq Object Summary",
                                         subtitle = "Dataset characteristics and quality metrics",
                                         format = c("gt", "kable", "df"),
                                         add_sample_vars = TRUE,
                                         decimal_places = 1) {
  
  # Input validation
  if (!requireNamespace("microbiome", quietly = TRUE)) {
    stop("Package 'microbiome' is required but not installed. Please install it with: install.packages('microbiome')")
  }
  
  if (!inherits(phyloseq_obj, "phyloseq")) {
    stop("phyloseq_obj must be a phyloseq object. Current class: ", class(phyloseq_obj)[1])
  }
  
  if (!is.character(title) || length(title) != 1) {
    stop("title must be a single character string")
  }
  
  if (!is.character(subtitle) || length(subtitle) != 1) {
    stop("subtitle must be a single character string")
  }
  
  format <- match.arg(format)
  
  if (!is.logical(add_sample_vars) || length(add_sample_vars) != 1) {
    stop("add_sample_vars must be a single logical value (TRUE or FALSE)")
  }
  
  if (!is.numeric(decimal_places) || length(decimal_places) != 1 || decimal_places < 0) {
    stop("decimal_places must be a single non-negative number")
  }
  
  # Check for required packages based on format
  if (format == "gt" && !requireNamespace("gt", quietly = TRUE)) {
    warning("Package 'gt' is required for gt format but not installed. Falling back to 'kable' format.")
    format <- "kable"
  }
  
  if (format == "kable" && !requireNamespace("knitr", quietly = TRUE)) {
    stop("Package 'knitr' is required for kable format but not installed.")
  }
  
  # Get the summary from microbiome package
  tryCatch({
    suppressMessages(summary_list <- microbiome::summarize_phyloseq(phyloseq_obj))
  }, error = function(e) {
    stop("Error generating phyloseq summary: ", e$message)
  })
  
  # Validate summary_list structure
  if (!is.list(summary_list) || length(summary_list) < 10) {
    stop("Unexpected format from microbiome::summarize_phyloseq(). Please check your phyloseq object.")
  }
  
  # Helper function to safely extract numeric values
  safe_extract_numeric <- function(text, pattern, default = 0) {
    tryCatch({
      extracted <- gsub(pattern, "", text)
      as.numeric(extracted)
    }, error = function(e) {
      warning("Could not extract numeric value from: ", text, ". Using default: ", default)
      default
    })
  }
  
  # Extract and clean the main statistics with error handling
  min_reads <- safe_extract_numeric(summary_list[[1]], "^\\d+\\]\\s*Min\\. number of reads = ")
  max_reads <- safe_extract_numeric(summary_list[[2]], "^\\d+\\]\\s*Max\\. number of reads = ")
  total_reads <- safe_extract_numeric(summary_list[[3]], "^\\d+\\]\\s*Total number of reads = ")
  avg_reads <- safe_extract_numeric(summary_list[[4]], "^\\d+\\]\\s*Average number of reads = ")
  median_reads <- safe_extract_numeric(summary_list[[5]], "^\\d+\\]\\s*Median number of reads = ")
  sparsity <- safe_extract_numeric(summary_list[[6]], "^\\d+\\]\\s*Sparsity = ")
  low_reads <- gsub("^\\d+\\]\\s*Any OTU sum to 1 or less\\? ", "", summary_list[[7]])
  singletons <- safe_extract_numeric(summary_list[[8]], "^\\d+\\]\\s*Number of singletons = ")
  singleton_pct <- safe_extract_numeric(summary_list[[9]], ".*\\)")
  
  # Create the data frame
  df <- data.frame(
    Metric = c("Minimum reads/sample", 
               "Maximum reads/sample", 
               "Total reads", 
               "Average reads/sample", 
               "Median reads/sample", 
               "Data sparsity", 
               "OTUs with ≤1 read", 
               "Singleton OTUs", 
               "Singleton percentage"),
    Value = c(
      format(min_reads, big.mark = ","),
      format(max_reads, big.mark = ","),
      format(total_reads, big.mark = ","),
      format(round(avg_reads, decimal_places), big.mark = ","),
      format(median_reads, big.mark = ","),
      paste0(round(sparsity * 100, decimal_places), "%"),
      low_reads,
      format(singletons, big.mark = ","),
      paste0(round(singleton_pct, decimal_places), "%")
    ),
    stringsAsFactors = FALSE
  )
  
  # Return based on format
if (format == "df") {
  if (add_sample_vars && length(summary_list) >= 11) {
    # Store the full list and truncated display version
    attr(df, "sample_variables") <- summary_list[[11]]
    n_vars <- safe_extract_numeric(summary_list[[10]], "^\\d+\\]\\s*Number of sample variables are: ")
    attr(df, "n_sample_variables") <- n_vars
    
    # Create display version
    if (length(summary_list[[11]]) > 10) {
      displayed_vars <- summary_list[[11]][1:10]
      attr(df, "sample_variables_display") <- paste(c(displayed_vars, "..."), collapse = ", ")
    } else {
      attr(df, "sample_variables_display") <- paste(summary_list[[11]], collapse = ", ")
    }
  }
  return(df)
}

  
if (format == "kable") {
  table_out <- knitr::kable(df, 
                           caption = paste0(title, ": ", subtitle),
                           format = "html",
                           table.attr = "class='table table-striped'")
  
  if (add_sample_vars && length(summary_list) >= 11) {
    n_vars <- safe_extract_numeric(summary_list[[10]], "^\\d+\\]\\s*Number of sample variables are: ")
    
    # Limit display to first 10 variables if there are more than 10
    if (length(summary_list[[11]]) > 10) {
      displayed_vars <- summary_list[[11]][1:10]
      sample_vars <- paste(c(displayed_vars, "..."), collapse = ", ")
    } else {
      sample_vars <- paste(summary_list[[11]], collapse = ", ")
    }
    
    attr(table_out, "sample_vars_note") <- paste0("Sample variables (", n_vars, "): ", sample_vars)
  }
  
  return(table_out)
}
  
  # GT format (default)
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required for gt format but not installed.")
  }
  
  gt_table <- df %>%
    gt::gt() %>%
    gt::tab_header(
      title = title,
      subtitle = subtitle
    ) %>%
    gt::tab_row_group(
      label = "📊 Read Statistics",
      rows = 1:5
    ) %>%
    gt::tab_row_group(
      label = "🔍 Data Quality Metrics", 
      rows = 6:9
    ) %>%
    gt::cols_label(
      Metric = "Metric",
      Value = "Value"
    ) %>%
    gt::cols_align(
      align = "left",
      columns = "Metric"
    ) %>%
    gt::cols_align(
      align = "right", 
      columns = "Value"
    ) %>%
    gt::tab_options(
      row_group.font.weight = "bold",
      row_group.background.color = "#f8f9fa",
      table.font.size = "14px",
      heading.title.font.size = "18px",
      heading.subtitle.font.size = "14px"
    ) %>%
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels()
    )
  
# Add sample variables information
  if (add_sample_vars && length(summary_list) >= 11) {
    n_vars <- safe_extract_numeric(summary_list[[10]], "^\\d+\\]\\s*Number of sample variables are: ")
    
    # Limit display to first 10 variables if there are more than 10
    if (length(summary_list[[11]]) > 10) {
      displayed_vars <- summary_list[[11]][1:10]
      sample_vars <- paste(c(displayed_vars, "..."), collapse = ", ")
    } else {
      sample_vars <- paste(summary_list[[11]], collapse = ", ")
    }
    
    gt_table <- gt_table %>%
      gt::tab_source_note(
        source_note = paste0("Sample variables (", n_vars, "): ", sample_vars)
      )
  }
  
  return(gt_table)
}