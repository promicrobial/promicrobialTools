#' Extract Feature Counts with Taxonomy from Phyloseq Object
#'
#' This function takes a phyloseq object and creates a comprehensive dataframe
#' containing feature (OTU/ASV) counts along with their taxonomic classifications.
#' The resulting dataframe includes both the abundance data and full taxonomic
#' hierarchy for each feature.
#'
#' @param ps A phyloseq object containing OTU table and taxonomy table.
#'   Must have both otu_table and tax_table components.
#' @param merge_samples Logical. If TRUE, sums counts across all samples to get
#'   total abundance per feature. If FALSE (default), keeps individual sample counts.
#' @param include_sample_data Logical. If TRUE and merge_samples is FALSE,
#'   includes sample metadata in the output. Default is FALSE.
#' @param normalize_counts Logical. If TRUE, converts counts to relative abundance
#'   (proportions). Default is FALSE to keep raw counts.
#' @param min_abundance Numeric. Minimum total abundance threshold for including
#'   features. Features with total abundance below this value are excluded.
#'   Default is 0 (include all features).
#' @param tax_na_replace Character string to replace NA values in taxonomy.
#'   Default is "Unknown".
#'
#' @return A data.frame with the following structure:
#'   \itemize{
#'     \item Feature columns: One column per sample (if merge_samples = FALSE) or
#'       one "Total_Count" column (if merge_samples = TRUE)
#'     \item Taxonomy columns: Kingdom, Phylum, Class, Order, Family, Genus, Species
#'       (depending on available taxonomic ranks in the phyloseq object)
#'     \item Feature_ID: Row names converted to a column for easier handling
#'   }
#'
#' @examples
#' \dontrun{
#' # Basic usage with phyloseq object
#' counts_tax_df <- taxaCount(my_phyloseq)
#'
#' # Merge samples to get total counts per feature
#' total_counts_df <- taxaCount(my_phyloseq, merge_samples = TRUE)
#'
#' # Get relative abundances with minimum threshold
#' rel_abund_df <- taxaCount(
#'   my_phyloseq,
#'   normalize_counts = TRUE,
#'   min_abundance = 0.001
#' )
#'
#' # Include sample metadata
#' full_df <- taxaCount(
#'   my_phyloseq,
#'   include_sample_data = TRUE
#' )
#' }
#'
#' @details
#' The function performs the following operations:
#' \enumerate{
#'   \item Validates the input phyloseq object
#'   \item Extracts the OTU/ASV count table
#'   \item Extracts the taxonomy table
#'   \item Optionally filters features by minimum abundance
#'   \item Optionally normalizes counts to relative abundance
#'   \item Merges count and taxonomy data into a single dataframe
#'   \item Optionally includes sample metadata
#' }
#'
#' @note
#' \itemize{
#'   \item If the phyloseq object lacks a taxonomy table, only count data is returned
#'   \item Sample names are preserved as column names in the count matrix
#'   \item Taxonomic ranks are automatically detected from the taxonomy table
#'   \item Features with all zero counts are automatically removed
#' }
#'
#' @seealso
#' \code{\link[phyloseq]{otu_table}}, \code{\link[phyloseq]{tax_table}},
#' \code{\link[phyloseq]{sample_data}}
#'
#' @importFrom phyloseq otu_table tax_table sample_data taxa_are_rows
#' @export
taxaCount <- function(ps,
                      merge_samples = FALSE,
                      include_sample_data = FALSE,
                      normalize_counts = FALSE,
                      min_abundance = 0,
                      tax_na_replace = "Unknown") {
  
  # Input validation
  if (!inherits(ps, "phyloseq")) {
    stop("Input must be a phyloseq object")
  }
  
  if (is.null(otu_table(ps))) {
    stop("Phyloseq object must contain an OTU table")
  }
  
  # Extract OTU table
  otu_mat <- as(otu_table(ps), "matrix")
  
  # Ensure taxa are rows (phyloseq convention can vary)
  if (!taxa_are_rows(ps)) {
    otu_mat <- t(otu_mat)
  }
  
  # Filter by minimum abundance if specified
  if (min_abundance > 0) {
    if (normalize_counts) {
      # For relative abundance, use proportional threshold
      total_props <- rowSums(otu_mat) / sum(otu_mat)
      keep_taxa <- total_props >= min_abundance
    } else {
      # For raw counts, use absolute threshold
      keep_taxa <- rowSums(otu_mat) >= min_abundance
    }
    
    otu_mat <- otu_mat[keep_taxa, , drop = FALSE]
    
    if (nrow(otu_mat) == 0) {
      warning("No features remain after minimum abundance filtering")
      return(data.frame())
    }
  }
  
  # Remove features with all zero counts
  non_zero_features <- rowSums(otu_mat) > 0
  otu_mat <- otu_mat[non_zero_features, , drop = FALSE]
  
  # Normalize counts if requested
  if (normalize_counts) {
    otu_mat <- sweep(otu_mat, 2, colSums(otu_mat), FUN = "/")
  }
  
  # Merge samples if requested
  if (merge_samples) {
    total_counts <- rowSums(otu_mat)
    count_df <- data.frame(
      Feature_ID = names(total_counts),
      Total_Count = total_counts,
      stringsAsFactors = FALSE
    )
  } else {
    # Convert to dataframe with Feature_ID column
    count_df <- data.frame(
      Feature_ID = rownames(otu_mat),
      otu_mat,
      stringsAsFactors = FALSE
    )
  }
  
  # Extract taxonomy if available
  if (!is.null(tax_table(ps))) {
    tax_mat <- as(tax_table(ps), "matrix")
    
    # Filter taxonomy to match remaining features
    tax_mat <- tax_mat[rownames(tax_mat) %in% count_df$Feature_ID, , drop = FALSE]
    
    # Replace NA values in taxonomy
    tax_mat[is.na(tax_mat)] <- tax_na_replace
    
    # Convert to dataframe
    tax_df <- data.frame(
      Feature_ID = rownames(tax_mat),
      tax_mat,
      stringsAsFactors = FALSE
    )
    
    # Merge count and taxonomy data
    result_df <- merge(count_df, tax_df, by = "Feature_ID", all.x = TRUE)
    
  } else {
    warning("No taxonomy table found in phyloseq object. Returning counts only.")
    result_df <- count_df
  }
  
  # Add sample data if requested and not merging samples
  if (include_sample_data && !merge_samples && !is.null(sample_data(ps))) {
    sample_df <- data.frame(sample_data(ps), stringsAsFactors = FALSE)
    sample_df$Sample_ID <- rownames(sample_df)
    
    # Reshape to long format to merge with feature data
    count_cols <- setdiff(colnames(result_df), c("Feature_ID", colnames(tax_df)))
    
    if (length(count_cols) > 0) {
      # Melt the count data
      long_df <- reshape2::melt(
        result_df,
        id.vars = c("Feature_ID", setdiff(colnames(tax_df), "Feature_ID")),
        variable.name = "Sample_ID",
        value.name = "Count"
      )
      
      # Merge with sample data
      result_df <- merge(long_df, sample_df, by = "Sample_ID", all.x = TRUE)
    }
  }
  
  # Reorder columns for better readability
  if (!is.null(tax_table(ps))) {
    # Put Feature_ID first, then taxonomy, then counts
    tax_cols <- setdiff(colnames(tax_df), "Feature_ID")
    count_cols <- setdiff(colnames(result_df), c("Feature_ID", tax_cols))
    
    if (include_sample_data && !merge_samples) {
      # For long format with sample data
      col_order <- c("Feature_ID", tax_cols, "Sample_ID", "Count", 
                     setdiff(colnames(result_df), 
                             c("Feature_ID", tax_cols, "Sample_ID", "Count")))
    } else {
      col_order <- c("Feature_ID", tax_cols, count_cols)
    }
    
    result_df <- result_df[, col_order[col_order %in% colnames(result_df)]]
  }
  
  # Add attributes for metadata
  attr(result_df, "phyloseq_summary") <- list(
    n_features = nrow(result_df),
    n_samples = ifelse(merge_samples, 1, ncol(otu_table(ps))),
    normalized = normalize_counts,
    merged_samples = merge_samples,
    min_abundance_filter = min_abundance
  )
  
  return(result_df)
}