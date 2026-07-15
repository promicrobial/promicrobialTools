#' Load and Combine Multiple Data Files
#'
#' @description
#' Reads multiple files from a directory matching a pattern, combines them into a single
#' data frame, and exports the result. Each file's content is labeled with its source
#' filename in a 'sample' column.
#'
#' @param file_dir Character string specifying the directory containing input files
#' @param file_pattern Character string specifying the file pattern to match (e.g., "*.tsv")
#' @param header Logical indicating if files have headers (default: TRUE)
#' @param sep Character specifying the field separator in files (default: "\t")
#' @param skip Numeric indicating number of lines to skip before reading data (default: 0)
#' @param output Character string specifying the path for the output file
#' @param clean_names Logical indicating whether to clean column names (default: TRUE)
#' @param encoding Character string specifying file encoding (default: "UTF-8")
#' @param na.strings Character vector of strings to be interpreted as NA values (default: c("NA", "", "NULL"))
#'
#' @return Invisibly returns the combined data frame
#'
#' @details
#' The function performs the following operations:
#' * Validates input parameters and file accessibility
#' * Reads all matching files from the specified directory
#' * Combines files into a single data frame with a 'sample' column indicating source
#' * Optionally cleans column names for compatibility
#' * Exports the combined data to the specified output file
#'
#' @note
#' * Requires data.table and dplyr packages
#' * Handles missing columns across files using data.table's fill parameter
#' * Preserves the original working directory
#' * Attempts to standardize column types across files
#'
#' @examples
#' \dontrun{
#' loadcombine(
#'   file_dir = "path/to/files",
#'   file_pattern = "*.tsv",
#'   sep = "\t",
#'   output = "path/to/output.csv",
#'   clean_names = TRUE
#' )
#' }
#'
#' @export
#'
#' @importFrom data.table data.table::rbindlist data.table::fread
#' @importFrom dplyr %>%
#' @importFrom tools file_ext
#'
loadcombine <- function(
  file_dir,
  file_pattern,
  header = TRUE,
  sep = "\t",
  skip = 0,
  output,
  clean_names = TRUE,
  encoding = "UTF-8",
  na.strings = c("NA", "", "NULL")
) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop(
      "Package \"data.table\" must be installed to use this function.",
      call. = FALSE
    )
  }

  # Input validation
  if (!dir.exists(file_dir)) {
    stop("Directory does not exist: ", file_dir)
  }

  if (!is.character(file_pattern)) {
    stop("file_pattern must be a character string")
  }

  if (!is.character(output)) {
    stop("output must be a character string")
  }

  # Validate output directory exists
  output_dir <- dirname(output)
  if (!dir.exists(output_dir)) {
    stop("Output directory does not exist: ", output_dir)
  }

  # Check output file extension is valid
  valid_extensions <- c("csv", "tsv", "txt")
  output_ext <- tolower(tools::file_ext(output))
  if (!output_ext %in% valid_extensions) {
    stop(
      "Output file must have one of these extensions: ",
      paste(valid_extensions, collapse = ", ")
    )
  }

  # Get file paths
  all_file_paths <- list.files(
    path = file_dir,
    pattern = file_pattern,
    full.names = TRUE
  )

  if (length(all_file_paths) == 0) {
    stop("No files found matching pattern: ", file_pattern)
  }

  # Create progress bar
  pb <- txtProgressBar(min = 0, max = length(all_file_paths), style = 3)

  # Load files with error handling
  all_files <- vector("list", length(all_file_paths))
  for (i in seq_along(all_file_paths)) {
    tryCatch(
      {
        all_files[[i]] <- data.table::fread(
          all_file_paths[i],
          header = header,
          sep = sep,
          skip = skip,
          encoding = encoding,
          na.strings = na.strings
        )
      },
      error = function(e) {
        warning(
          "Error reading file: ",
          basename(all_file_paths[i]),
          "\n",
          e$message
        )
        NULL
      }
    )
    setTxtProgressBar(pb, i)
  }
  close(pb)

  # Remove NULL entries (failed reads)
  all_files <- all_files[!sapply(all_files, is.null)]

  if (length(all_files) == 0) {
    stop("No files were successfully read")
  }

  # Get filenames
  all_filenames <- basename(all_file_paths)

  # Clean column names if requested
  if (clean_names) {
    all_files <- lapply(all_files, function(df) {
      names(df) <- make.names(names(df), unique = TRUE)
      return(df)
    })
  }

  # Combine files
  all_result <- data.table::rbindlist(
    mapply(
      function(df, name) {
        df[, sample := name]
        return(df)
      },
      all_files,
      all_filenames,
      SIMPLIFY = FALSE
    ),
    fill = TRUE
  )

  # Export results based on file extension
  tryCatch(
    {
      switch(
        output_ext,
        "csv" = data.table::fwrite(all_result, file = output),
        "tsv" = data.table::fwrite(all_result, file = output, sep = "\t"),
        "txt" = data.table::fwrite(all_result, file = output, sep = "\t")
      )
    },
    error = function(e) {
      stop("Error writing output file: ", e$message)
    }
  )

  # Return combined data invisibly
  invisible(all_result)
}

#' Load Multiple Data Files into a Named List
#'
#' @description
#' Reads multiple files from a directory matching a pattern and returns them as a named
#' list where each element is a data frame named after its source file (without extension).
#'
#' @param file_dir Character string specifying the directory containing input files (default: "./")
#' @param file_pattern Character string specifying the file pattern to match (default: "*.csv")
#' @param header Logical indicating if files have headers (default: TRUE)
#' @param sep Character specifying the field separator in files (default: ",")
#' @param skip Numeric indicating number of lines to skip before reading data (default: 0)
#' @param encoding Character string specifying file encoding (default: "UTF-8")
#' @param clean_names Logical indicating whether to clean column names (default: TRUE)
#' @param na.strings Character vector of strings to be interpreted as NA values (default: c("NA", "", "NULL"))
#' @param verbose Logical indicating whether to show progress and information (default: TRUE)
#'
#' @return A named list of data frames, where names are derived from source filenames
#'
#' @details
#' The function performs the following operations:
#' * Validates input parameters and file accessibility
#' * Reads all matching files from the specified directory
#' * Creates a named list of data frames
#' * Optionally cleans column names for compatibility
#' * Provides progress feedback if verbose is TRUE
#'
#' @note
#' * Handles missing or corrupted files gracefully
#' * Preserves original column types where possible
#' * Supports various file formats through pattern matching
#' * Names are sanitized for R compatibility
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' data_list <- loadAll()
#'
#' # Specific directory and pattern
#' data_list <- loadAll(
#'   file_dir = "path/to/files",
#'   file_pattern = "*.tsv",
#'   sep = "\t"
#' )
#'
#' # Access individual datasets
#' first_dataset <- data_list[[1]]
#' named_dataset <- data_list[["filename"]]
#' }
#'
#' @export
#'
#' @importFrom stringr str_remove
#' @importFrom dplyr %>%
#' @importFrom tools file_path_sans_ext
#' @importFrom utils read.table setTxtProgressBar txtProgressBar
#'
loadAll <- function(file_dir = "./",
                   file_pattern = "*.csv",
                   header = TRUE,
                   sep = ",",
                   skip = 0,
                   encoding = "UTF-8",
                   clean_names = TRUE,
                   na.strings = c("NA", "", "NULL"),
                   verbose = TRUE) {
  
  # Input validation
  if (!dir.exists(file_dir)) {
    stop("Directory does not exist: ", file_dir)
  }
  
  if (!is.character(file_pattern)) {
    stop("file_pattern must be a character string")
  }
  
  # Get file paths
  all_file_paths <- list.files(
    path = file_dir,
    pattern = file_pattern,
    full.names = TRUE
  )
  
  if (length(all_file_paths) == 0) {
    stop("No files found matching pattern: ", file_pattern)
  }
  
  # Initialize progress bar if verbose
  if (verbose) {
    cat("Found", length(all_file_paths), "files matching pattern\n")
    pb <- txtProgressBar(min = 0, max = length(all_file_paths), style = 3)
  }
  
  # Initialize results list
  all_files <- vector("list", length(all_file_paths))
  
  # Load files with error handling
  for (i in seq_along(all_file_paths)) {
    tryCatch({
      all_files[[i]] <- read.table(
        all_file_paths[[i]],
        header = header,
        sep = sep,
        skip = skip,
        encoding = encoding,
        na.strings = na.strings,
        stringsAsFactors = FALSE,
        check.names = clean_names
      )
      
      if (verbose) setTxtProgressBar(pb, i)
      
    }, error = function(e) {
      warning(
        "Error reading file: ",
        basename(all_file_paths[i]),
        "\n",
        e$message
      )
      NULL
    })
  }
  
  if (verbose) {
    close(pb)
    cat("\n")
  }
  
  # Remove NULL entries (failed reads)
  valid_files <- !sapply(all_files, is.null)
  all_files <- all_files[valid_files]
  all_file_paths <- all_file_paths[valid_files]
  
  if (length(all_files) == 0) {
    stop("No files were successfully read")
  }
  
  # Generate clean names for the list elements
  file_names <- all_file_paths %>%
    basename() %>%
    tools::file_path_sans_ext() %>%
    make.names(unique = TRUE)
  
  # Name the list elements
  names(all_files) <- file_names
  
  # Additional cleaning and validation of column names if requested
  if (clean_names) {
    all_files <- lapply(all_files, function(df) {
      names(df) <- make.names(names(df), unique = TRUE)
      return(df)
    })
  }
  
  # Print summary if verbose
  if (verbose) {
    cat("Successfully loaded", length(all_files), "files\n")
    cat("Available datasets:", paste(names(all_files), collapse = ", "), "\n")
  }
  
  # Add class and attributes for potential method dispatch
  class(all_files) <- c("loaded_files", class(all_files))
  attr(all_files, "source_dir") <- normalizePath(file_dir)
  attr(all_files, "pattern") <- file_pattern
  attr(all_files, "load_time") <- Sys.time()
  
  return(all_files)
}

#' Print method for loaded_files objects
#' @param x Character, file directory
#' @export
print.loaded_files <- function(x) {
  cat("Loaded Files Object\n")
  cat("Source directory:", attr(x, "source_dir"), "\n")
  cat("Pattern matched:", attr(x, "pattern"), "\n")
  cat("Load time:", attr(x, "load_time"), "\n")
  cat("Number of datasets:", length(x), "\n\n")
  cat("Available datasets:\n")
  for (i in seq_along(x)) {
    cat(sprintf("%s: %d rows, %d columns\n",
                names(x)[i], nrow(x[[i]]), ncol(x[[i]])))
  }
}