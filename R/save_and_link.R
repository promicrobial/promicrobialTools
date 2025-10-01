#' Get Project Root Directory
#'
#' Automatically detects the project root directory by searching for common
#' project indicators like _quarto.yml, .git, *.Rproj files, etc.
#'
#' @return Character string of the project root directory path
#'
#' @details
#' The function walks up the directory tree from the current file location
#' searching for project indicators in the following order:
#' \itemize{
#'   \item _quarto.yml (Quarto project)
#'   \item .git (Git repository)
#'   \item *.Rproj (RStudio project)
#'   \item _targets.R (targets pipeline)
#'   \item DESCRIPTION (R package)
#' }
#' If no project root is found, it returns the directory containing the current file
#' or the current working directory as a fallback.
#'
#' @examples
#' \dontrun{
#' root <- get_project_root()
#' print(root)
#' }
#'
#' @export
get_project_root <- function() {
  current_file <- knitr::current_input(dir = TRUE)
  
  if (!is.null(current_file)) {
    # Walk up the directory tree to find project root
    # Look for common project indicators
    current_dir <- dirname(current_file)
    
    while (current_dir != dirname(current_dir)) {  # Not at filesystem root
      indicators <- c("_quarto.yml", ".git", "*.Rproj", "_targets.R", "DESCRIPTION")
      
      for (indicator in indicators) {
        if (length(Sys.glob(file.path(current_dir, indicator))) > 0) {
          return(current_dir)
        }
      }
      current_dir <- dirname(current_dir)
    }
    
    # If no project root found, use the directory containing the current file
    return(dirname(current_file))
  }
  
  return(getwd())
}

#' Create Results Directory Structure
#'
#' Creates a structured results directory in the project root based on either
#' the parent directory name, the current filename, or both combined.
#'
#' @param directory_naming Character string specifying the naming convention.
#'   Options are:
#'   \itemize{
#'     \item "parent" (default): Uses parent directory name
#'     \item "filename": Uses current filename without extension
#'     \item "both": Uses both parent directory and filename separated by underscore
#'   }
#'
#' @return Character string of the created results directory path
#'
#' @details
#' The function creates a directory structure: `project_root/results/subdirectory/`
#' where subdirectory is determined by the directory_naming parameter:
#' \itemize{
#'   \item "parent": Parent directory name (e.g., "analysis" from "/path/analysis/file.qmd")
#'   \item "filename": Current filename without extension (e.g., "file" from "file.qmd")
#'   \item "both": Combined format (e.g., "analysis_file" from "/path/analysis/file.qmd")
#' }
#' 
#' Uses regex pattern `"(?<=\\/)[^\\/]+(?=\\/[^\\/]+$)"` to extract parent directory name.
#'
#' @examples
#' \dontrun{
#' # Use parent directory name (default)
#' results_dir <- create_results_dir()
#' results_dir <- create_results_dir("parent")
#' 
#' # Use current filename
#' results_dir <- create_results_dir("filename")
#' 
#' # Use both parent directory and filename
#' results_dir <- create_results_dir("both")
#' }
#'
#' @seealso \code{\link{get_project_root}}, \code{\link{save_and_link}}
#' @export
create_results_dir <- function(directory_naming = "parent") {
  # Validate input
  valid_options <- c("parent", "filename", "both")
  if (!directory_naming %in% valid_options) {
    stop("directory_naming must be one of: ", paste(valid_options, collapse = ", "))
  }
  
  current_file <- knitr::current_input(dir = TRUE)
  project_root <- get_project_root()
  
  if (!is.null(current_file)) {
    # Get parent directory name
    parent_name <- stringr::str_extract(
      current_file,
      "(?<=\\/)[^\\/]+(?=\\/[^\\/]+$)"
    )
    
    # Get filename without extension
    filename_no_ext <- tools::file_path_sans_ext(basename(current_file))
    
    # Create subdirectory name based on naming convention
    subdir <- switch(directory_naming,
      "parent" = parent_name,
      "filename" = filename_no_ext,
      "both" = paste(parent_name, filename_no_ext, sep = "_")
    )
    
    # Handle case where parent_name might be NULL (for files in root)
    if (is.null(subdir) || is.na(subdir) || subdir == "") {
      subdir <- if (directory_naming == "filename") {
        filename_no_ext
      } else {
        "root"
      }
    }
    
    filepath <- file.path(project_root, "results", subdir)
  } else {
    # Fallback if current_input() doesn't work
    filepath <- file.path(project_root, "results", "misc")
  }
  
  dir.create(filepath, showWarnings = FALSE, recursive = TRUE)
  return(filepath)
}

#' Save Data and Create Download Link
#'
#' Saves data to a file in the results directory and generates a markdown download link
#' that can be embedded in Quarto documents.
#'
#' @param data The data object to save. Can be a data.frame, list, or other R object.
#' @param filename Character string. The filename to save the data as, including extension.
#' @param description Character string or NULL. Optional description for the download link.
#'   If NULL, defaults to "Download {filename}".
#' @param directory_naming Character string specifying the naming convention.
#'   Options are "parent" (default), "filename", or "both". 
#'   Passed to \code{\link{create_results_dir}}.
#'
#' @return Invisibly returns the full file path where data was saved
#'
#' @details
#' Supported file formats are determined by file extension:
#' \itemize{
#'   \item .csv: Uses \code{write.csv()} with row.names = FALSE
#'   \item .xlsx: Uses \code{openxlsx::write.xlsx()} (requires openxlsx package)
#'   \item .rds: Uses \code{saveRDS()}
#'   \item .txt: Uses \code{writeLines()} after converting to character
#'   \item .json: Uses \code{jsonlite::write_json()} with pretty = TRUE (requires jsonlite package)
#' }
#' 
#' The function outputs a markdown link using \code{cat()} that can be rendered in Quarto documents.
#' The link path is relative to the project root for portability.
#'
#' @examples
#' \dontrun{
#' # Save a data frame as CSV with custom description (parent directory naming)
#' data <- mtcars[1:10, ]
#' save_and_link(data, "car_data.csv", "🚗 Top 10 Cars Dataset")
#' 
#' # Save as Excel file using filename as subdirectory
#' save_and_link(iris, "iris.xlsx", "🌸 Iris Dataset", directory_naming = "filename")
#' 
#' # Save R object as RDS using both parent and filename
#' model <- lm(mpg ~ wt, data = mtcars)
#' save_and_link(model, "linear_model.rds", "📊 Linear Model Object", directory_naming = "both")
#' }
#'
#' @seealso \code{\link{create_results_dir}}, \code{\link{generate_results_section}}
#' @export
save_and_link <- function(data, filename, description = NULL, directory_naming = "parent") {
  # Create results directory
  results_dir <- create_results_dir(directory_naming = directory_naming)
  project_root <- get_project_root()
  
  # Full file path
  file_path <- file.path(results_dir, filename)
  
  # Save the data based on file extension
  ext <- tools::file_ext(filename)
  
  if (ext == "csv") {
    write.csv(data, file_path, row.names = FALSE)
  } else if (ext == "xlsx") {
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      stop("openxlsx package required for Excel files")
    }
    openxlsx::write.xlsx(data, file_path)
  } else if (ext == "rds") {
    saveRDS(data, file_path)
  } else if (ext == "txt") {
    writeLines(as.character(data), file_path)
  } else if (ext == "json") {
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      stop("jsonlite package required for JSON files")
    }
    jsonlite::write_json(data, file_path, pretty = TRUE)
  }
  
  # Generate relative path from project root
  relative_path <- file.path("results", basename(results_dir), filename)
  link_text <- if (!is.null(description)) description else paste("Download", filename)
  
  # Return markdown link
  cat(paste0("[", link_text, "](", relative_path, ")\n\n"))
  
  return(invisible(file_path))
}

#' Generate Results Section in Quarto Document
#'
#' Automatically generates a formatted results section listing all files in the
#' current results directory, organized by file type with download links.
#'
#' @param directory_naming Character string specifying the naming convention.
#'   Options are "parent" (default), "filename", or "both". 
#'   Passed to \code{\link{create_results_dir}}.
#'
#' @return NULL (invisibly). The function outputs formatted markdown using \code{cat()}.
#'
#' @details
#' The function creates a markdown section with:
#' \itemize{
#'   \item Organized file listing by type (Data Files, Plots, Reports, Models, Other)
#'   \item File metadata including size and modification time
#'   \item Appropriate icons for each file type
#'   \item Download links with clean display names (without extensions)
#'   \item Directory path information
#' }
#' 
#' File type classification:
#' \itemize{
#'   \item Data Files: csv, xlsx, rds, json, parquet
#'   \item Plots: png, pdf, svg, jpg, jpeg, eps, tiff
#'   \item Reports: html, docx, txt, md, rtf
#'   \item Models: rds, pkl, joblib, h5
#'   \item Other: All other file types
#' }
#' 
#' Returns early (invisibly NULL) if no results directory exists or contains no files.
#'
#' @examples
#' \dontrun{
#' # Generate results section using parent directory
#' generate_results_section()
#' 
#' # Generate results section using current filename
#' generate_results_section("filename")
#' 
#' # Generate results section using both parent and filename
#' generate_results_section("both")
#' }
#'
#' @seealso \code{\link{create_results_dir}}, \code{\link{save_and_link}}
#' @export
generate_results_section <- function(directory_naming = "parent") {
  results_dir <- create_results_dir(directory_naming = directory_naming)
  project_root <- get_project_root()
  
  if (!dir.exists(results_dir)) {
    return(invisible(NULL))
  }
  
  files <- list.files(results_dir, full.names = FALSE)
  
  if (length(files) == 0) {
    return(invisible(NULL))
  }
  
  # Get the subdirectory name for the header
  subdir_name <- basename(results_dir)
  
  cat(paste0("\n## 📊 Analysis Results (", subdir_name, ")\n\n"))
  cat("The following files contain detailed results from this analysis:\n\n")
  
  # Group files by type
  file_types <- list(
    "Data Files" = c("csv", "xlsx", "rds", "json", "parquet"),
    "Plots" = c("png", "pdf", "svg", "jpg", "jpeg", "eps", "tiff"),
    "Reports" = c("html", "docx", "txt", "md", "rtf"),
    "Models" = c("rds", "pkl", "joblib", "h5"),
    "Other" = character(0)
  )
  
  files_by_type <- list()
  
  for (file in files) {
    ext <- tools::file_ext(tolower(file))
    assigned <- FALSE
    
    for (type_name in names(file_types)) {
      if (ext %in% file_types[[type_name]]) {
        if (is.null(files_by_type[[type_name]])) {
          files_by_type[[type_name]] <- character(0)
        }
        files_by_type[[type_name]] <- c(files_by_type[[type_name]], file)
        assigned <- TRUE
        break
      }
    }
    
    if (!assigned) {
      if (is.null(files_by_type[["Other"]])) {
        files_by_type[["Other"]] <- character(0)
      }
      files_by_type[["Other"]] <- c(files_by_type[["Other"]], file)
    }
  }
  
  # Generate links by type
  for (type_name in names(files_by_type)) {
    if (length(files_by_type[[type_name]]) > 0) {
      cat(paste0("\n### ", type_name, "\n\n"))
      
      for (file in files_by_type[[type_name]]) {
        # Create relative path from project root
        relative_path <- file.path("results", subdir_name, file)
        full_file_path <- file.path(results_dir, file)
        
        # Get file size and modification time
        if (file.exists(full_file_path)) {
          file_info <- file.info(full_file_path)
          size_kb <- round(file_info$size / 1024, 1)
          mod_time <- format(file_info$mtime, "%Y-%m-%d %H:%M")
          meta_info <- paste0(" (", size_kb, " KB, ", mod_time, ")")
        } else {
          meta_info <- ""
        }
        
        # Generate appropriate icon
        ext <- tools::file_ext(tolower(file))
        icon <- switch(ext,
          "csv" = "📊", "xlsx" = "📋", "rds" = "💾", "json" = "🔧", "parquet" = "📦",
          "png" = "🖼️", "pdf" = "📄", "svg" = "🎨", "jpg" = "🖼️", "jpeg" = "🖼️",
          "html" = "🌐", "txt" = "📝", "md" = "📄", "docx" = "📄",
          "h5" = "🧠", "pkl" = "🤖", "joblib" = "⚙️",
          "📄"
        )
        
        # Clean filename for display (remove extension)
        display_name <- tools::file_path_sans_ext(file)
        
        cat(paste0("- ", icon, " [", display_name, "](", relative_path, ")", meta_info, "\n"))
      }
    }
  }
  
  # Add directory info
  cat(paste0("\n**Results directory:** `", file.path("results", subdir_name), "`\n\n"))
}

#' Get Current Results Directory Path
#'
#' Convenience function to get the path of the current results directory
#' without creating any files or output.
#'
#' @param directory_naming Character string specifying the naming convention.
#'   Options are "parent" (default), "filename", or "both". 
#'   Passed to \code{\link{create_results_dir}}.
#'
#' @return Character string of the results directory path
#'
#' @details
#' This is a simple wrapper around \code{\link{create_results_dir}} that can be
#' useful for getting the directory path for other operations without side effects.
#'
#' @examples
#' \dontrun{
#' # Get current results directory path (parent directory naming)
#' results_path <- get_results_dir()
#' print(results_path)
#' 
#' # Use filename-based naming
#' results_path <- get_results_dir("filename")
#' 
#' # Use both parent and filename
#' results_path <- get_results_dir("both")
#' 
#' # Use in other file operations
#' results_path <- get_results_dir("both")
#' file.copy("external_file.csv", file.path(results_path, "copied_file.csv"))
#' }
#'
#' @seealso \code{\link{create_results_dir}}
#' @export
get_results_dir <- function(directory_naming = "parent") {
  return(create_results_dir(directory_naming = directory_naming))
}

#' Clean Results Directory
#'
#' Removes all files from the current results directory with optional confirmation prompt.
#'
#' @param directory_naming Character string specifying the naming convention.
#'   Options are "parent" (default), "filename", or "both". 
#'   Passed to \code{\link{create_results_dir}}.
#' @param confirm Logical. If TRUE (default), prompts user for confirmation before deleting files.
#'   If FALSE, deletes files without confirmation.
#'
#' @return NULL (invisibly). Prints status messages using \code{cat()}.
#'
#' @details
#' The function:
#' \itemize{
#'   \item Lists all files in the current results directory
#'   \item Shows file count and names (if confirm = TRUE)
#'   \item Prompts for user confirmation (if confirm = TRUE)
#'   \item Deletes all files and reports count of deleted files
#'   \item Returns early with message if directory doesn't exist or is empty
#' }
#' 
#' Use with caution, especially when confirm = FALSE, as this permanently deletes files.
#'
#' @examples
#' \dontrun{
#' # Clean with confirmation prompt (default, parent directory naming)
#' clean_results_dir()
#' 
#' # Clean without confirmation (use carefully!)
#' clean_results_dir(confirm = FALSE)
#' 
#' # Clean results directory based on filename
#' clean_results_dir("filename", confirm = TRUE)
#' 
#' # Clean results directory using both parent and filename
#' clean_results_dir("both", confirm = TRUE)
#' }
#'
#' @seealso \code{\link{create_results_dir}}, \code{\link{get_results_dir}}
#' @export
clean_results_dir <- function(directory_naming = "parent", confirm = TRUE) {
  results_dir <- create_results_dir(directory_naming = directory_naming)
  
  if (!dir.exists(results_dir)) {
    cat("Results directory doesn't exist.\n")
    return(invisible(NULL))
  }
  
  files <- list.files(results_dir, full.names = TRUE)
  
  if (length(files) == 0) {
    cat("Results directory is already empty.\n")
    return(invisible(NULL))
  }
  
  if (confirm) {
    cat(paste0("Found ", length(files), " files in ", results_dir, "\n"))
    cat("Files to delete:\n")
    cat(paste0("- ", basename(files), collapse = "\n"))
    cat("\n")
    
    response <- readline(prompt = "Delete all files? (y/N): ")
    if (tolower(response) != "y") {
      cat("Cancelled.\n")
      return(invisible(NULL))
    }
  }
  
  file.remove(files)
  cat(paste0("Deleted ", length(files), " files from results directory.\n"))
}

#' Copy Files to Results Directory
#'
#' Copies external files to the current results directory and creates download links
#' for each copied file.
#'
#' @param source_files Character vector of source file paths to copy.
#' @param directory_naming Character string specifying the naming convention.
#'   Options are "parent" (default), "filename", or "both". 
#'   Passed to \code{\link{create_results_dir}}.
#' @param descriptions Character vector or NULL. Optional descriptions for download links.
#'   Should be same length as source_files. If NULL or shorter than source_files,
#'   defaults to filename without extension with 📄 icon.
#'
#' @return NULL (invisibly). Outputs markdown links using \code{cat()}.
#'
#' @details
#' For each source file that exists:
#' \itemize{
#'   \item Copies file to results directory using \code{file.copy()} with overwrite = TRUE
#'   \item Creates a markdown download link with relative path from project root
#'   \item Uses provided description or generates default from filename
#' }
#' 
#' Files that don't exist are silently skipped. The basename of source files is used
#' as the destination filename (directory structure is flattened).
#'
#' @examples
#' \dontrun{
#' # Copy single file (parent directory naming)
#' copy_to_results("data/raw_data.csv")
#' 
#' # Copy multiple files with descriptions using filename naming
#' files <- c("plots/figure1.png", "models/trained_model.rds")
#' descriptions <- c("📊 Main Analysis Plot", "🤖 Trained ML Model")
#' copy_to_results(files, directory_naming = "filename", descriptions = descriptions)
#' 
#' # Copy to combined parent-filename subdirectory
#' copy_to_results("external_report.pdf", directory_naming = "both")
#' }
#'
#' @seealso \code{\link{create_results_dir}}, \code{\link{save_and_link}}
#' @export
copy_to_results <- function(source_files, directory_naming = "parent", descriptions = NULL) {
  results_dir <- create_results_dir(directory_naming = directory_naming)
  project_root <- get_project_root()
  subdir_name <- basename(results_dir)
  
  for (i in seq_along(source_files)) {
    source_file <- source_files[i]
    if (file.exists(source_file)) {
      dest_file <- file.path(results_dir, basename(source_file))
      file.copy(source_file, dest_file, overwrite = TRUE)
      
      # Create link
      relative_path <- file.path("results", subdir_name, basename(source_file))
      description <- if (!is.null(descriptions) && length(descriptions) >= i) {
        descriptions[i]
      } else {
        paste("📄", tools::file_path_sans_ext(basename(source_file)))
      }
      
      cat(paste0("[", description, "](", relative_path, ")\n\n"))
    }
  }
}

#' List All Results Directories
#'
#' Displays a summary of all results subdirectories in the project with file counts.
#'
#' @return NULL (invisibly). Outputs formatted markdown using \code{cat()}.
#'
#' @details
#' The function:
#' \itemize{
#'   \item Searches for results directory in project root
#'   \item Lists all subdirectories within results/
#'   \item Shows file count for each subdirectory
#'   \item Outputs formatted markdown list
#' }
#' 
#' Returns early with message if no results directory exists or contains no subdirectories.
#' Only shows immediate subdirectories (not recursive).
#'
#' @examples
#' \dontrun{
#' # List all results directories
#' list_all_results()
#' }
#'
#' @seealso \code{\link{get_project_root}}, \code{\link{create_results_dir}}
#' @export
list_all_results <- function() {
  project_root <- get_project_root()
  results_root <- file.path(project_root, "results")
  
  if (!dir.exists(results_root)) {
    cat("No results directory found.\n")
    return(invisible(NULL))
  }
  
  subdirs <- list.dirs(results_root, full.names = FALSE, recursive = FALSE)
  
  if (length(subdirs) == 0) {
    cat("No result subdirectories found.\n")
    return(invisible(NULL))
  }
  
  cat("## All Results Directories\n\n")
  for (subdir in subdirs) {
    subdir_path <- file.path(results_root, subdir)
    file_count <- length(list.files(subdir_path))
    cat(paste0("- **", subdir, "/** (", file_count, " files)\n"))
  }
  cat("\n")
}