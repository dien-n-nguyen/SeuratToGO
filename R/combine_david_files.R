#' Combine text files produced by DAVID analysis
#'
#' This function reads and combines text files produced by DAVID's annotation
#' chart into a list of data frames, one data frame for each file (each cluster)
#'
#' @param path_to_folder A string that is the path to the folder that contains
#' all the DAVID text files. The folder must only contain DAVID text files that
#' the user wants to analyze. The files must be names such that they are in
#' chronological order of their cluster number.
#' @return A list of data frames, one data frame for each cluster
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(SeuratToGO)
#' david_file_path = system.file("extdata", "david", package = "SeuratToGO")
#' # this is where DAVID output files are stored in this package
#' # when using your own files, you must provide the file path to the folder
#' # where you saved them
#' combined_list <- combine_david_files(david_file_path)
#' View(combined_list)
#' # see the vignette for more details
#' }
#'
#' @references
#' Sherman BT, Hao M, Qiu J, Jiao X, Baseler MW, Lane HC, Imamichi T, Chang W.
#' 2022. DAVID: a web server for functional enrichment analysis and functional
#' annotation of gene lists (2021 update). \emph{Nucleic Acids Res}.
#' 50(W1):W216–W221. \href{https://pubmed.ncbi.nlm.nih.gov/35325185/}{Link}

combine_david_files <- function(path_to_folder) {

  # account for users forgetting to add the forward slash to path
  last_char <- substr(path_to_folder, nchar(path_to_folder), nchar(path_to_folder))
  if (last_char != "/") {
    path_to_folder <- paste0(path_to_folder, "/")
  }

  file_list <- list.files(path = path_to_folder)
  david_list <- list()
  for (i in 1:length(file_list)){
    file_path <- paste0(path_to_folder, file_list[[i]])
    curr_df <- read.table(file_path, header = TRUE, sep = "\t")

    # check that data frame has correct columns
    desired_columns = c("Category", "Term", "Count", "X.", "PValue",
                        "Genes", "List.Total", "Pop.Hits", "Pop.Total",
                        "Fold.Enrichment", "Bonferroni", "Benjamini",
                        "FDR")
    if (!all(desired_columns %in% colnames(curr_df))) {
      stop("Error: Check that the DAVID file has these columns: Category, Term,
           Count, X., PValue, Genes, List.Total, Pop.Hits, Pop.Total,
           Fold.Enrichment, Bonferroni, Benjamini, FDR.")
    }

    david_list[[i]] <- curr_df
  }
  return(david_list)
}
# [END]
