#' Get the p-value for the top processes of all clusters
#'
#' This function finds the top processes identified for each cluster and compile
#' them into one data frame.
#'
#' @param combined_list A list of data frames (preferably the one produced by
#' combine_david_files function), where each data frame is a cluster, in
#' chronological order. This assumes the first data frame is cluster 0, the
#' second data frame is cluster 1, and so on.
#' @param benjamini A number that represents the Benjamini threshold for
#' filtering processes. Only processes with a Benjamini value smaller than the
#' threshold will be kept. The default value is 0.05.
#' @param top_n The number of processes to identify for each cluster. The
#' default value is 5
#' @return A data frame with the top_n top processes for each cluster, combined.
#' The row names are the all the top_n processes, and the column names are the
#' clusters.
#'
#' @export
#'
#'
#' @examples
#' \dontrun{
#' library(SeuratToGO)
#' david_file_path = system.file("extdata", "david", package = "SeuratToGO")
#' # this is where DAVID output files are stored in this package
#' # when using your own files, you must provide the file path to the folder
#' # where you saved them
#' combined_list <- combine_david_files(david_file_path)
#' top_processes <- get_all_top_processes(combined_list)
#' View(top_processes)
#' }
#'
#' @references
#' Sherman BT, Hao M, Qiu J, Jiao X, Baseler MW, Lane HC, Imamichi T, Chang W.
#' 2022. DAVID: a web server for functional enrichment analysis and functional
#' annotation of gene lists (2021 update). \emph{Nucleic Acids Res}.
#' 50(W1):W216–W221. \href{https://pubmed.ncbi.nlm.nih.gov/35325185/}{Link}
#'
#' Benjamini Y, Hochberg Y. 1995. Controlling the False Discovery Rate: A
#' Practical and Powerful Approach to Multiple Testing. \emph{Journal of the
#' Royal Statistical Society: Series B (Methodological).} 57(1):289–300.
#' \href{https://rss.onlinelibrary.wiley.com/doi/10.1111/j.2517-6161.1995.tb02031.x}{Link}
#'
get_all_top_processes <- function(combined_list, benjamini = 0.05, top_n = 5) {
  # check that benjamini is greater than 0
  if (benjamini <= 0) {
    stop("Error: benjamini number provided must be a positive number.")
  }

  # check that top_n is greater than 0
  if (top_n <= 0 || is.numeric(top_n) == FALSE) {
    stop("Error: top_n provided must be a positive integer.")
  }

  all_top_processes <- c()
  num_clusters <- length(combined_list)
  cluster_top_dfs <- list()
  # get the top n processes for each cluster
  for (i in 1:num_clusters) {
    cluster_top <- get_top_processes(combined_list, cluster = i - 1,
                                     benjamini, top_n)
    cluster_top_dfs[[i]] <- cluster_top
    all_top_processes <- append(all_top_processes, cluster_top$Term)
  }
  all_top_processes <- unique(all_top_processes)

  # create blank data frame
  num_rows <- length(all_top_processes)
  top_processes_df <- data.frame(matrix(NA, nrow = num_rows,
                                        ncol = num_clusters))
  # get cluster names to be columns
  column_names <- get_cluster_names(num_clusters)
  colnames(top_processes_df) <- column_names
  rownames(top_processes_df) <- all_top_processes

  # populate blank data frame with p-values of each cluster's processes
  for (i in 1:length(cluster_top_dfs)) {
    curr_df <- cluster_top_dfs[[i]]
    apply(curr_df, 1, function(x) {
      top_processes_df[x["Term"], column_names[i]] <<- as.numeric(x["PValue"])
    })
  }
  return(top_processes_df)
}
# [END]
