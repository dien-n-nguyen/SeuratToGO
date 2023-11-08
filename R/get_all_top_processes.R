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
#' @param top_n The number of processes to identify for each cluster.
#' @return A data frame with the top_n top processes for each cluster, combined.
#'
#' @export
get_all_top_processes <- function(combined_list, benjamini, top_n) {
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
      top_processes_df[x["Term"], column_names[i]] <<- x["PValue"]
    })
  }
  return(top_processes_df)
}
# [END]