#' Get the top GO processes in a cluster
#'
#' This function find the top GO processes identified in a cluster, based on
#' the Benjamini threshold given.
#'
#' @param combined_list A list of data frames (preferably the one produced by
#' combine_david_files function), where each data frame is a cluster, in
#' chronological order. This assumes the first data frame is cluster 0, the
#' second data frame is cluster 1, and so on.
#' @param cluster An integer that represents the cluster to be analyzed. For
#' example, 0 for cluster 0, 1 for cluster 1, and so on.
#' @param benjamini A number that represents the Benjamini threshold for
#' filtering processes. Only processes with a Benjamini value smaller than the
#' threshold will be kept. The default value is 0.05.
#' @param top_n The number of processes to return.
#' @return A data frame with the top_n top processes.
#'
#' @export
get_top_processes <- function(combined_list, cluster, benjamini = 0.05, top_n) {
  curr_df <- combined_list[[cluster + 1]]

  # sort data frame from smallest to largest PValue
  sorted_df <- dplyr::arrange(curr_df, "PValue")

  # keep rows that meet Benjamini threshold
  filtered_df <- sorted_df[sorted_df$Benjamini < benjamini, ]
  top_rows <- head(filtered_df, top_n)
  return(top_rows)
}
# [END]
