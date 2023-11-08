#' Produce a vector of cluster names
#'
#' This function creates a vector that contains cluster names (str), based on
#' the number of cluster given. Each cluster name has the format: Cluster x,
#' where x is the cluster number.
#'
#' @param num_clusters The number of clusters to generate names for
#' @return A vector of length num_clusters, which contain all the cluster names
get_cluster_names <- function(num_clusters) {
  cluster_names <- c()
  for (i in 1:num_clusters){
    cluster_name <- paste("Cluster", as.character(i - 1))
    cluster_names <- append(cluster_names, cluster_name)
  }
  return(cluster_names)
}


#' Separate genes based on cluster
#'
#' This function separates the genes based on their cluster, and labels the
#' the column name as the cluster number. It also exports the data frame as a
#' tab-delimited text file so it can be uploaded onto the DAVID Website.
#'
#' @param markers A data frame of gene markers with columns "cluster" and "gene".
#' @return A data frame of gene markets with cluster number as column name.
#'
#' @export
separate_clusters <- function(markers) {
  markers_df <- data.frame(markers)
  markers_df <- markers_df[, c("cluster", "gene")]

  # get cluster numbers
  all_cluster_numbers <- unique(markers_df$cluster)
  num_clusters <- length(all_cluster_numbers)
  clusters <- list()

  # get vector of column names, e.g. c("Cluster 0", "Cluster 1", etc...)
  column_names <- get_cluster_names(num_clusters)

  # extract the genes in each cluster and store them in the list
  for (i in 1:length(all_cluster_numbers)) {
    genes <- markers_df %>% dplyr::filter(cluster == (i - 1)) %>% dplyr::select(gene)
    clusters[[i]] <- genes$gene
  }
  cluster_eq_len <- list()
  max_cluster <- max(lengths(clusters))

  # create vectors of equal length for each cluster, in order to combine them
  # into one data set (each column must have the same number of rows)
  for (i in 1:length(clusters)) {
    na_to_add <- max_cluster - length(clusters[[i]])
    cluster_with_na <- c(clusters[[i]], rep(NA, na_to_add))
    cluster_eq_len[[i]] <- cluster_with_na
  }
  all_df <- data.frame(cluster_eq_len)
  names(all_df) <- column_names

  # write to a tab-delimited text file
  current_dir <- getwd()
  file_path <- file.path(current_dir, "genes_list.txt")
  write.table(all_df, file = file_path, sep = "\t", row.names = FALSE,
              quote = FALSE)
  return(all_df)
}
# [END]
