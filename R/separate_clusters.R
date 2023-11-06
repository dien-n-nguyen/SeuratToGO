#' Separate genes based on cluster
#'
#' This function separates the genes based on their cluster, and labels the
#' the column name as the cluster number. It also exports the data frame as a
#' tab-delimited text file so it can be uploaded onto the DAVID Website.
#'
#' @param x A data frame of gene markers with columns "cluster" and "gene".
#' @return A data frame of gene markets with cluster number as column name.
#'
#' @export
separate_clusters <- function(markers) {
  markers_df <- data.frame(markers)
  markers_df <- markers_df[, c("cluster", "gene")]
  all_cluster_numbers <- unique(df$cluster)
  clusters <- list()
  column_names <- c()
  for (i in 1:length(all_cluster_numbers)) {
    genes <- df %>% dplyr::filter(cluster == (i - 1)) %>% select(gene)
    column_name <- paste("Cluster", as.character(i - 1))
    column_names <- append(column_names, column_name)
    clusters[[i]] <- genes$gene
  }
  cluster_eq_len <- list()
  max_cluster <- max(lengths(clusters))
  for (i in 1:length(clusters)) {
    na_to_add <- max_cluster - length(clusters[[i]])
    cluster_with_na <- c(clusters[[i]], rep(NA, na_to_add))
    cluster_eq_len[[i]] <- cluster_with_na
  }
  all_df <- data.frame(cluster_eq_len)
  names(all_df) <- column_names
  current_dir <- getwd()
  file_path <- file.path(current_dir, "genes_list.txt")
  write.table(all_df, file = file_path, sep = "\t", row.names = FALSE,
              quote = FALSE)
  return(all_df)
}
# [END]
