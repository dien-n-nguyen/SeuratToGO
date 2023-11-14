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
#' @return A data frame of gene markers with cluster number as column name.
#'
#'
#' @references
#' Stuart T, Butler A, Hoffman P, Hafemeister C, Papalexi E, Mauck WM, Hao Y,
#' Stoeckius M, Smibert P, Satija R. 2019. Comprehensive Integration of
#' Single-Cell Data. \emph{Cell}. 177(7):1888-1902.e21.
#' \href{"https://pubmed.ncbi.nlm.nih.gov/31178118/"}{Link}
#'
#' Sherman BT, Hao M, Qiu J, Jiao X, Baseler MW, Lane HC, Imamichi T, Chang W.
#' 2022. DAVID: a web server for functional enrichment analysis and functional
#' annotation of gene lists (2021 update). \emph{Nucleic Acids Res}.
#' 50(W1):W216–W221. \href{"https://pubmed.ncbi.nlm.nih.gov/35325185/"}{Link}
#'
#'
#' @examples
#' \dontrun{
#' library(SeuratToGO)
#' separate_clusters(pbmc_markers)
#' }
#'
#' @export
#' @import magrittr
#' @import dplyr


separate_clusters <- function(markers) {
  markers_df <- data.frame(markers)

  # check that columns "cluster" and "gene" exist
  desired_columns = c("cluster", "gene")
  if (all(desired_columns %in% colnames(markers_df))) {
    print("All needed columns exist.")
  } else {
    stop("Error: Check that the data frame provided has cluster and gene
         columns.")
  }

  markers_df <- markers_df[, c("cluster", "gene")]

  # get cluster numbers
  all_cluster_numbers <- unique(markers_df$cluster)
  num_clusters <- length(all_cluster_numbers)

  # check that all clusters from 0 to num_clusters exist
  if (all(all_cluster_numbers == 0:(num_clusters - 1)) == FALSE) {
    stop("Error: All clusters from 0 to n - 1 must exist.")
  }

  clusters <- list()

  # get vector of column names, e.g. c("Cluster 0", "Cluster 1", etc...)
  column_names <- get_cluster_names(num_clusters)

  # extract the genes in each cluster and store them in the list
  for (i in 1:length(all_cluster_numbers)) {
    genes <- markers_df %>% dplyr::filter(cluster == (i - 1))%>%
      dplyr::select(gene)
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
