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
