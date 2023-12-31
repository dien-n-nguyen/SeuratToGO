% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/separate_clusters.R
\name{get_cluster_names}
\alias{get_cluster_names}
\title{Produce a vector of cluster names}
\usage{
get_cluster_names(num_clusters)
}
\arguments{
\item{num_clusters}{The number of clusters to generate names for}
}
\value{
A vector of length num_clusters, which contain all the cluster names
}
\description{
This function creates a vector that contains cluster names (str), based on
the number of cluster given. Each cluster name has the format: Cluster x,
where x is the cluster number.
}
