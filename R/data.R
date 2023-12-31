#' Gene markers from analysis of Peripheral Blood Mononuclear Cells (PBMC)
#' using Seurat
#'
#' 2,700 single cells were sequenced on the Illumina NextSeq 500 and analysed
#' using an R package called Seurat.
#'
#' @source Seurat Tutorial by Satija Lab. The raw data is provided here:
#' \url{https://satijalab.org/seurat/articles/pbmc3k_tutorial.html}.
#' This data set was produced by the FindAllMarkers() function in the tutorial.
#'
#' @format A data frame with columns:
#' \describe{
#'  \item{p_val}{P-value that indicates significance of this gene's expression.}
#'  \item{avg_log2FC}{Average log2 fold change that indicates average change in
#'  gene expression levels of this gene in different clusters}
#'  \item{pct.1}{Percentage of cells in cluster that expresses this gene}
#'  \item{pct.2}{Percentage of other cells in cluster that expresses this gene}
#'  \item{p_val_adj}{Adjust p-value}
#'  \item{cluster}{The cluster that expresses this gene}
#'  \item{gene}{The gene}
#'  }
#'  @examples
#'  pbmc_markers
"pbmc_markers"

