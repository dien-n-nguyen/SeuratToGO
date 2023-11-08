#' Produce a heatmap of the top processes
#'
#' This function creates and returns a heatmap showing the top processes in each
#' cluster.
#'
#' @param top_processes_df A data frame containing the top processes of each
#' cluster. The rownames should be the processes and the column names should
#' be the cluster. It can be produced using the get_all_top_processes function
#' in the package.
#' @param cellwidth The cell width for each cell in the heatmap.
#' @return A heatmap that represents the top processes in each cluster.
#'
#' @export
top_processes_heatmap <- function(top_processes_df, cellwidth = 30) {
  # pheatmap function doesn't take NA values, so convert all NAs to 0
  top_processes_df[is.na(top_processes_df)] <- 0
  top_processes_df[] <- lapply(top_processes_df, as.numeric)

  # apply function to make variations more distinguishable in heat map
  log2_df <- apply(top_processes_df, 2, log2)
  log2_df[log2_df == -Inf] <- 0
  my_palette <- colorRampPalette(c("blue", "white"))(100)

  heatmap_plot <- pheatmap::pheatmap(log2_df,
                           color = my_palette,
                           cluster_rows = F,
                           cluster_cols = F,
                           border_color = "black",
                           cellwidth = cellwidth)
  return(heatmap_plot)
}
# [END]
