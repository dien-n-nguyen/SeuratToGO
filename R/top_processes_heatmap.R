#' Produce a heatmap of the top processes
#'
#' This function creates and returns a heatmap showing the top processes in each
#' cluster. It automatically saves the plot as a .png file in the current
#' working directory.
#'
#' @param top_processes_df A data frame containing the top processes of each
#' cluster. The rownames should be the processes and the column names should
#' be the cluster. It can be produced using the get_all_top_processes function
#' in the package.
#' @param width The width of the output file
#' @param height The height of the output file
#' @param cellwidth The cell width for each cell in the heatmap.
#' @return A heatmap that represents the top processes in each cluster.
#'
#' @export
#' @import pheatmap
#'
#' @examples
#' \dontrun{
#' library(SeuratToGO)
#' combined_list <- combine_david_files("./david")
#' top_df <- get_all_top_processes(combined_list, benjamini = 0.05, top_n = 5)
#' top_processes_heatmap(top_df)
#' }

top_processes_heatmap <- function(top_processes_df, width = 12, height = 6,
                                  cellwidth = 30) {
  # check that cellwidth is > 0
  if (cellwidth <= 0) {
    stop("Cellwidth provided must be greater than 0.")
  }

  # check that width and height are both > 0
  if (width < 0 || height < 0) {
    stop("Both width and height must be greater than 0.")
  }

  # check that row names are GO processes
  if (!all(grepl("^GO:", rownames(top_processes_df)))) {
    stop("Each rowname must be a GO process, for example, GO:0005829~cytosol.")
  }

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
                           cellwidth = cellwidth,
                           main = "Heatmap of top processes of each cluster",
                           filename = "./top_process_heatmap.png",
                           width = width,
                           height = height)
  return(heatmap_plot)
}
# [END]
