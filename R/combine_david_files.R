#' Combine text files produced by DAVID analysis
#'
#' This function reads and combines text files produced by DAVID's annotation
#' chart into a list of data frames, one data frame for each file (each cluster)
#'
#' @param path_to_folder A string that is the path to the folder that contains
#' all the DAVID text files. The folder must only contain DAVID text files that
#' the user wants to analyze. The files must be names such that they are in
#' chronological order of their cluster number.
#' @return A list of data frames, one data frame for each cluster
#'
#' @export
combine_david_files <- function(path_to_folder) {
  file_list <- list.files(path = path_to_folder)
  david_list <- list()
  for (i in 1:length(file_list)){
    file_path <- paste0(path_to_folder, file_list[[i]])
    print(file_path)
    curr_df <- read.table(file_path, header = TRUE, sep = "\t")
    david_list[[i]] <- curr_df
  }
  return(david_list)
}
# [END]