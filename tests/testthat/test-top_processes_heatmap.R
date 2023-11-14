library(SeuratToGO)
library(testthat)

test_that("top_processes_heatmap produces correct output", {
  david_combined <- combine_david_files("./test_david/normal_david/")
  top_df <- get_all_top_processes(david_combined, 0.05, 5)
  plot_output <- top_processes_heatmap(top_df)

  current_dir <- getwd()
  output_file <- file.path(current_dir, "top_process_heatmap.png")

  expect_length(top_df, 2)
  expect_true(file.exists(output_file))
})

test_that("top_processes_heatmap errors upon invalid user input", {
  david_combined <- combine_david_files("./test_david/normal_david/")
  top_df <- get_all_top_processes(david_combined, 0.05, 5)
  top_df_wrong <- top_df
  rownames(top_df_wrong)[3] <- "invalid row name"

  # check that invalid row name causes an error
  expect_error(top_processes_heatmap(top_df_wrong))

  # check that invalid cellwidth causes an error
  expect_error(top_processes_heatmap(top_df, cellwidth = -1))

  # check that invalid height or width causes an error
  expect_error(top_processes_heatmap(top_df, width = -6))
  expect_error(top_processes_heatmap(top_df, height = -6))
})


