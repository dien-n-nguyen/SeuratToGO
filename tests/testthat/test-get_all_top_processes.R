library(SeuratToGO)
library(testthat)

test_that("get_all_top_processes produces the correct output", {
  david_combined <- SeuratToGO::combine_david_files(
    "./test_david/normal_david/")
  test_results <- get_all_top_processes(david_combined, 0.05, 2)

  expected_rownames <- c("GO:0002181~cytoplasmic translation",
                         "GO:0022626~cytosolic ribosome",
                         "GO:0070062~extracellular exosome",
                         "GO:0005515~protein binding")
  expected_colnames <- c("Cluster 0", "Cluster 1")
  expected_c0 <- c(3.061464e-131, 4.817482e-120, NA, NA)
  expected_c1 <- c(NA, NA, 1.250497e-38, 9.284760e-23)

  expect_length(test_results, 2)
  expect_equal(rownames(test_results), expected_rownames)
  expect_equal(colnames(test_results), expected_colnames)
  expect_equal(test_results$`Cluster 0`, expected_c0)
  expect_equal(test_results$`Cluster 1`, expected_c1)
})

test_that("get_all_top_processes errors upon invalid user input", {
  david_combined <- SeuratToGO::combine_david_files(
    "./test_david/normal_david/")

  # benjamini is not a positive number
  expect_error(test_results <- get_all_top_processes(david_combined, -0.05,
                                                     1))

  # top_n is not a positive integer
  expect_error(test_results <- get_all_top_processes(david_combined, 0.05, -1))
})
