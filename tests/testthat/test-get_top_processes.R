library(SeuratToGO)
library(testthat)

test_that("get_top_processes produces the correct output", {
  david_combined <- combine_david_files("./test_david/normal_david/")
  test_results <- get_top_processes(david_combined, 1, 0.05, 2)

  expected_terms <- c("GO:0070062~extracellular exosome",
                      "GO:0005515~protein binding")
  expected_PValues <- c(1.250497e-38, 9.284760e-23)

  expect_length(test_results, 13)
  expect_true(nrow(test_results) == 2)
  expect_equal(test_results$Term, expected_terms)
  expect_equal(test_results$PValue, expected_PValues)
  expect_true(all(test_results$Benjamini < 0.05))
})

test_that("get_top_processes errors upon inccorect user input", {
  david_combined <- combine_david_files("./test_david/normal_david/")

  # cluster number is greater than actual number of clusters
  expect_error(test_results <- get_top_processes(david_combined, 3, 0.05, 1))

  # cluster number is negative
  expect_error(test_results <- get_top_processes(david_combined, -1, 0.05, 1))

  # benjamini is not a positive number
  expect_error(test_results <- get_top_processes(david_combined, 1, -0.05, 1))

  # top_n is not a positive integer
  expect_error(test_results <- get_top_processes(david_combined, 1, 0.05, -1))
})

