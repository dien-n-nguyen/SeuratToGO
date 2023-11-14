library(SeuratToGO)
library(testthat)

test_that("combine_david_files produces correct list of dataframes", {
  test_results <- SeuratToGO::combine_david_files("./test_david/normal_david/")
  expected_colnames <- c("Category", "Term", "Count", "X.", "PValue",
                         "Genes", "List.Total", "Pop.Hits", "Pop.Total",
                         "Fold.Enrichment", "Bonferroni", "Benjamini",
                         "FDR")
  expect_length(test_results, 2)
  expect_equal(colnames(test_results[[1]]), expected_colnames)
})

test_that("combine_david_files error upon missing columns", {
  expect_error(test_results <-
                 combine_david_files("./test_david/missing_columns_david/"))

})
