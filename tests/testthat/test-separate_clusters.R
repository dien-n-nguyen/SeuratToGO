library(SeuratToGO)
library(testthat)

test_that("separate clusters with 3 clusters", {
  test_df <- data.frame(
    p_val = c(1.273332e-143, 6.817653e-143, 4.661810e-141, 8.158412e-138,
              5.177478e-130, 3.244898e-123),
    avg_log2FC = c(0.7298951, 0.6870694, 0.7281575, 0.6196246, 0.6252832,
                   0.7496479),
    pct.1 = c(1.000, 1.000, 0.999, 0.999, 1.000, 0.997),
    pct.2 = c(0.991, 0.995, 0.992, 0.995, 0.994, 0.975),
    cluster = c(0, 0, 1, 1, 2, 2),
    gene = c("RPS12", "RPS6", "RPS27", "RPL32", "RPS14", "RPS25")
  )

  expected_df <- data.frame(
    "Cluster 0" = c("RPS12", "RPS6"),
    "Cluster 1" = c("RPS27", "RPL32"),
    "Cluster 2" = c("RPS14", "RPS25")
  )
  current_dir <- getwd()
  output_file <- file.path(current_dir, "genes_list.txt")
  colnames(expected_df) = c("Cluster 0", "Cluster 1", "Cluster 2")

  test_result <- SeuratToGO::separate_clusters(test_df)


  expect_type(test_result, "list")
  expect_length(test_result, 3)
  expect_equal(test_result, expected_df)
  expect_true(file.exists(output_file), "Output file exists.")
})

test_that("separate clusters with one cluster", {
  test_df <- data.frame(
    p_val = c(1.273332e-143, 6.817653e-143, 4.661810e-141, 8.158412e-138,
              5.177478e-130, 3.244898e-123),
    avg_log2FC = c(0.7298951, 0.6870694, 0.7281575, 0.6196246, 0.6252832,
                   0.7496479),
    pct.1 = c(1.000, 1.000, 0.999, 0.999, 1.000, 0.997),
    pct.2 = c(0.991, 0.995, 0.992, 0.995, 0.994, 0.975),
    cluster = c(0, 0, 0, 0, 0, 0),
    gene = c("RPS12", "RPS6", "RPS27", "RPL32", "RPS14", "RPS25")
  )

  expected_df <- data.frame(
    "Cluster 0" = c("RPS12", "RPS6", "RPS27", "RPL32", "RPS14", "RPS25")
  )
  colnames(expected_df) = c("Cluster 0")
  current_dir <- getwd()
  output_file <- file.path(current_dir, "genes_list.txt")

  test_result <- SeuratToGO::separate_clusters(test_df)


  expect_type(test_result, "list")
  expect_length(test_result, 1)
  expect_equal(test_result, expected_df)
  expect_true(file.exists(output_file), "Output file exists.")

})

test_that("separate_clusters error upon invalid user input", {

  # missing "gene" column
  test_df <- data.frame(
    p_val = c(1.273332e-143, 6.817653e-143, 4.661810e-141, 8.158412e-138,
              5.177478e-130, 3.244898e-123),
    avg_log2FC = c(0.7298951, 0.6870694, 0.7281575, 0.6196246, 0.6252832,
                   0.7496479),
    pct.1 = c(1.000, 1.000, 0.999, 0.999, 1.000, 0.997),
    pct.2 = c(0.991, 0.995, 0.992, 0.995, 0.994, 0.975),
    cluster = c(0, 0, 1, 1, 2, 2)
  )
  expect_error(test_result <- separate_clusters(test_df))


  # missing "cluster" column
  test_df <- data.frame(
    p_val = c(1.273332e-143, 6.817653e-143, 4.661810e-141, 8.158412e-138,
              5.177478e-130, 3.244898e-123),
    avg_log2FC = c(0.7298951, 0.6870694, 0.7281575, 0.6196246, 0.6252832,
                   0.7496479),
    pct.1 = c(1.000, 1.000, 0.999, 0.999, 1.000, 0.997),
    pct.2 = c(0.991, 0.995, 0.992, 0.995, 0.994, 0.975),
    gene = c("RPS12", "RPS6", "RPS27", "RPL32", "RPS14", "RPS25")
  )
  expect_error(test_result <- separate_clusters(test_df))

  # clusters are not sequential
  test_df <- data.frame(
    p_val = c(1.273332e-143, 6.817653e-143, 4.661810e-141, 8.158412e-138,
              5.177478e-130, 3.244898e-123),
    avg_log2FC = c(0.7298951, 0.6870694, 0.7281575, 0.6196246, 0.6252832,
                   0.7496479),
    pct.1 = c(1.000, 1.000, 0.999, 0.999, 1.000, 0.997),
    pct.2 = c(0.991, 0.995, 0.992, 0.995, 0.994, 0.975),
    cluster = c(0, 0, 0, 0, 2, 2),
    gene = c("RPS12", "RPS6", "RPS27", "RPL32", "RPS14", "RPS25")
  )
  expect_error(test_result <- separate_clusters(test_df))
})
