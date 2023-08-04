test_that("Returns tibble with matching files when both folders and pattern exist", {
  old_folder <- testthat::test_path("test_outputs/old")
  new_folder <- testthat::test_path("test_outputs/new")

  # Perform the listing
  result <- list_files(old_folder, new_folder, pattern = "test03")

  # Expect the output to be a tibble with 3 columns
  expect_s3_class(result, "tbl_df")
  expect_equal(ncol(result), 3)
  expect_equal(names(result), c("file", "old_path", "new_path"))

  # Expect the output to contain all the files
  expect_equal(result$file, c("test03a.rtf", "test03b.rtf", "test03c.rtf", "test03d.rtf", "test03e.rtf"))

  # Expect the output to have NA values for old or new path if file is missing
  expect_equal(na.omit(result)$file, c("test03a.rtf", "test03c.rtf", "test03d.rtf"))
})

test_that("Returns empty tibble when pattern does not match any files", {
  old_folder <- testthat::test_path("test_outputs/old")
  new_folder <- testthat::test_path("test_outputs/new")

  # Perform the listing with non-matching pattern
  result <- list_files(old_folder, new_folder, pattern = "nomatch")

  # Expect the output to be an empty tibble
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("Returns 'one or both of the folders do not exist' if either folder does not exist", {
  old_folder <- testthat::test_path("test_outputs/old")
  new_folder <- "non_existent_folder"

  result <- capture_output(list_files(old_folder, new_folder, pattern = "rtf"))

  # Expect the output to contain the error message
  expect_true(grepl("one or both of the folders do not exist", result))
})
