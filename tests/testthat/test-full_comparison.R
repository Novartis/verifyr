
test_that("Returns comparison when both files exist", {
  old_file <- paste0(fs::path_package("/test_outputs/unit_tests/old/","test020_old.rtf", package = "verifyr"))
  new_file <- paste0(fs::path_package("/test_outputs/unit_tests/new/","test02b_new.rtf", package = "verifyr"))

  # Perform the comparison
  result <- full_comparison(old_file, new_file)

  # Expect the output to contain the Diff comparison as S4 object
  expect_equal(typeof(result), "S4")
})

test_that("Returns 'one or both of the files do not exist' if 'old' file does not exist", {
  old_file <- "non_existent_old_file.rtf"
  new_file <-  paste0(fs::path_package("/test_outputs/unit_tests/new/","test02a_new.rtf", package = "verifyr"))

  result <- capture_output(full_comparison(old_file, new_file))

  # Expect the output to contain the error message
  expect_true(grepl("one or both of the files do not exist", result))
})
