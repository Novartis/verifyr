test_that("errors", {
  expect_error(
    initial_comparison(old = paste0(system.file("/extdata/base_files/",    "15-1.01.rtf", package = "verifyr")),
                       new = paste0(system.file("/extdata/compare_files/", "15-1.01.rtf", package = "verifyr"))),
    "Neither of the files exist"

  )
})

test_that("comparisons", {
  expect_equal(
    initial_comparison(old = paste0(system.file("/extdata/base_files/",    "14-1.03.rtf", package = "verifyr")),
                       new = paste0(system.file("/extdata/compare_files/", "14-1.03.rtf", package = "verifyr"))),
    "Unable to compare"
  )

  expect_equal(
    initial_comparison(old = paste0(fs::path_package("/extdata/base_files/",    "14-1.01.rtf", package = "verifyr")),
                       new = paste0(fs::path_package("/extdata/compare_files/", "14-1.01.rtf", package = "verifyr"))),
    "Output has changes in 3 places"
  )

  expect_equal(
    initial_comparison(old = paste0(fs::path_package("/extdata/base_files/",    "14-2.01.rtf", package = "verifyr")),
                       new = paste0(fs::path_package("/extdata/compare_files/", "14-2.01.rtf", package = "verifyr"))),
    "Only one row has changed"
  )
})

test_that("Returns 'No changes' for identical files", {
  old_file <- testthat::test_path("test_outputs/old", "test010_old.rtf")
  new_file <- testthat::test_path("test_outputs/new", "test01a_new.rtf")

  result <- initial_comparison(old_file, new_file)
  expect_equal(result, "No changes")
})

test_that("Returns 'No changes' for identical files", {
  old_file <- testthat::test_path("test_outputs/old", "test010_old.rtf")
  new_file <- testthat::test_path("test_outputs/new", "test01a_new.rtf")

  result <- initial_comparison(old_file, new_file)
  expect_equal(result, "No changes")
})

test_that("Returns 'Output size has changed' for files with different number of lines", {
  old_file <- testthat::test_path("test_outputs/old", "test010_old.rtf")
  new_file <- testthat::test_path("test_outputs/new", "test01b_new.rtf")

  result <- initial_comparison(old_file, new_file)
  expect_equal(result, "Output size has changes")
})

test_that("Returns 'Only one row has changed' for files with a single different row", {
  old_file <- testthat::test_path("test_outputs/old", "test010_old.rtf")
  new_file <- testthat::test_path("test_outputs/new", "test01c_new.rtf")

  result <- initial_comparison(old_file, new_file)
  expect_equal(result, "Only one row has changed")
})

test_that("Returns 'Output has changes in 2 places' for files with multiple different rows", {
  old_file <- testthat::test_path("test_outputs/old", "test010_old.rtf")
  new_file <- testthat::test_path("test_outputs/new", "test01d_new.rtf")

  result <- initial_comparison(old_file, new_file)
  expect_equal(result, "Output has changes in 2 places")
})

test_that("Returns 'Unable to compare' if one file does not exist", {
  old_file <- testthat::test_path("test_outputs/old", "test010_old.rtf")
  new_file <- "non_existent.rtf"


  result <- initial_comparison(old_file, new_file)
  expect_equal(result, "Unable to compare")
})

test_that("Comparison with 'omit' parameter that is not present in either file Returns 'No changes'", {
  old_file <- testthat::test_path("test_outputs/old", "test010_old.rtf")
  new_file <- testthat::test_path("test_outputs/new", "test01a_new.rtf")

  # Define the 'omit' parameter
  omit_rows <- "Nothing"

  # Perform the comparison
  result <- initial_comparison(old_file, new_file, omit = omit_rows)

  # Expect the output to be 'No changes'
  expect_equal(result, "No changes")
})

test_that("Comparison with 'omit' parameter Returns 'No changes'", {
  old_file <- testthat::test_path("test_outputs/old", "test010_old.rtf")
  new_file <- testthat::test_path("test_outputs/new", "test01e_new.rtf")

  # Define the 'omit' parameter
  omit_rows <- "Footnote"

  # Perform the comparison
  result <- initial_comparison(old_file, new_file, omit = omit_rows)

  # Expect the output to be 'No changes'
  expect_equal(result, "No changes")
})

test_that("Comparison with 'omit' parameter Returns 'Only one row has changed'", {
  old_file <- testthat::test_path("test_outputs/old", "test010_old.rtf")
  new_file <- testthat::test_path("test_outputs/new", "test01f_new.rtf")

  # Define the 'omit' parameter
  omit_rows <- "Title"

  # Perform the comparison
  result <- initial_comparison(old_file, new_file, omit = omit_rows)

  # Expect the output to be 'Only one row has changed'
  expect_equal(result, "Only one row has changed")
})


test_that("Function stops with an error when both 'old' and 'new' files do not exist", {
  old_file <- "non_existent_old_file.txt"
  new_file <- "non_existent_new_file.txt"

  expect_error(initial_comparison(old_file, new_file), "Neither of the files exist")
})
