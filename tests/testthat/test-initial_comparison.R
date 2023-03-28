testthat::test_that("errors", {
  testthat::expect_error(
    initial_comparison(old = paste0(system.file("/test_outputs/base_files/",    "15-1.01.rtf", package = "verifyr")),
                       new = paste0(system.file("/test_outputs/compare_files/", "15-1.01.rtf", package = "verifyr"))),
    "Neither of the files exist"

  )
})

testthat::test_that("comparisons", {
testthat::expect_equal(
  initial_comparison(old = paste0(system.file("/test_outputs/base_files/",    "14-1.03.rtf", package = "verifyr")),
                     new = paste0(system.file("/test_outputs/compare_files/", "14-1.03.rtf", package = "verifyr"))),
  "Unable to compare"
  )

  testthat::expect_equal(
    initial_comparison(old = paste0(fs::path_package("/test_outputs/base_files/",    "14-1.01.rtf", package = "verifyr")),
                       new = paste0(fs::path_package("/test_outputs/compare_files/", "14-1.01.rtf", package = "verifyr"))),
    "Output has changes in 3 places"
  )

  testthat::expect_equal(
    initial_comparison(old = paste0(fs::path_package("/test_outputs/base_files/",    "14-2.01.rtf", package = "verifyr")),
                       new = paste0(fs::path_package("/test_outputs/compare_files/", "14-2.01.rtf", package = "verifyr"))),
    "Only one row has changed"
  )
})


