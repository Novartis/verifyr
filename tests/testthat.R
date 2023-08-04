Sys.setenv("R_TESTS" = "")

library(testthat)
library(verifyr)

test_check("verifyr")
