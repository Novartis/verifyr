Sys.setenv("R_TESTS" = "")

library(lintr)
library(testthat)
library(verifyr)

test_check("verifyr")
