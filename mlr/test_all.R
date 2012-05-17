library(methods)
library(devtools)
library(testthat)

if (interactive()) {
  load_all("skel")
} else {
  library(mlr)  
}
source("skel/inst/tests/helpers.R")
source("skel/inst/tests/objects.R")
test_dir("skel/inst/tests")

