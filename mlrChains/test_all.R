library(methods)
library(testthat)
library(reshape)
library(e1071)

if (interactive()) {
  library(devtools)
  library(adabag)
  library(MASS)
  library(ROCR)
  load_all("skel")
} else {
  library(mlr)  
}
#source("skel/inst/tests/helpers.R")
source("skel/inst/tests/objects.R")
test_dir("skel/inst/tests")
