library(methods)
library(testthat)
library(reshape)
library(e1071)
library(adabag)

if (interactive()) {
  library(devtools)
  library(adabag)
  library(boot)
  library(MASS)
  library(ROCR)
  library(ParamHelpers)
  load_all("skel")
} else {
  library(mlr)  
}
source("skel/inst/tests/helpers.R")
source("skel/inst/tests/objects.R")
options(mlr.debug.seed=123L)
configureMlr(show.learner.output=FALSE)
test_dir("skel/inst/tests")
