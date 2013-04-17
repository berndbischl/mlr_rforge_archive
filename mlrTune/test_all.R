library(methods)
library(testthat)
library(e1071)
library(reshape)
library(mlr)

configureMlr(show.learner.output=FALSE)

if (interactive()) {
  library(devtools)
  load_all("skel")
} else {
  library(mlrTune)  
}

source("skel/inst/tests/helpers.R")
source("skel/inst/tests/objects.R")
test_dir("skel/inst/tests")
