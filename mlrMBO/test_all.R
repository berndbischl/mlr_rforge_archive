library(methods)
library(testthat)
library(mlr)

if (interactive()) {
  library(devtools)
  load_all("skel")
} else {
  library(mlrMBO)  
}

configureMlr(show.learner.output=FALSE)
test_dir("skel/inst/tests")
