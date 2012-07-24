library(methods)
library(testthat)

if (interactive()) {
  library(devtools)
  load_all("skel")
} else {
  library(mlrChains)  
}

configureMlr(show.learner.output=FALSE)

source("skel/inst/tests/objects.R")
test_dir("skel/inst/tests")
