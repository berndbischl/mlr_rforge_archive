library(methods)
library(testthat)
library(devtools)
library(cmaes)
load_all("skel", reset=TRUE)
configureMlr(show.learner.output=FALSE, on.learner.error="stop")

set.seed(2)

