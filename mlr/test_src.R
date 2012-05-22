library("devtools")
library("testthat")
library(mlbench)

load_all("skel")
source("skel/inst/tests/objects.R")

lrn = makeLearner("classif.lda")
task = makeClassifTask(data=iris, target="Species")
rdesc = makeResampleDesc("CV", iters=2)
print(rdesc)
rin = makeResampleInstance(rdesc, task=task)
print(rin)
#resample
