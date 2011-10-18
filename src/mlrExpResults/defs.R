library(mlr)
library(mlrData)

#todo: stratified subsampling
#todo: phd: test spo againts grid against neldermead against untuned for svm tuning 

rin.type = "Subsample"
rin.iters = 5
rin.path = "resampling"
#rin.splits = c(1/2, 2/3, 4/5, 9/10)
rin.splits = c(1/2, 9/10)
#names(rin.splits) = c("12", "23", "45", "910")
names(rin.splits) = c("12", "910")

results.path = "results"
jobs.path = "submitR"

dss.classif = c(
  "Iris",
  "Ionosphere"
)

learners.classif = list(
  makeLearner("classif.lda", id="lda"),
  makeLearner("classif.qda", id="qda"),
  makeLearner("classif.ksvm", id="svm-lin"),
  makeLearner("classif.ksvm", id="svm-poly"),
  makeLearner("classif.ksvm", id="svm-rbf"),
  makeLearner("classif.rpart", id="rpart"),
  makeLearner("classif.randomForest", id="randomForest")
)
names(learners.classif) = sapply(learners.classif, function(x) x@id)
learners.regr = list(
  makeLearner("regr.lm", id="lm")
)
names(learners.regr) = sapply(learners.regr, function(x) x@id)
learners = c(
  classif=learners.classif,
  regr=learners.regr
)

measures.multi = list(mmce, ber)
names(measures.multi) = sapply(measures.multi, function(x) x@id)
measures.binary = list(auc, f1, gmean)
names(measures.binary) = sapply(measures.binary, function(x) x@id)
measures.regr = list(rmse, mae)
names(measures.regr) = sapply(measures.regr, function(x) x@id)
