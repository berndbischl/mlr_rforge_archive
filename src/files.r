base.files <<- c(
		"log/log.r",
#		"base/packagedesc.r",
		"base/aaa.r",
		"base/to.string.r",
		
		"base/learner.props.r",
		"base/learner.props.classif.r",
		"base/learner.props.regr.r",
		
		"base/wrapped.learner.r",
		"base/wrapped.learner.classif.r",
		"base/wrapped.learner.regr.r",
		"base/wrapped.learner.make.r",
		
		"base/data.desc.r",
		"base/prepare.df.r",
		"base/task.learn.r",
		"base/task.classif.r",
		"base/task.classif.make.r",
		"base/task.regr.r",
		"base/task.regr.make.r",
		"base/check.task.r",
		
		"base/wrapped.model.r",
		"base/learner.failure.r",
		
		"base/train.learner.r",
		"base/train.task.r",
		"base/check.task.learner.r",
		
		"base/predict.learner.r",
		"base/predict.r",
		"base/loss.r",
		"base/performance.r",
		
		"base/resample/resample.desc.r",
		"base/resample/resample.desc.make.r",
		"base/resample/resample.instance.r",
		"base/resample/resample.instance.make.r",
		"base/resample/cv.desc.r",
		"base/resample/bs.desc.r",
		"base/resample/subsample.desc.r",
		"base/resample/holdout.desc.r",
		"base/resample/cv.instance.r",
		"base/resample/bs.instance.r",
		"base/resample/subsample.instance.r",
		"base/resample/holdout.instance.r",
		"base/resample/resample.result.r",
		"base/resample/resample.fit.r",
		"base/resample/resample.performance.r",
		"base/binary.r",
		
		
		"base/conf.matrix.r",
		
		
		"base/tune/check.ranges.r",
		"base/tune/combine.ranges.r",
		"base/tune/tune.r",
		"base/tune/tune.grid.r",
#		"base/tune/tune.cmaes.r",
		"base/tune/control.grid.r",
		"base/tune/control.ps.r",
		"base/tune/pattern.search.r",
		"base/tune/tune.ps.r",
		"base/tune/tune.wrapper.r",
		
		"base/benchmark/benchmark.r",
		"base/benchmark/benchexp.r",
		"base/benchmark/benchresult.r",
		"base/benchmark/bench.add.r",
		
		"base/parallel/export.r",
		"base/parallel/eval.r",
		"base/parallel/mylapply.r",
		"base/parallel/parallel.r"
)

classif.files <<- c( 		
		"models/classification/novars.r",
		"models/classification/knn.r",
		"models/classification/lda.r",
		"models/classification/qda.r",
		"models/classification/rda.r",
		"models/classification/mda.r",
		"models/classification/loclda.r",
		"models/classification/logreg.r",
		"models/classification/multinom.r",
		"models/classification/nb.r",
		"models/classification/rpart.r",
		"models/classification/randomForest.r",
		"models/classification/boost.r",
		"models/classification/ada.r",
		"models/classification/blackboost.r",
		"models/classification/svm.r"
#		"models/classification/metacost.r"
)

regr.files <<- c( 		
		"models/regression/lm.r",
		"models/regression/blackboost.r",
		"models/regression/gbm.r",
		"models/regression/kknn.r",
##		"models/regression/regr.myknn.r",
		"models/regression/ridge.r",
		"models/regression/lasso.r"
)


load.all.libs <- function() {
	library(RUnit)
	library(MASS)
	library(rpart)
	library(e1071)
	library(boot)
	library(roxygen)
	library(kernlab)
	library(adabag)
	library(kknn)
	library(randomForest)
	library(ada)
	library(mboost)
	library(mda)
	library(gbm)
	library(penalized)
	library(mlbench)
	library(reshape)
	library(klaR)
	library(snowfall)
	library(nnet)
}

load.all.sources <- function(prefix) {
	fs = c(base.files, classif.files, regr.files)
	for (f in fs) {
		source(file.path(prefix, f))
	}
}
