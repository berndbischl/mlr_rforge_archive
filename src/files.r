base.files <<- c( 		
		"log/log.r",
		"base/packagedesc.r",
		"base/zzz.r",
		
		"base/learner.props.r",
		"base/learner.props.classif.r",
		"base/learner.props.regr.r",

		"base/wrapped.learner.r",
		"base/wrapped.learner.classif.r",
		"base/wrapped.learner.regr.r",
		
		"base/data.desc.r",
		"base/prepare.df.r",
		"base/task.learn.r",
		"base/task.classif.r",
		"base/task.classif.make.r",
		"base/task.regr.r",
		"base/task.regr.make.r",
		"base/check.task.classif.r",

		"base/wrapped.model.r",
		"base/learner.failure.r",
				
		"base/train.learner.r",
		"base/train.task.r",
		"base/train.task.classif.r",
		"base/train.task.regr.r",

		"base/predict.classif.r",
		"base/predict.regr.r",
		"base/perf.measure.r",
		"base/performance.r",
		
		"base/resample.desc.r",
		"base/cv.desc.r",
		"base/bs.desc.r",
		"base/subsample.desc.r",
		
		"base/resample.instance.r",
		"base/cv.instance.r",
		"base/bs.instance.r",
		"base/subsample.instance.r",
		
		
		"base/resample.result.r",
		"base/resample.fit.r",
		"base/resample.fit.iter.r",
		"base/resample.performance.r",
		"base/conf.matrix.r",
		
		"base/tune.r",
		"base/benchmark.r",
		"base/parallel.r"
)

classif.files <<- c( 		
		"models/classification/boost.r",
		"models/classification/knn.r",
		"models/classification/lda.r",
		"models/classification/logreg.r",
		"models/classification/mda.r",
		"models/classification/multinom.r",
		"models/classification/nb.r",
		"models/classification/qda.r",
		"models/classification/randomForest.r",
		"models/classification/rda.r",
		"models/classification/rpart.r",
		"models/classification/svm.r"
)

regr.files <<- c( 		
		"models/regression/lm.r",
		"models/regression/blackboost.r",
		"models/regression/gbm.r",
		"models/regression/kknn.r",
#		"models/regression/regr.myknn.r",
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


