base.files <<- c(
		"log/log.r",
##		"base/packagedesc.r",
		"base/aaa.r",
		"base/helpers.r",
#		"base/to.string.r",
		"base/object.r",
		
		"base/learner.props.r",
			
		"base/data.desc.r",
		"base/prepare.df.r",
		"base/task.desc.r",
		"base/task.learn.r",
		"base/task.classif.r",
		"base/task.regr.r",
		"base/task.make.r",
		"base/check.task.r",
		
		"base/learner.r",
		"base/learnerR.r",
		"base/learner.make.r",
		"base/learners.r",
		"base/get.learners.r",
		"base/set.hyper.pars.r",
		
		
		"base/wrapped.model.r",
		"base/learner.failure.r",
		
		"base/train.learner.r",
		"base/train.task.r",
		"base/check.task.learner.r",
		
		"base/pred.learner.r",
		"base/prediction.r",
		"base/predict.r",
		"base/threshold.r",
		"base/loss.r",
		"base/performance.r",
		"base/measures.r",
		
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
		"base/resample/resample.prediction.r",
		"base/resample/resample.fit.r",
		"base/resample/resample.performance.r",
		"base/conf.matrix.r",
		
		"base/wrappers/base.wrapper.r",
#		"base/wrappers/novars.r",
		"base/wrappers/preproc.wrapper.r",
		"base/wrappers/multiclass.wrapper.r",
		
		"base/optml/opt.control.r",
		"base/optml/opt.result.r",
		"base/optml/optml.helpers.r",
		"base/optml/tune.helpers.r",
		"base/optml/varsel.helpers.r",
		"base/optml/opt.wrapper.r",
		
		"base/tune/control.tune.r",
		"base/tune/control.grid.r",
#		"base/tune/control.ps.r",
		"base/tune/control.nm.r",
		"base/tune/control.cmaes.r",
		"base/tune/check.ranges.r",
		"base/tune/combine.ranges.r",
		"base/tune/tune.r",
		"base/tune/tune.threshold.r",
		"base/tune/tune.grid.r",
		"base/tune/tune.cmaes.r",
#		"base/tune/pattern.search.r",
#		"base/tune/tune.ps.r",
		"base/tune/tune.nm.r",
		"base/tune/tune.wrapper.r",
#		"base/varsel/varsel.helpers.r",
		"base/varsel/novars.r",
		"base/varsel/control.varsel.r",
		"base/varsel/control.seq.r",
		"base/varsel/varsel.random.r",
		"base/varsel/varsel.bestcor.r",
		"base/varsel/varsel.hybrid.r",
		"base/varsel/varsel.hybrid2.r",
		"base/varsel/varsel.seq.r",
		"base/varsel/varsel.r",
		"base/varsel/varsel.wrapper.r",
		
		
		"base/benchmark/benchmark.r",
		"base/benchmark/benchexp.r",
		"base/benchmark/benchresult.r",
		
		"base/parallel/export.r",
		"base/parallel/eval.r",
		"base/parallel/mylapply.r",
		"base/parallel/parallel.r"
)

classif.files <<- c( 		
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
		"models/classification/j48.r",
		"models/classification/randomForest.r",
		"models/classification/boost.r",
		"models/classification/ada.r",
		"models/classification/blackboost.classif.r",
		"models/classification/glmboost.classif.r",
		"models/classification/gbm.classif.r",
		"models/classification/kernlab.svm.classif.r",
		"models/classification/svm.classif.r",
		"models/classification/nnet.r",
		"models/classification/grplasso.classif.r",
		"models/classification/lvq1.classif.r",
		"models/classification/llr.r"
##		"models/classification/metacost.r"
)

regr.files <<- c( 		
		"models/regression/lm.r",
		"models/regression/blackboost.regr.r",
		"models/regression/gbm.regr.r",
		"models/regression/kknn.r",
		"models/regression/ridge.r",
		"models/regression/lasso.r",
		"models/regression/kernlab.svm.regr.r"
)


load.all.libs <- function() {
	library(abind)
	library(RUnit)
	library(MASS)
	library(rpart)
	library(e1071)
	library(boot)
	library(roxygen)
#	library(kernlab)
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
	library(RWeka)
	library(party)
}

load.all.sources <- function(prefix) {
	fs = c(base.files, classif.files, regr.files)
	for (f in fs) {
		source(file.path(prefix, f))
	}
}
