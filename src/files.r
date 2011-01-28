base.files <<- c(
		"log/log.r",
		"base/errorhandler.r",
##		"base/packagedesc.r",
		"base/aaa.r",
		"base/helpers.r",
    "base/empty.r",
#		"base/to.string.r",
		"base/object.r",
		
		"base/io/read.arff.r",
		"base/data/data.chars.r",

		"base/prepare.df.r",
		"base/task.desc.r",
		"base/task.learn.r",
		"base/task.classif.r",
		"base/task.regr.r",
		"base/task.make.r",
    "base/check.costs.r",
    
		"base/Parameter.R",
    "base/Parameter_make.R",
    "base/Parameter_feasible.R",
    "base/Parameter_randomVal.R",
    "base/LearnerParameter.R",
    "base/LearnerParameter_make.R",
    "base/learner.r",
		"base/learnerR.r",
		"base/learner.make.r",
		"base/learners.r",
		"base/get.learners.r",
		"base/set.hyper.pars.r",
		"base/set.id.r",
    "base/set.predict.type.r",
    
    "base/wrapped.model.r",
    "base/wrapped.model.make.r",
    "base/learner.failure.r",
		
		"base/train.learner.r",
		"base/train.task.r",
		"base/check.task.learner.r",
		
		"base/pred.learner.r",
		"base/prediction.r",
		"base/predict.r",
		"base/set.threshold.r",
		#"base/loss.r",
    "base/aggr.r",
    "base/measure.r",
    "base/measure.make.r",
    "base/measure.make.costs.r",
    "base/measures.r",
		"base/set.aggr.r",
    "base/performance.r",
    
		"base/resample/resample.desc.r",
		"base/resample/resample.desc.make.r",
		"base/resample/resample.instance.r",
		"base/resample/resample.instance.make.r",
		"base/resample/get.train.test.r",
		"base/resample/cv.desc.r",
    "base/resample/cv.strat.desc.r",
    "base/resample/cv.rep.desc.r",
    "base/resample/bs.desc.r",
    "base/resample/subsample.desc.r",
		"base/resample/holdout.desc.r",
		"base/resample/cv.instance.r",
    "base/resample/cv.strat.instance.r",
    "base/resample/cv.rep.instance.r",
    "base/resample/bs.instance.r",
    "base/resample/subsample.instance.r",
		"base/resample/holdout.instance.r",
		"base/resample/prediction.resample.r",
		"base/resample/resample.r",
		"base/conf.matrix.r",

    "base/rocr/as.rocr.preds.r",
    "base/rocr/ROCR_zzz.R",
    "base/rocr/ROCR_aux.R",
    "base/rocr/ROCR_prediction.R",
    "base/rocr/ROCR_performance_measures.R",
    "base/rocr/ROCR_performance.R",
    "base/rocr/ROCR_performance_plots.R",
    "base/rocr/ROCR_mlr_plots.r",

    "base/optimize/bounds.r",
    "base/optimize/opt.path.r",
    
		"base/wrappers/base.wrapper.r",
#		"base/wrappers/novars.r",
		"base/wrappers/preproc.wrapper.r",
		"base/wrappers/multiclass.wrapper.r",
    "base/wrappers/probth.wrapper.r",
    
		"base/optml/opt.control.r",
		"base/optml/opt.result.r",
		"base/optml/optml.helpers.r",
		"base/optml/tune.helpers.r",
		"base/optml/varsel.helpers.r",
    "base/optml/opt.model.r",
    "base/optml/opt.wrapper.r",
		
		"base/tune/control.tune.r",
		"base/tune/control.grid.r",
#		"base/tune/control.ps.r",
		"base/tune/control.optim.r",
		"base/tune/control.cmaes.r",
    "base/tune/control.diceoptim.r",
    "base/tune/tune.r",
		"base/tune/tune.threshold.r",
		"base/tune/tune.grid.r",
		"base/tune/tune.cmaes.r",
    "base/tune/tune.diceoptim.r",
    #		"base/tune/pattern.search.r",
#		"base/tune/tune.ps.r",
		"base/tune/tune.optim.r",
		"base/tune/tune.wrapper.r",
#		"base/varsel/varsel.helpers.r",
		"base/varsel/novars.r",
		"base/varsel/control.varsel.r",
		"base/varsel/control.seq.r",
		"base/varsel/control.randomvarsel.r",
    "base/varsel/control.exhvarsel.r",
    "base/varsel/varsel.random.r",
    "base/varsel/varsel.exhaustive.r",
    "base/varsel/varsel.bestcor.r",
		"base/varsel/varsel.hybrid.r",
		"base/varsel/varsel.hybrid2.r",
		"base/varsel/varsel.seq.r",
		"base/varsel/varsel.r",
    "base/varsel/filter.r",
    "base/varsel/varsel.wrapper.r",
    "base/varsel/filter.wrapper.r",
    
		
		"base/modelcompare/benchmark.r",
		"base/modelcompare/benchexp.r",
		"base/modelcompare/benchresult.r",
		
		"base/parallel/export.r",
		"base/parallel/eval.r",
		"base/parallel/mylapply.r",
		"base/parallel/parallel.r"
)

classif.files <<- c( 		
		"models/classification/ada.r",
		"models/classification/adaboost.m1.classif.r",
		"models/classification/blackboost.classif.r",
		"models/classification/ctree.classif.r",
    "models/classification/fnn.classif.r",
    "models/classification/gbm.classif.r",
		"models/classification/glmboost.classif.r",
		"models/classification/grplasso.classif.r",
		"models/classification/j48.r",
		"models/classification/JRip.r",
		"models/classification/kknn.classif.r",
		"models/classification/ksvm.classif.r",
		"models/classification/lda.r",
		"models/classification/loclda.r",
		"models/classification/logreg.r",
		"models/classification/lssvm.r",
		"models/classification/lvq1.classif.r",
		"models/classification/mda.r",
		"models/classification/multinom.r",
		"models/classification/OneR.r",
		"models/classification/nb.r",
		"models/classification/nnet.r",
		"models/classification/PART.r",
		"models/classification/penalizedSVM.R",
		"models/classification/qda.r",
		"models/classification/randomForest.classif.r",
		"models/classification/rda.r",
		"models/classification/rpart.classif.r",
		"models/classification/sda.r",
		"models/classification/svm.classif.r"
		##		"models/classification/metacost.r"
)

regr.files <<- c( 	
#		"models/regression/bagEarth.regr.r",
		"models/regression/blackboost.regr.r",
		"models/regression/earth.regr.r",
    "models/regression/fnn.regr.r",
    "models/regression/gbm.regr.r",
#		"models/regression/icr.r",
		"models/regression/kknn.regr.r",
		"models/regression/ksvm.regr.r",
    "models/regression/krigdice.regr.r",
    "models/regression/lasso.r",
		"models/regression/lm.r",
		"models/regression/mars.regr.r",
    "models/regression/nnet.regr.r",
    "models/regression/pcr.r",
		"models/regression/randomForest.regr.r",
		"models/regression/ridge.r",
		"models/regression/rpart.regr.r",
    "models/regression/rsm.r",
    "models/regression/rvm.regr.r"
)


load.all.libs <- function() {
	library(abind)
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
	library(RWeka)
	library(party)
	library(earth)
	library(cmaes)
}

load.all.sources <- function(prefix) {
	fs = c(base.files, classif.files, regr.files)
	for (f in fs) {
		source(file.path(prefix, f))
	}
}
