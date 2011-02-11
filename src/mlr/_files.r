pack.files = c(
		"mlr/log/log.r",
		"mlr/errorhandler.r",
##		"mlr/packagedesc.r",
		"mlr/aaa.r",
		"mlr/helpers.r",
    "mlr/empty.r",
#		"mlr/to.string.r",
		"mlr/object.r",
		
		"mlr/io/read.arff.r",
		"mlr/data/data.chars.r",

		"mlr/prepare.df.r",
		"mlr/task.desc.r",
		"mlr/task.learn.r",
		"mlr/task.classif.r",
		"mlr/task.regr.r",
		"mlr/makeClassifTask.R",
    "mlr/makeRegrTask.R",
    "mlr/check.costs.r",
    
		"mlr/Parameter.R",
    "mlr/Parameter_make.R",
    "mlr/Parameter_feasible.R",
#    "mlr/Parameter_randomVal.R",
    "mlr/LearnerParameter.R",
    "mlr/LearnerParameter_make.R",
    "mlr/optimize/bounds.r",
    "mlr/learner.r",
		"mlr/learnerR.r",
		"mlr/learner.make.r",
		"mlr/learners.r",
		"mlr/get.learners.r",
		"mlr/setHyperPars.R",
		"mlr/set.id.r",
    "mlr/set.predict.type.r",
    
    "mlr/wrapped.model.r",
    "mlr/wrapped.model.make.r",
    "mlr/learner.failure.r",
		
		"mlr/train.learner.r",
		"mlr/train.task.r",
		"mlr/check.task.learner.r",
		
		"mlr/pred.learner.r",
		"mlr/prediction.r",
		"mlr/predict.r",
		"mlr/set.threshold.r",
		#"mlr/loss.r",
    "mlr/aggr.r",
    "mlr/measure.r",
    "mlr/measure.make.r",
    "mlr/measure.make.costs.r",
    "mlr/measures.r",
		"mlr/set.aggr.r",
    "mlr/performance.r",
    
		"mlr/resample/resample.desc.r",
		"mlr/resample/resample.desc.make.r",
		"mlr/resample/resample.instance.r",
		"mlr/resample/resample.instance.make.r",
		"mlr/resample/get.train.test.r",
		"mlr/resample/cv.desc.r",
    "mlr/resample/loo.desc.r",
    "mlr/resample/cv.strat.desc.r",
    "mlr/resample/cv.rep.desc.r",
    "mlr/resample/bs.desc.r",
    "mlr/resample/subsample.desc.r",
		"mlr/resample/holdout.desc.r",
		"mlr/resample/cv.instance.r",
    "mlr/resample/loo.instance.r",
    "mlr/resample/cv.strat.instance.r",
    "mlr/resample/cv.rep.instance.r",
    "mlr/resample/bs.instance.r",
    "mlr/resample/subsample.instance.r",
		"mlr/resample/holdout.instance.r",
		"mlr/resample/prediction.resample.r",
		"mlr/resample/resample.r",
		"mlr/conf.matrix.r",

    "mlr/rocr/as.rocr.preds.r",
    "mlr/rocr/ROCR_zzz.R",
    "mlr/rocr/ROCR_aux.R",
    "mlr/rocr/ROCR_prediction.R",
    "mlr/rocr/ROCR_performance_measures.R",
    "mlr/rocr/ROCR_performance.R",
    "mlr/rocr/ROCR_performance_plots.R",
    "mlr/rocr/ROCR_mlr_plots.r",

    "mlr/optml/opt.control.r",
    "mlr/optml/opt.result.r",
    "mlr/optml/optml.helpers.r",
    "mlr/optml/tune.helpers.r",
    "mlr/optml/varsel.helpers.r",
    "mlr/optml/opt.model.r",
    
    "mlr/optimize/opt.path.r",
    "mlr/optimize/makeDesign.R",
    "mlr/optimize/myspo/makeSPOFunction.R",
    "mlr/optimize/myspo/makeSPOControl.R",
    "mlr/optimize/myspo/proposePoints.R",
    "mlr/optimize/myspo/spo.R",
    
		"mlr/wrappers/base.wrapper.r",
    "mlr/wrappers/BaseCombiner.R",
    #		"mlr/wrappers/novars.r",
		"mlr/wrappers/preproc.wrapper.r",
		"mlr/wrappers/multiclass.wrapper.r",
    "mlr/wrappers/probth.wrapper.r",
    "mlr/optml/opt.wrapper.r",
    "mlr/wrappers/makeVarselWrapper.r",
    "mlr/wrappers/makeCombineWrapperRegrAvg.R",
    "mlr/varsel/filter.wrapper.r",
    
		
		"mlr/tune/control.tune.r",
		"mlr/tune/control.grid.r",
#		"mlr/tune/control.ps.r",
		"mlr/tune/control.optim.r",
		"mlr/tune/control.cmaes.r",
    "mlr/tune/control.diceoptim.r",
    "mlr/tune/tune.r",
		"mlr/tune/tune.threshold.r",
		"mlr/tune/tune.grid.r",
		"mlr/tune/tune.cmaes.r",
    "mlr/tune/tune.diceoptim.r",
    #		"mlr/tune/pattern.search.r",
#		"mlr/tune/tune.ps.r",
		"mlr/tune/tune.optim.r",
		"mlr/tune/tune.wrapper.r",
#		"mlr/varsel/varsel.helpers.r",
		"mlr/varsel/novars.r",
		"mlr/varsel/control.varsel.r",
		"mlr/varsel/control.seq.r",
		"mlr/varsel/control.randomvarsel.r",
    "mlr/varsel/control.exhvarsel.r",
    "mlr/varsel/varsel.random.r",
    "mlr/varsel/varsel.exhaustive.r",
    "mlr/varsel/varsel.bestcor.r",
		"mlr/varsel/varsel.hybrid.r",
		"mlr/varsel/varsel.hybrid2.r",
		"mlr/varsel/varsel.seq.r",
		"mlr/varsel/varsel.r",
    "mlr/varsel/filter.r",
    
		
		"mlr/modelcompare/benchmark.r",
		"mlr/modelcompare/benchexp.r",
		"mlr/modelcompare/benchresult.r",
		
		"mlr/parallel/export.r",
		"mlr/parallel/eval.r",
		"mlr/parallel/mylapply.r",
		"mlr/parallel/parallel.r",
    
#################    

		"mlr/models/classification/ada.r",
		"mlr/models/classification/adaboost.m1.classif.r",
		"mlr/models/classification/blackboost.classif.r",
		"mlr/models/classification/ctree.classif.r",
    "mlr/models/classification/fnn.classif.r",
    "mlr/models/classification/gbm.classif.r",
		"mlr/models/classification/glmboost.classif.r",
		"mlr/models/classification/grplasso.classif.r",
		"mlr/models/classification/j48.r",
		"mlr/models/classification/JRip.r",
		"mlr/models/classification/kknn.classif.r",
		"mlr/models/classification/ksvm.classif.r",
		"mlr/models/classification/lda.r",
		"mlr/models/classification/loclda.r",
		"mlr/models/classification/logreg.r",
		"mlr/models/classification/lssvm.r",
		"mlr/models/classification/lvq1.classif.r",
		"mlr/models/classification/mda.r",
		"mlr/models/classification/multinom.r",
		"mlr/models/classification/OneR.r",
		"mlr/models/classification/nb.r",
		"mlr/models/classification/nnet.r",
		"mlr/models/classification/PART.r",
		"mlr/models/classification/penalizedSVM.R",
		"mlr/models/classification/qda.r",
		"mlr/models/classification/randomForest.classif.r",
		"mlr/models/classification/rda.r",
		"mlr/models/classification/rpart.classif.r",
		"mlr/models/classification/sda.r",
		"mlr/models/classification/svm.classif.r",
		##		"mlr/models/classification/metacost.r"
    
##########

#		"mlr/models/regression/bagEarth.regr.r",
		"mlr/models/regression/blackboost.regr.r",
		"mlr/models/regression/earth.regr.r",
    "mlr/models/regression/fnn.regr.r",
    "mlr/models/regression/gbm.regr.r",
#		"mlr/models/regression/icr.r",
		"mlr/models/regression/kknn.regr.r",
		"mlr/models/regression/ksvm.regr.r",
    "mlr/models/regression/krigdice.regr.r",
    "mlr/models/regression/lasso.r",
		"mlr/models/regression/lm.r",
		"mlr/models/regression/mars.regr.r",
    "mlr/models/regression/nnet.regr.r",
    "mlr/models/regression/pcr.r",
		"mlr/models/regression/randomForest.regr.r",
		"mlr/models/regression/ridge.r",
		"mlr/models/regression/rpart.regr.r",
    "mlr/models/regression/rsm.r",
    "mlr/models/regression/rvm.regr.r"
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
