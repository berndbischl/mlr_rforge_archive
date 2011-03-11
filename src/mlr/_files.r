pack.files = c(
		"mlr/log/log.r",
		"mlr/errorhandler.r",
##		"mlr/packagedesc.r",
		"mlr/aaa.r",
		"mlr/helpers.r",
    "mlr/empty.r",
#		"mlr/to.string.r",
		"mlr/object.r",
		
		"mlr/prepare.df.r",
		"mlr/task.desc.r",
		"mlr/LearnTask.R",
		"mlr/ClassifTask.R",
		"mlr/RegrTask.R",
		"mlr/makeClassifTask.R",
    "mlr/makeRegrTask.R",
    "mlr/check.costs.r",
    
		"mlr/Parameter.R",
    "mlr/Parameter_make.R",
    "mlr/Parameter_feasible.R",
#    "mlr/Parameter_randomVal.R",
    "mlr/LearnerParameter.R",
    "mlr/LearnerParameter_make.R",
    "mlr/ParameterSet.R",
    "mlr/LearnerDesc.R",
    "mlr/Learner.R",
		"mlr/learnerR.r",
		"mlr/makeLearner.R",
		"mlr/learners.r",
		"mlr/getLearners.R",
		"mlr/setHyperPars.R",
		"mlr/setId.R",
    "mlr/setPredictType.R",
    
    "mlr/WrappedModel.R",
    "mlr/WrappedModel_make.R",
    "mlr/FailureModel.R",
		
		"mlr/trainLearner.R",
		"mlr/train.R",
		"mlr/check.task.learner.r",
		
		"mlr/predictLearner.R",
		"mlr/Prediction.R",
		"mlr/predict.r",
		"mlr/setThreshold.R",
		#"mlr/loss.r",
    "mlr/Aggregation.R",
    "mlr/aggregations.R",
    "mlr/Measure.R",
    "mlr/Measure_make.R",
    "mlr/Measure_make_cost.R",
    "mlr/measures.R",
		"mlr/setAggregation.R",
    "mlr/performance.r",
    
		"mlr/resample/ResampleDesc.R",
		"mlr/resample/ResampleDesc_make.R",
		"mlr/resample/ResampleInstance.R",
		"mlr/resample/ResampleInstance_make.R",
		"mlr/resample/get.train.test.r",
		"mlr/resample/CVDesc.R",
    "mlr/resample/LOODesc.R",
    "mlr/resample/StratCVDesc.R",
    "mlr/resample/RepCVDesc.R",
    "mlr/resample/BSDesc.R",
    "mlr/resample/SubsampleDesc.R",
		"mlr/resample/HoldoutDesc.R",
		"mlr/resample/cv.instance.r",
    "mlr/resample/loo.instance.r",
    "mlr/resample/cv.strat.instance.r",
    "mlr/resample/cv.rep.instance.r",
    "mlr/resample/bs.instance.r",
    "mlr/resample/subsample.instance.r",
		"mlr/resample/holdout.instance.r",
		"mlr/resample/ResamplePrediction.R",
		"mlr/resample/resample.r",
		"mlr/conf.matrix.r",
    "mlr/resample/iters.R",
    
    "mlr/rocr/as.rocr.preds.r",
    "mlr/rocr/ROCR_zzz.R",
    "mlr/rocr/ROCR_aux.R",
    "mlr/rocr/ROCR_prediction.R",
    "mlr/rocr/ROCR_performance_measures.R",
    "mlr/rocr/ROCR_performance.R",
    "mlr/rocr/ROCR_performance_plots.R",
    "mlr/rocr/ROCR_mlr_plots.r",

    "mlr/optml/OptControl.R",
    "mlr/optml/opt.result.r",
    "mlr/optml/optml.helpers.r",
    "mlr/optml/tune.helpers.r",
    "mlr/optml/varsel.helpers.r",
    "mlr/optml/opt.model.r",
    
    "mlr/optimize/OptPath.R",
    "mlr/optimize/makeDesign.R",
    "mlr/optimize/myspo/makeSPOFunction.R",
    "mlr/optimize/myspo/SPOControl.R",
    "mlr/optimize/myspo/proposePoints.R",
    "mlr/optimize/myspo/spo.R",
    
		"mlr/wrappers/BaseWrapper.R",
    "mlr/wrappers/BaseCombiner.R",
    "mlr/wrappers/OptWrapper.R",
    "mlr/wrappers/TuneWrapper.R",
    "mlr/wrappers/VarselWrapper.R",
    "mlr/wrappers/PreprocWrapper.R",
		"mlr/wrappers/MulticlassWrapper.R",
    "mlr/wrappers/ProbthWrapper.R",
    "mlr/wrappers/CombineWrapperRegrAvg.R",
    "mlr/wrappers/FilterWrapper.R",
    
		
		"mlr/tune/TuneControl.R",
    "mlr/tune/TuneGridControl.R",
    "mlr/tune/TuneOptimControl.R",
    "mlr/tune/TuneCMAESControl.R",
    "mlr/tune/TuneSPOControl.R",
    "mlr/tune/tune.r",
		"mlr/tune/tune.threshold.r",
		"mlr/tune/tune.grid.r",
    "mlr/tune/tune.optim.r",
    "mlr/tune/tune.cmaes.r",
    "mlr/tune/tune.spo.R",
#		"mlr/varsel/varsel.helpers.r",
		"mlr/varsel/novars.r",
		"mlr/varsel/control.varsel.r",
		"mlr/varsel/control.seq.r",
		"mlr/varsel/control.randomvarsel.r",
    "mlr/varsel/control.exhvarsel.r",
    "mlr/varsel/varsel.random.r",
    "mlr/varsel/varsel.exhaustive.r",
#    "mlr/varsel/varsel.bestcor.r",
#		"mlr/varsel/varsel.hybrid.r",
#		"mlr/varsel/varsel.hybrid2.r",
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
    "mlr/models/regression/krigdice.forrester.r",
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

