# todo: for ctree_control we should load party as well. pack / packs in learner?

#' @include learnerR.r
roxygen()
#' @include WrappedModel.R
roxygen()
#' @include trainLearner.R
roxygen()
#' @include predictLearner.R
roxygen()
#' @include ClassifTask.R
roxygen()


setClass(
		"classif.blackboost", 
		contains = c("rlearner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("classif.blackboost"),
		def = function(.Object) {
			
			
			.Object = callNextMethod(.Object, pack=c("mboost", "party"))
      
      .Object = setProperties(.Object, 
        twoclass = TRUE,
        missings = TRUE,
        numerics = TRUE,
        factors = TRUE,
        prob = TRUE,
        weights = TRUE
      )
      
			par.set = makeParamSet(
					makeDiscreteLearnerParam(id="family", default="Binomial", vals=list(AdaExp=AdaExp(), Binomial=Binomial())),
          makeIntegerLearnerParam(id="mstop", default=100L, lower=1L),
					makeNumericLearnerParam(id="nu", default=0.1, lower=0, upper=1),
					makeDiscreteLearnerParam(id="teststat", default="quad", vals=c("quad", "max")),
					makeDiscreteLearnerParam(id="testtype", default="Bonferroni", vals=c("Bonferroni", "MonteCarlo", "Univariate", "Teststatistic")),
					makeNumericLearnerParam(id="mincriterion", default=0.95, lower=0, upper=1),
          makeIntegerLearnerParam(id="minsplit", default=20L, lower=1L),
          makeIntegerLearnerParam(id="minbucket", default=7L, lower=1L),
					makeLogicalLearnerParam(id="stump", default=FALSE),
          makeIntegerLearnerParam(id="nresample", default=9999L, lower=1L, requires=expression(testtype=="MonteCarlo")),
          makeIntegerLearnerParam(id="maxsurrogate", default=0L, lower=0L),
          makeIntegerLearnerParam(id="mtry", default=0L, lower=0L),
					makeLogicalLearnerParam(id="savesplitstats", default=TRUE),
          makeIntegerLearnerParam(id="maxdepth", default=0L, lower=0L)
			)
     
			# we have to load the package first for Binomial()
      .Object@par.set = par.set
			setHyperPars(.Object, family="Binomial")
		}
)

#' @rdname trainLearner

setMethod(
		f = "trainLearner",
		signature = signature(
				.learner="classif.blackboost", 
				.task="ClassifTask", .subset="integer" 
		),
		
		def = function(.learner, .task, .subset,  ...) {		
			xs = learnerArgsToControl(boost_control, c("mstop", "nu", "risk"), list(...))
			ys = learnerArgsToControl(ctree_control, c("teststat", "testtype", "mincriterion", "maxdepth"), xs$args)
			f = getFormula(.task)
      args = c(list(f, data=getData(.task, .subset), control=xs$control, tree_control=ys$control), ys$args)
			if (.task@desc@has.weights)
        args$weights = .task@weights[.subset] 
			do.call(blackboost, args)
		}
)

#' @rdname predictLearner

setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "classif.blackboost", 
				.model = "WrappedModel", 
				.newdata = "data.frame" 
		),
		
		def = function(.learner, .model, .newdata, ...) {
			type = ifelse(.learner@predict.type == "response", "class", "response")
			p = predict(.model@learner.model, newdata=.newdata, type=type, ...)
			if (.learner@predict.type == "prob") {
				y = matrix(0, ncol=2, nrow=nrow(.newdata))
				colnames(y) = .model@task.desc@class.levels
				y[,1] = p
				y[,2] = 1-p
				return(y)
			} else {
				return(p)
			}
		}
)	





