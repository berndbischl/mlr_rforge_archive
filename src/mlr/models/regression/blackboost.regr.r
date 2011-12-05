#' @include learnerR.r
roxygen()
#' @include RegrTask.R
roxygen()

setClass(
		"regr.blackboost", 
		contains = c("rlearner.regr")
)


setMethod(
		f = "initialize",
		signature = signature("regr.blackboost"),
		def = function(.Object) {			
      par.set = makeParamSet(
        makeDiscreteLearnerParam(id="family", default=Gaussian(), values=list(Gaussian=Gaussian(), Huber=Huber(), Laplace=Laplace())),
        makeIntegerLearnerParam(id="mstop", default=100L, lower=1L),
        makeNumericLearnerParam(id="nu", default=0.1, lower=0, upper=1),
        makeDiscreteLearnerParam(id="teststat", default="quad", values=c("quad", "max")),
        makeDiscreteLearnerParam(id="testtype", default="Bonferroni", values=c("Bonferroni", "MonteCarlo", "Univariate", "Teststatistic")),
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
			
      .Object = callNextMethod(.Object, pack=c("mboost", "party"), par.set=par.set)
      
      setProperties(.Object,
        missings = FALSE,
        numerics = TRUE,
        factors = TRUE,
        se = FALSE,
        weights = TRUE
      )
		}
)

#' @rdname trainLearner

setMethod(
		f = "trainLearner",
		signature = signature(
				.learner="regr.blackboost", 
				.task="RegrTask", .subset="integer" 
		),
		
		def = function(.learner, .task, .subset, ...) {
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
				.learner = "regr.blackboost", 
				.model = "WrappedModel", 
				.newdata = "data.frame" 
		),
		
		def = function(.learner, .model, .newdata, ...) {
			predict(.model@learner.model, newdata=.newdata, ...)
		}
)	





