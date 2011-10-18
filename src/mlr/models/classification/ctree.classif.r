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
		"classif.ctree", 
		contains = c("rlearner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("classif.ctree"),
		def = function(.Object) {
			
      par.set = makeParameterSet(
        makeDiscreteLearnerParameter(id="teststat", default="quad", vals=c("quad", "max")),
        makeDiscreteLearnerParameter(id="testtype", default="Bonferroni", vals=c("Bonferroni", "MonteCarlo", "Univariate", "Teststatistic")),
        makeNumericLearnerParameter(id="mincriterion", default=0.95, lower=0, upper=1),
        makeIntegerLearnerParameter(id="minsplit", default=20L, lower=1L),
        makeIntegerLearnerParameter(id="minbucket", default=7L, lower=1L),
        makeLogicalLearnerParameter(id="stump", default=FALSE),
        makeIntegerLearnerParameter(id="nresample", default=9999L, lower=1L, requires=expression(testtype=="MonteCarlo")),
        makeIntegerLearnerParameter(id="maxsurrogate", default=0L, lower=0L),
        makeIntegerLearnerParameter(id="mtry", default=0L, lower=0L),
        makeLogicalLearnerParameter(id="savesplitstats", default=TRUE),
        makeIntegerLearnerParameter(id="maxdepth", default=0L, lower=0L)
      )
      
      .Object = callNextMethod(.Object, pack="party", par.set=par.set)
    
      setProperties(.Object, 
        twoclass = TRUE,
        multiclass = TRUE,
        missings = TRUE,
        numerics = TRUE,
        factors = TRUE,
        prob = TRUE,
        weights = TRUE
      )
    }
)

#' @rdname trainLearner


setMethod(
		f = "trainLearner",
		signature = signature(
				.learner="classif.ctree", 
				.task="ClassifTask", .subset="integer" 
		),
		
		def = function(.learner, .task, .subset,  ...) {
			ns = c("teststat", "testtype", "mincriterion", "minsplit", "minbucket", "stump", 
					"nresample", "maxsurrogate", "mtry", "savesplitstats", "maxdepth")
			xs = learnerArgsToControl(ctree_control, ns, list(...))
			f = getFormula(.task)
			args = c(list(f, data=getData(.task, .subset), control=xs$control), xs$args)
			do.call(ctree, args)
		}
)
#' @rdname predictLearner

setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "classif.ctree", 
				.model = "WrappedModel", 
				.newdata = "data.frame" 
		),
		
		def = function(.learner, .model, .newdata, ...) {
			if (.learner@predict.type == "prob") {
				m = .model@learner.model
				p = treeresponse(m, newdata=.newdata, ...)
				p = do.call(rbind, p)
				rownames(p) = NULL
				colnames(p) = m@responses@levels[[.model@desc@target]]
				return(p)
			} else 
				predict(.model@learner.model, newdata=.newdata, ...)
			
		}
)