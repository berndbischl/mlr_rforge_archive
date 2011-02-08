#' @include learnerR.r
roxygen()
#' @include wrapped.model.r
roxygen()
#' @include train.learner.r
roxygen()
#' @include pred.learner.r
roxygen()
#' @include task.classif.r
roxygen()



setClass(
		"classif.ctree", 
		contains = c("rlearner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("classif.ctree"),
		def = function(.Object) {
			
			desc = c(
					oneclass = FALSE,
					twoclass = TRUE,
					multiclass = TRUE,
					missings = TRUE,
					doubles = TRUE,
					factors = TRUE,
					prob = TRUE,
					decision = FALSE,
					weights = TRUE,
					costs = FALSE
			)
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
      callNextMethod(.Object, pack="party", desc=desc, par.set=par.set)
		}
)

#' @rdname train.learner


setMethod(
		f = "train.learner",
		signature = signature(
				.learner="classif.ctree", 
				.task="classif.task", .subset="integer" 
		),
		
		def = function(.learner, .task, .subset,  ...) {
			ns = c("teststat", "testtype", "mincriterion", "minsplit", "minbucket", "stump", 
					"nresample", "maxsurrogate", "mtry", "savesplitstats", "maxdepth")
			xs = args.to.control(ctree_control, ns, list(...))
			f = .task["formula"]
			args = c(list(f, data=get.data(.task, .subset), control=xs$control), xs$args)
			do.call(ctree, args)
		}
)
#' @rdname pred.learner

setMethod(
		f = "pred.learner",
		signature = signature(
				.learner = "classif.ctree", 
				.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			if (.type == "prob") {
				m = .model["learner.model"]
				p = treeresponse(m, newdata=.newdata, ...)
				p = Reduce(rbind, p)
				rownames(p) = NULL
				colnames(p) = m@responses@levels[[.model["target"]]]
				return(p)
			} else 
				predict(.model["learner.model"], newdata=.newdata, ...)
			
		}
)