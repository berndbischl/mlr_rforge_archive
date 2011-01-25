#' @include learnerR.r
roxygen()
#' @include task.regr.r
roxygen()

setClass(
		"regr.blackboost", 
		contains = c("rlearner.regr")
)


setMethod(
		f = "initialize",
		signature = signature("regr.blackboost"),
		def = function(.Object) {
			
			desc = c(
					missings = FALSE,
					doubles = TRUE,
					factors = TRUE,
					weights = TRUE
			)
      par.descs = list(
        discrete.learner.parameter(id="family", default="Gaussian", vals=list(Gaussian=Gaussian(), Huber=Huber(), Laplace=Laplace())),
        integer.learner.parameter(id="mstop", default=100L, lower=1L),
        numeric.learner.parameter(id="nu", default=0.1, lower=0, upper=1),
        discrete.learner.parameter(id="teststat", default="quad", vals=c("quad", "max")),
        discrete.learner.parameter(id="testtype", default="Bonferroni", vals=c("Bonferroni", "MonteCarlo", "Univariate", "Teststatistic")),
        numeric.learner.parameter(id="mincriterion", default=0.95, lower=0, upper=1),
        integer.learner.parameter(id="minsplit", default=20L, lower=1L),
        integer.learner.parameter(id="minbucket", default=7L, lower=1L),
        logical.learner.parameter(id="stump", default=FALSE),
        integer.learner.parameter(id="nresample", default=9999L, lower=1L, requires=expression(testtype=="MonteCarlo")),
        integer.learner.parameter(id="maxsurrogate", default=0L, lower=0L),
        integer.learner.parameter(id="mtry", default=0L, lower=0L),
        logical.learner.parameter(id="savesplitstats", default=TRUE),
        integer.learner.parameter(id="maxdepth", default=0L, lower=0L)
      )
			callNextMethod(.Object, pack=c("mboost", "party"), desc=desc, par.descs=par.descs)
		}
)

#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="regr.blackboost", 
				.task="regr.task", .subset="integer" 
		),
		
		def = function(.learner, .task, .subset, ...) {
			xs = args.to.control(boost_control, c("mstop", "nu", "risk"), list(...))
			ys = args.to.control(ctree_control, c("teststat", "testtype", "mincriterion", "maxdepth"), xs$args)
			f = .task["formula"]
      args = c(list(f, data=get.data(.task, .subset), control=xs$control, tree_control=ys$control), ys$args)
      if (.task["has.weights"])
        args$weights = .task["weights"][.subset] 
			do.call(blackboost, args)
		}
)

#' @rdname pred.learner

setMethod(
		f = "pred.learner",
		signature = signature(
				.learner = "regr.blackboost", 
				.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "missing" 
		),
		
		def = function(.learner, .model, .newdata, ...) {
			predict(.model["learner.model"], newdata=.newdata, ...)
		}
)	





