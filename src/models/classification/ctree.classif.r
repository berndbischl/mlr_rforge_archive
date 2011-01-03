#' @include learnerR.r
roxygen()
#' @include wrapped.model.r
roxygen()
#' @include train.learner.r
roxygen()
#' @include pred.learner.r
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
			callNextMethod(.Object, pack="party", desc=desc)
		}
)

#' @rdname train.learner


setMethod(
		f = "train.learner",
		signature = signature(
				.learner="classif.ctree", 
				.task="classif.task", .subset="integer", .vars="character" 
		),
		
		def = function(.learner, .task, .subset, .vars,  ...) {
			ns = c("teststat", "testtype", "mincriterion", "minsplit", "minbucket", "stump", 
					"nresample", "maxsurrogate", "mtry", "savesplitstats", "maxdepth")
			xs = args.to.control(ctree_control, ns, list(...))
			f = .task["formula"]
			args = c(list(f, data=get.data(.task, .subset, .vars), control=xs$control), xs$args)
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