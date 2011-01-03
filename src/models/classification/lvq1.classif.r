#' @include learnerR.r
roxygen()
#' @include wrapped.model.r
roxygen()
#' @include train.learner.r
roxygen()
#' @include pred.learner.r
roxygen()


setClass(
		"classif.lvq1", 
		contains = c("rlearner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("classif.lvq1"),
		def = function(.Object) {
			
			desc = new("learner.desc.classif",
					oneclass = FALSE,
					twoclass = TRUE,
					multiclass = TRUE,
					missings = FALSE,
					numerics = TRUE,
					factors = TRUE,
					probs = FALSE,
					decision = FALSE,
					weights = FALSE,
					costs = FALSE
			)
			
			callNextMethod(.Object, pack="class", desc=desc)
		}
)

#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="classif.lvq1", 
				.task="classif.task", .subset="integer", .vars="character" 
		),
		
		def = function(.learner, .task, .subset, .vars,  ...) {
			inputs = setdiff(colnames(task["data"][.subset, .vars]), .targetvar)
			
			cdbk.args = insert(list(), list(...), c("size", "k", "prior"))
			cdbk.args$x = task["data"][.subset, .vars][,inputs] 
			cdbk.args$cl = task["data"][.subset, .vars][,.targetvar] 
			codebk = do.call(lvqinit, cdbk.args)  

			lvq.args = insert(list(), list(...), c("niter", "alpha"))
			lvq.args$x = task["data"][.subset, .vars][,inputs] 
			lvq.args$cl = task["data"][.subset, .vars][,.targetvar] 
			lvq.args$codebk = codebk 
			do.call(lvq1, lvq.args)  
		}
)

#' @rdname pred.learner

setMethod(
		f = "pred.learner",
		signature = signature(
				.learner = "classif.lvq1", 
				.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			lvqtest(.model["learner.model"], test=.newdata, ...)
		}
)	






