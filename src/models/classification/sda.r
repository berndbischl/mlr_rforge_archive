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
		"classif.sda", 
		contains = c("rlearner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("classif.sda"),
		def = function(.Object) {
			
			desc = c(
					oneclass = FALSE,
					twoclass = TRUE,
					multiclass = TRUE,
					missings = FALSE,
					doubles = TRUE,
					factors = FALSE,
					prob = TRUE,
					decision = FALSE,
					weights = FALSE,			
					costs = FALSE
			)
			
      par.descs = list (
        new("par.desc.log", par.name="diagonal", default=FALSE)
      )
      
      
			callNextMethod(.Object, pack="sda", desc=desc, par.descs=par.descs)
		}
)

#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="classif.sda", 
				.task="classif.task", .subset="integer" 
		),
		
		def = function(.learner, .task, .subset,  ...) {
			d = get.data(.task, .subset, target.extra=TRUE)
			sda(Xtrain = as.matrix(d$data), L = d$target, ...)
		}
)

#' @rdname pred.learner

setMethod(
		f = "pred.learner",
		signature = signature(
				.learner = "classif.sda", 
				.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			p = predict(.model["learner.model"], as.matrix(.newdata))
			if(.type == "response")
				return(p$class)
			else
				return(p$posterior)
		}
)



