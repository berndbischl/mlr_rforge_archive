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
		"classif.randomForest", 
		contains = c("rlearner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("classif.randomForest"),
		def = function(.Object) {
			
			desc = c(
					oneclass = FALSE,
					twoclass = TRUE,
					multiclass = TRUE,
					missings = FALSE,
					doubles = TRUE,
					factors = TRUE,
					prob = TRUE,
					decision = FALSE,
					weights = FALSE,
					costs = FALSE
			)
			
			par.descs = list(
					numeric.learner.parameter(name="ntree", default=500L, lower=1L),
					numeric.learner.parameter(name="mtry", lower=1),
					logical.learner.parameter(name="replace", default=TRUE),
					numeric.learner.parameter(name="sampsize", lower=1L),
					numeric.learner.parameter(name="nodesize", default=1L, lower=1L),
					numeric.learner.parameter(name="maxnodes", lower=1L),
        
          logical.learner.parameter(name="importance", default=FALSE, flags=list(optimize=FALSE)),
          logical.learner.parameter(name="localImp", default=FALSE, flags=list(optimize=FALSE)),
          logical.learner.parameter(name="norm.votes", default=TRUE, flags=list(optimize=FALSE)),
          logical.learner.parameter(name="keep.inbag", default=FALSE, flags=list(optimize=FALSE)),
          numeric.learner.parameter(name="maxnodes", lower=1L)
			)

      callNextMethod(.Object, pack="randomForest", desc=desc, par.descs=par.descs)
		}
)


#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="classif.randomForest", 
				.task="classif.task", .subset="integer" 
		),
		
		def = function(.learner, .task, .subset,  ...) {
			f = .task["formula"]
			randomForest(f, data=get.data(.task, .subset), ...)
		}
)

#' @rdname pred.learner

setMethod(
		f = "pred.learner",
		signature = signature(
				.learner = "classif.randomForest", 
				.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			.type <- ifelse(.type=="response", "response", "prob")
			predict(.model["learner.model"], newdata=.newdata, type=.type, ...)
		}
)	









