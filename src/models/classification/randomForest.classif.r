#' @include learnerR.r
roxygen()
#' @include wrapped.model.r
roxygen()
#' @include train.learner.r
roxygen()
#' @include pred.learner.r
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
					new("par.desc.num", par.name="ntree", default=500L, lower=1L),
					new("par.desc.num", par.name="mtry", lower=1),
					new("par.desc.log", par.name="replace", default=TRUE),
					new("par.desc.num", par.name="sampsize", lower=1L),
					new("par.desc.num", par.name="nodesize", default=1L, lower=1L),
					new("par.desc.num", par.name="maxnodes", lower=1L),
        
          new("par.desc.log", par.name="importance", default=FALSE, flags=list(optimize=FALSE)),
          new("par.desc.log", par.name="localImp", default=FALSE, flags=list(optimize=FALSE)),
          new("par.desc.log", par.name="norm.votes", default=TRUE, flags=list(optimize=FALSE)),
          new("par.desc.log", par.name="keep.inbag", default=FALSE, flags=list(optimize=FALSE)),
          new("par.desc.num", par.name="maxnodes", lower=1L)
			)

      callNextMethod(.Object, pack="randomForest", desc=desc, par.descs=par.descs)
		}
)


#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="classif.randomForest", 
				.task="classif.task", .subset="integer", .vars="character" 
		),
		
		def = function(.learner, .task, .subset, .vars,  ...) {
			f = as.formula(paste(.task["target"], "~."))
			randomForest(f, data=.task["data"][.subset, .vars], ...)
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









