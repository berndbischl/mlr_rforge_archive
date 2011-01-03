#' @include learnerR.r
roxygen()
#' @include wrapped.model.r
roxygen()
#' @include train.learner.r
roxygen()
#' @include pred.learner.r
roxygen()


setClass(
		"classif.svm", 
		contains = c("rlearner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("classif.svm"),
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
        new("par.desc.disc", par.name="type", default="C-classification", vals=c("C-classification", "nu-classification")),
        new("par.desc.double", par.name="cost",  default=1, lower=0, requires=expression(type=="C-classification")),
        new("par.desc.double", par.name="nu", default=0.5, requires=expression(type=="nu-classification")),
        new("par.desc.disc", par.name="kernel", default="radial", vals=c("linear", "polynomial", "radial", "sigmoid")),
        new("par.desc.double", par.name="degree", default=3L, lower=1L, requires=expression(kernel=="polynomial")),
        new("par.desc.double", par.name="coef0", default=0, requires=expression(kernel=="polynomial" || kernel=="sigmoid")),
        new("par.desc.double", par.name="gamma",  default=3L, lower=1L, requires=expression(kernel!="linear")),
        new("par.desc.double", par.name="tolerance", default=0.001, lower=0),
        new("par.desc.log", par.name="shrinking", default=TRUE),
        new("par.desc.log", par.name="probability", default=FALSE, flags=list(optimize=FALSE)),
        new("par.desc.double", par.name="cachesize", default=40L, flags=list(optimize=FALSE))
      )
      
			callNextMethod(.Object, pack="e1071", desc=desc, par.descs=par.descs)
		}
)

#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="classif.svm", 
				.task="classif.task", .subset="integer", .vars="character" 
		),
		
		def = function(.learner, .task, .subset, .vars,  ...) {
			f = .task["formula"]
			svm(f, data=.task["data"][.subset, .vars], ...)
		}
)

#' @rdname pred.learner

setMethod(
		f = "pred.learner",
		signature = signature(
				.learner = "classif.svm", 
				.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			if(.type=="response") {
				p = predict(.model["learner.model"], newdata=.newdata, ...)
			} else {
				p = predict(.model["learner.model"], newdata=.newdata, probability=TRUE, ...)
				p = attr(p, "probabilities")
			}
			return(p)
		}
)	


