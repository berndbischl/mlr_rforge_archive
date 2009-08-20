#' @include learner.props.r
roxygen()



setClass(
		"wrapped.learner.classif",
		contains = c("wrapped.learner"),
		representation = representation(
				train.par.for.classes = "list",
				train.par.for.probs = "list",
				predict.par.for.classes = "list",
				predict.par.for.probs = "list",
				trafo.for.classes = "function",
				trafo.for.probs   = "function",
				dummy.classes = "logical"
		)
)

setMethod(
		f = "initialize",
		signature = signature("wrapped.learner.classif"),
		def = function(
				.Object, 
				learner.name, 
				learner.pack, 
				learner.model.class, 
				learner.model.S4,
				train.fct, 
				train.fct.pars=list(), 
				predict.fct=predict, predict.fct.pars=list(), 
				learner.props,				
				train.par.for.classes = list(),
				train.par.for.probs = list(),
				predict.par.for.classes = list(type="class"),
				predict.par.for.probs = list(type="prob"),
				trafo.for.classes = "default",
				trafo.for.probs   = "default",
				dummy.classes = FALSE
		) {
			
			if (missing(learner.name))
				return(.Object)
			
			if (is.character(trafo.for.classes) && trafo.for.classes == "default") {
				trafo.for.classes <- function(x, model) {
					if (is.factor(x)) {
						return(x)
					} else if (is.list(x) && "class" %in% names(x)) {
						return(as.factor(x$class))
					}      
					logger.error("unkown return structure in predict fct! pls define predict.fct.trafo yourself!")
				}
			}
			if (is.character(trafo.for.probs) && trafo.for.probs == "default") {
				trafo.for.probs <- function(x, model) {
					if (is.matrix(x)) {
						return(x)
					} else if (is.list(x) && "posterior" %in% names(x)) {
						return(x$posterior)
					}      
					logger.error("unkown return structure in predict fct! pls define predict.fct.trafo yourself!")
				}
			}
			
			
			
			.Object@train.par.for.classes <- train.par.for.classes
			.Object@train.par.for.probs <- train.par.for.probs
			.Object@predict.par.for.classes <- predict.par.for.classes
			.Object@predict.par.for.probs <- predict.par.for.probs
			
			.Object@trafo.for.classes <- trafo.for.classes
			.Object@trafo.for.probs <- trafo.for.probs
			
			.Object@dummy.classes <- dummy.classes
			callNextMethod(.Object, 
					learner.name = learner.name, 
					learner.pack = learner.pack, 
					learner.model.class = leraner.model.class, 
					learner.model.S4,
					train.fct = train.fct,
					train.fct.pars = train.fct.pars,
					predict.fct = predict.fct,
					predict.fct.pars = predict.fct.pars,
					learner.props=learner.props
			)
		}
)