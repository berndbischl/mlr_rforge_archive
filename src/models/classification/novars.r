#' @include wrapped.learner.classif.r
roxygen()

setClass(
		"novars.classif", 
		contains = c("wrapped.learner.classif")
)


#----------------- constructor ---------------------------------------------------------
setMethod(
		f = "initialize",
		signature = signature("novars.classif"),
		def = function(.Object) {
			
			desc = new("classif.props",
					supports.multiclass = TRUE,
					supports.missing = TRUE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = TRUE,
					supports.probs = TRUE,
					supports.weights = TRUE
			)
			
			callNextMethod(.Object, learner.name="NoVars", learner.pack="mlr", learner.props=desc)
		}
)


setMethod(
		f = "train.learner",
		signature = signature(
				.wrapped.learner="novars.classif", 
				.targetvar="character", 
				.data="data.frame", 
				.weights="numeric", 
				.costs="matrix", 
				.type = "character" 
		),
		
		def = function(.wrapped.learner, .targetvar, .data, .weights, .costs, .type,  ...) {
			list(targets=.data[, .targetvar])
		}
)

setMethod(
		f = "predict.learner",
		signature = signature(
				.wrapped.learner = "novars.classif", 
				.wrapped.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.wrapped.learner, .wrapped.model, .newdata, .type, ...) {
			m <- wrapped.model["learner.model"]
			if(.type=="class")
				as.factor(sample(m@targets, nrow(.newdata)))	
			else
				as.numeric(table(m@targets)) / length(m@targets)
		}
)	







