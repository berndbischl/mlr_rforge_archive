
#' @exportClass filter.wrapper
setClass(
		"filter.wrapper",
		contains = c("base.wrapper"),
		representation = representation(
				vars = "character"
		)
)

#' Constructor.

setMethod(
		f = "initialize",
		signature = signature("filter.wrapper"),
		def = function(.Object, learner, vars) {
			.Object@vars = vars
			callNextMethod(.Object, learner)
		}
)


#' @export
make.filter.wrapper = function(learner, vars) {
	new("filter.wrapper", learner=learner, vars=vars)
}



#' @export
setMethod(
		f = "train.learner",
		signature = signature(
				.learner="filter.wrapper", 
				.targetvar="character", 
				.data="data.frame", 
				.data.desc="data.desc", 
				.task.desc="task.desc", 
				.weights="numeric", 
				.costs="matrix" 
		),
		
		def = function(.learner, .targetvar, .data, .data.desc, .task.desc, .weights, .costs,  ...) {	
			vars = .learner@vars
			vars = c(vars, .targetvar)
			.data = .data[, vars, drop=F]
			callNextMethod(.learner, .targetvar, .data, .data.desc, .task.desc, .weights, .costs,  ...)
		}
)

