#' @include task.classif.r
#' @include task.regr.r



make.loss <- function(name) {
	if (is.function(name))
		return(name)
	if (name=="squared") 
		fun=function(true.y, pred.y, weights) (true.y - pred.y)^2 
	else if (name=="abs") 
		fun=function(true.y, pred.y, weights) abs(true.y - pred.y) 
	else if (name=="zero-one") 
		fun=function(true.y, pred.y, weights) as.numeric(true.y != pred.y) 
	else 	
		stop(paste("Loss", name, "does not exist!"))
	
	attr(fun, "name") = name
	return(fun)
}

setGeneric(
		name = "default.loss",
		def = function(learn.task) {
			standardGeneric("default.loss")
		}
)

setMethod(
		f = "default.loss",
		signature = c(learn.task="classif.task"),
		def = function(learn.task) {
			return(make.loss("zero-one"))
		}
)

setMethod(
		f = "default.loss",
		signature = c(learn.task="regr.task"),
		def = function(learn.task) {
			return(make.loss("squared"))
		}
)






