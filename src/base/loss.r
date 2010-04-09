#' @include task.classif.r
#' @include task.regr.r



make.losses = function(xs) {
	if (length(xs)==0)
		return(list())
	ys = list()
	for (i in 1:length(xs)) {
		x = xs[[i]] 
		if (is.function(x))
			y = x
		else if (is.character(x))
			y =make.loss(x)
		ys[[i]] = y
		nn = names(xs)[i]
		if (is.null(nn))
			nn = attr(y, "name")
		if (is.null(nn))
			stop("No name for measure!")
		names(ys)[i] = nn
	}
	return(ys)	
}


make.loss <- function(name) {
	if (name=="squared") 
		fun=function(pred, task) (pred["truth"] - pred["response"])^2 
	else if (name=="abs") 
		fun=function(pred, task) abs(pred["truth"] - pred["response"]) 
	else if (name=="zero-one") 
		fun=function(pred, task) as.numeric(pred["truth"] != pred["response"]) 
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
			return(list())
			#return(make.loss("zero-one"))
		}
)

setMethod(
		f = "default.loss",
		signature = c(learn.task="regr.task"),
		def = function(learn.task) {
			return(list())
			#return(make.loss("squared"))
		}
)






