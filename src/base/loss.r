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
			nn = attr(y, "id")
		if (is.null(nn))
			stop("No name for loss!")
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
	
	attr(fun, "id") = name
	return(fun)
}


default.loss = function() {
	return(list())
}





