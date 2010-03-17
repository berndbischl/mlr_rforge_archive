make.measures = function(xs) {
	if (length(xs)==0)
		return(list())
	ys = list()
	for (i in 1:length(xs)) {
		x = xs[[i]] 
		if (is.function(x))
			y = x
		else if (is.character(x))
			y =make.measure(x)
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


make.measure <- function(name) {
	if (name=="mmce") 
		x = mce
	else if (name=="smce") 
		x = sme
	else if (name=="sse") 
		x = sse
	else if (name=="mse") 
		x = mse
	else if (name=="rmse") 
		x = rmse
	attr(x, "name") = name
	return(x)
}


setGeneric(
		name = "default.measures",
		def = function(learn.task) {
			standardGeneric("default.measures")
		}
)

setMethod(
		f = "default.measures",
		signature = c(learn.task="classif.task"),
		def = function(learn.task) {
			return(c("mmce"))
		}
)

setMethod(
		f = "default.measures",
		signature = c(learn.task="regr.task"),
		def = function(learn.task) {
			return(c("mse"))
		}
)


default.aggr = function(task) {
	return(list(mean=mean, sd=sd)) 
}

mce = function(trues, preds, weights, task) {
	mean(as.character(trues) != as.character(preds)) 
}

sme = function(trues, preds, weights, task) {
	sum(as.character(trues) != as.character(preds)) 
}

mcesd = function(trues, preds, weights, task) {
	sd(as.character(trues) != as.character(preds)) 
}




rmse = function(trues, preds, weights, task) {
	sqrt(mean((trues - preds)^2)) 
}

sse = function(trues, preds, weights, task) {
	sum((trues - preds)^2) 
}

mse = function(trues, preds, weights, task) {
	mean((trues - preds)^2) 
}

