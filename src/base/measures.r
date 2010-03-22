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
	else if (name=="tpr") 
		x = tpr
	else if (name=="fpr") 
		x = fpr
	
	
	else if (name=="sse") 
		x = sse
	else if (name=="mse") 
		x = mse
	attr(x, "name") = name
	return(x)
}


setGeneric(
		name = "default.measures",
		def = function(x) {
			standardGeneric("default.measures")
		}
)


setMethod(
		f = "default.measures",
		signature = c(x="task.desc"),
		def = function(x) {
			if (x@task.class == "classif.task")
				return(make.measures("mmce"))
			else 
				return(make.measures("mse"))
		}
)


setMethod(
		f = "default.measures",
		signature = c(x="learn.task"),
		def = function(x) {
			default.measures(x@task.desc)
		}
)


default.aggr = function(task) {
	return(list(mean=mean, sd=sd)) 
}

mce = function(trues, preds, weights, task.desc, data.desc) {
	mean(as.character(trues) != as.character(preds)) 
}

sme = function(trues, preds, weights, task.desc, data.desc) {
	sum(as.character(trues) != as.character(preds)) 
}

mcesd = function(trues, preds, weights, task.desc, data.desc) {
	sd(as.character(trues) != as.character(preds)) 
}

tpr = function(trues, preds, weights, task.desc, data.desc) {
	sum(trues == preds & trues == task.desc["positive"]) / sum(trues == task.desc["positive"])  
}

fpr = function(trues, preds, weights, task.desc, data.desc) {
	sum(trues != preds & trues == task.desc["negative"]) / sum(trues == task.desc["negative"])  
}




sse = function(trues, preds, weights, task.desc, data.desc) {
	sum((trues - preds)^2) 
}

mse = function(trues, preds, weights, task.desc, data.desc) {
	mean((trues - preds)^2) 
}

