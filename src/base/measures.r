make.aggrs = function(xs) {
	if (length(xs)==0)
		return(list())
	ys = list()
	for (i in 1:length(xs)) {
		x = xs[[i]] 
		if (is.function(x))
			y = x
		else if (is.character(x)) {
			if (x == "combine") {
				y = function(...) NA
			}
			else {	
				y = get(x)
				if (!is.function(y)) {
					stop("Aggregation method is not the name of a function: ", x)
				}
			}
			attr(y, "name") = x
		}
		ys[[i]] = y
		nn = names(xs)[i]
		if (is.null(nn))
			nn = attr(y, "name")
		if (is.null(nn))
			stop("No name for aggregation method: ", capture.output(str(y)))
		names(ys)[i] = nn
	}
	return(ys)	
}


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
	if (name %in% c("acc")) 
		x = acc
	else if (name %in% c("mmce")) 
		x = mce
	
	else if (name=="tp") 
		x = tp
	else if (name %in% c("tpr", "hit-rate", "recall")) 
		x = tpr
	else if (name %in% c("fp", "false-alarm")) 
		x = fp
	else if (name %in% c("fpr", "false-alarm-rate", "fall-out")) 
		x = fpr
	else if (name %in% c("tn", "correct-rejection")) 
		x = tn
	else if (name %in% c("tnr", "specificity")) 
		x = tnr
	else if (name %in% c("fn", "miss")) 
		x = fn
	else if (name=="fnr") 
		x = fnr

	else if (name %in% c("ppv", "precision")) 
		x = ppv
	else if (name=="npv") 
		x = npv
	else if (name=="fdr") 
		x = fdr
	else if (name=="mcc") 
		x = mcc
	else if (name=="f1") 
		x = f1
	
	else if (name=="sse") 
		x = sse
	else if (name=="mse") 
		x = mse

	else 
		stop("Requested unknown measure: ", name)
	
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
	return(list("combine", "mean", "sd")) 
}

### classification


acc = function(trues, preds, weights, task.desc, data.desc) {
	mean(as.character(trues) == as.character(preds)) 
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

### binary



tp = function(trues, preds, weights, task.desc, data.desc) {
	sum(trues == preds & preds == task.desc["positive"])  
}
tn = function(trues, preds, weights, task.desc, data.desc) {
	sum(trues == preds & preds == task.desc["negative"])  
}
fp = function(trues, preds, weights, task.desc, data.desc) {
	sum(trues != preds & preds == task.desc["positive"])  
}
fn = function(trues, preds, weights, task.desc, data.desc) {
	sum(trues != preds & preds == task.desc["negative"])  
}




tpr = function(trues, preds, weights, task.desc, data.desc) {
	tp(trues, preds, weights, task.desc, data.desc) / sum(trues == task.desc["positive"])  
}
fpr = function(trues, preds, weights, task.desc, data.desc) {
	fp(trues, preds, weights, task.desc, data.desc) / sum(trues == task.desc["negative"])  
}
tnr = function(trues, preds, weights, task.desc, data.desc) {
	1 - fpr(trues, preds, weights, task.desc, data.desc)  
}
fnr = function(trues, preds, weights, task.desc, data.desc) {
	1 - tpr(trues, preds, weights, task.desc, data.desc)  
}


ppv = function(trues, preds, weights, task.desc, data.desc) {
	tp(trues, preds, weights, task.desc, data.desc) / sum(preds == task.desc["positive"])  
}
npv = function(trues, preds, weights, task.desc, data.desc) {
	tn(trues, preds, weights, task.desc, data.desc) / sum(preds == task.desc["negative"])  
}
fdr = function(trues, preds, weights, task.desc, data.desc) {
	fp(trues, preds, weights, task.desc, data.desc) / sum(preds == task.desc["positive"])  
}
mcc = function(trues, preds, weights, task.desc, data.desc) {
	print(table(trues, preds))
	(tp(trues, preds, weights, task.desc, data.desc) * tn(trues, preds, weights, task.desc, data.desc) -
	fp(trues, preds, weights, task.desc, data.desc) * fn(trues, preds, weights, task.desc, data.desc)) /
	sqrt(prod(table(trues, preds)))
}
f1 = function(trues, preds, weights, task.desc, data.desc) {
	2 * tp(trues, preds, weights, task.desc, data.desc) /
	(sum(trues == task.desc["positive"]) + sum(preds == task.desc["positive"]))  
}

### regression


sse = function(trues, preds, weights, task.desc, data.desc) {
	sum((trues - preds)^2) 
}

mse = function(trues, preds, weights, task.desc, data.desc) {
	mean((trues - preds)^2) 
}

