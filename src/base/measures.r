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


acc = function(x) {
	mean(as.character(x["target"]) == as.character(x["response"])) 
}
mce = function(x) {
	mean(as.character(x["target"]) != as.character(x["response"])) 
}
sme = function(x) {
	sum(as.character(x["target"]) != as.character(x["response"])) 
}

mcesd = function(x) {
	sd(as.character(x["target"]) != as.character(x["response"])) 
}

### binary



tp = function(x) {
	sum(x["target"] == x["response"] & x["response"] == pred@task.desc["positive"])  
}
tn = function(x) {
	sum(x["target"] == x["response"] & x["response"] == pred@task.desc["negative"])  
}
fp = function(x) {
	sum(x["target"] != x["response"] & x["response"] == pred@task.desc["positive"])  
}
fn = function(x) {
	sum(x["target"] != x["response"] & x["response"] == pred@task.desc["negative"])  
}




tpr = function(x) {
	tp(x) / sum(x["target"] == pred@task.desc["positive"])  
}
fpr = function(x) {
	fp(x) / sum(x["target"] == pred@task.desc["negative"])  
}
tnr = function(x) {
	1 - fpr(x)  
}
fnr = function(x) {
	1 - tpr(x)  
}


ppv = function(x) {
	tp(x) / sum(x["response"] == pred@task.desc["positive"])  
}
npv = function(x) {
	tn(x) / sum(x["response"] == pred@task.desc["negative"])  
}
fdr = function(x) {
	fp(x) / sum(x["response"] == pred@task.desc["positive"])  
}
mcc = function(x) {
	print(table(x["target"], x["response"]))
	(tp(x) * tn(x) -
	fp(x) * fn(x)) /
	sqrt(prod(table(x["target"], x["response"])))
}
f1 = function(x) {
	2 * tp(x) /
	(sum(x["target"] == pred@task.desc["positive"]) + sum(x["response"] == pred@task.desc["positive"]))  
}

### regression


sse = function(x) {
	sum((x["target"] - x["response"])^2) 
}

mse = function(x) {
	mean((x["target"] - x["response"])^2) 
}

