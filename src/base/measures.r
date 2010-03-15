


make.measure <- function(name) {
	if (name=="mce") 
		x = mce
	else if (name=="rmse") 
		x = rmse
	else if (name=="sme") 
		x = sme
	else if (name=="sd") 
		x = mcesd
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
			return(c("mce", "sd"))
		}
)

setMethod(
		f = "default.measures",
		signature = c(learn.task="regr.task"),
		def = function(learn.task) {
			return(c("rmse"))
		}
)


mce = function(trues, preds, weights, task) {
	mean(as.character(trues) != as.character(preds)) 
}

mcesd = function(trues, preds, weights, task) {
	sd(as.character(trues) != as.character(preds)) 
}

sme = function(trues, preds, weights, task) {
	sum(as.character(trues) != as.character(preds)) 
}



rmse = function(trues, preds, weights, task) {
	sqrt((trues - preds)^2) 
}



