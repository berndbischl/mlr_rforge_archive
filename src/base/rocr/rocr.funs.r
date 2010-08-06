
ROCR.performance = function(prediction.obj, measure, x.measure = "cutoff", ...) {
	f = get("performance", envir=getNamespace("ROCR"))
	f(prediction.obj, measure, x.measure, ...)	
} 
	
