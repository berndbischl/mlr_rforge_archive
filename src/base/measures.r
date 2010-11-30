#todo regr.binary extra

# A performance measure transforms a vector of predictions (compared to the true responses) into a single numerical value. 
#
#' Performance measures can always be passed as a single string (name of a single measure), a character vector of multiple names of measures or 
#' a list containing string names of measures and your on performance measures as function objects. The latter ones should 
#' be named list elements.\cr  
#' 
#' Classification: 
#' \itemize{ 
#' 		\item{\bold{mmce}}{\cr Mean misclassification error}
#' 		\item{\bold{acc}}{\cr Accuracy}
#' 		\item{\bold{costs}}{\cr Misclassification costs according to cost matrix}
#' 		\item{\bold{tp}}{\cr True positives}
#' 		\item{\bold{tpr, hit-rate, recall}}{\cr True positive rate}
#' 		\item{\bold{fp, false-alarm}}{\cr False positives}
#' 		\item{\bold{fpr, false-alarm-rate, fall-out}}{\cr False positive rate}
#' 		\item{\bold{tn, correct-rejection}}{\cr True negatives}
#' 		\item{\bold{tnr, specificity}}{\cr True negative rate}
#' 		\item{\bold{fn, miss}}{\cr False negatives}
#' 		\item{\bold{fnr}}{\cr False negative rate}
#' 		\item{\bold{ppv, precision}}{\cr Positive predictive value}
#' 		\item{\bold{npv}}{\cr Negative predictive value}
#' 		\item{\bold{fdr}}{\cr False discovery rate}
#' 		\item{\bold{f1}}{\cr F1 measure}
#' 		\item{\bold{mcc}}{\cr Matthews correlation coefficient}
#' 		\item{\bold{gmean}}{\cr G-mean, geomentric mean of recall and specificity.}
#' 		\item{\bold{gpr}}{\cr Geometric mean of precision and recall.}
#' 		\item{\bold{auc}}{\cr Area under the curve.}
#' 
#' 		\item{\bold{time.train}}{\cr Time of fitting the model}
#' 		\item{\bold{time.predict}}{\cr Time of predicting test set}
#' 		\item{\bold{time}}{\cr time.train + train.predict}
#' }
#' 
#' Regression:
#' \itemize{ 
#' 		\item{\bold{sse}}{\cr Sum of squared errors}
#' 		\item{\bold{mse}}{\cr Mean of squared errors}
#' 		\item{\bold{medse}}{\cr Median of squared errors}
#' 		\item{\bold{sae}}{\cr Sum of absolute errors}
#' 		\item{\bold{mae}}{\cr Mean of absolute errors}
#' 		\item{\bold{medae}}{\cr Median of absolute errors}
#' 
#' 		\item{\bold{time.train}}{\cr Time of fitting the model}
#' 		\item{\bold{time.predict}}{\cr Time of predicting test set}
#' 		\item{\bold{time}}{\cr time.train + train.predict}
#' }
#'  
#' @title Performance measures.
measures = function() {}


mmce = make.measure(id="mmce", minimize=TRUE, req.task.type="classif",  
  fun=function(task, model, pred.test, pred.train, pars) {
    mean(pred.test["response"] != pred.test["truth"])          
  }
)

mse = make.measure(id="mse", minimize=TRUE, req.task.type="regr",  
  fun=function(task, model, pred.test, pred.train, pars) {
    mean((pred.test["response"] - pred.test["truth"])^2)          
  }
)


time.train = make.measure(id="time.train", minimize=TRUE, 
  fun=function(task, model, pred.test, pred.train, pars) {
    model["time"]
  }
)

time.predict = make.measure(id="time.predict", minimize=TRUE, 
  fun=function(task, model, pred.test, pred.train, pars) {
    pred.test["time"]
  }  
)

nvars = make.measure(id="nvars", minimize=TRUE,  
  fun=function(task, model, pred.test, pred.train, pars) {
    length(model["vars"])          
  }
)


auc = make.measure(id="auc", minimize=FALSE, req.task.type="binary" ,req.pred.type="prob",  
  fun=function(task, model, pred.test, pred.train, pars) {
    # ROCR does not work with NAs
    if (any(is.na(pred.test["response"])) || length(unique(pred.test["truth"])) == 1)
  		return(as.numeric(NA))
    
  	rpreds = as.ROCR.preds(pred.test)
  	ROCR.performance(rpreds, "auc")@y.values[[1]]
  }  
)



### classification

#
#acc = make.measure(id="acc", minimize=FALSE, req.task.type="classif",  
#  fun=function(pred.test, pred.train, model, task, pars) {
#	  mean(as.character(x["truth"]) == as.character(x["response"]))
#  }
#)
#
#mmce = make.measure(id="mmce", minimize=TRUE, req.task.type="classif",  
#  fun=function(pred.test, pred.train, model, task, pars) {
#    mean(pred.test["response"] != pred.test["truth"])          
#  }
#)
#
#
#
#
##
##sme = function(x, task) {
##	sum(as.character(x["truth"]) != as.character(x["response"])) 
##}
##
##mcesd = function(x, task) {
##	sd(as.character(x["truth"]) != as.character(x["response"])) 
##}
##
##cost.measure = function(x, task, costs=task["costs"]) {
##	if (all(dim(costs) == 0))
##		stop("No costs were defined in task!")
##  # cannot index with NA
##  if (any(is.na(x["response"])))
##    return(as.numeric(NA))
##	cc = function(truth, pred) {
##		costs[truth, pred]
##	}
##	m = Reduce(sum, Map(cc, as.character(x["truth"]), as.character(x["response"])))
##	return(m)
##}
##
##make.cost.measure = function(task, costs) {
##	#todo checks
##	force(costs)
##	function(x, task) cost.measure(x, task, costs=costs)
##}
##
##### binary
#
#
##
##
##
##tp = function(x, task) {
##	sum(x["truth"] == x["response"] & x["response"] == x@task.desc["positive"])  
##}
##tn = function(x, task) {
##	sum(x["truth"] == x["response"] & x["response"] == x@task.desc["negative"])  
##}
##fp = function(x, task) {
##	sum(x["truth"] != x["response"] & x["response"] == x@task.desc["positive"])  
##}
##fn = function(x, task) {
##	sum(x["truth"] != x["response"] & x["response"] == x@task.desc["negative"])  
##}
##
##
##
##
##tpr = function(x, task) {
##	tp(x) / sum(x["truth"] == x@task.desc["positive"])  
##}
##fpr = function(x, task) {
##	fp(x) / sum(x["truth"] == x@task.desc["negative"])  
##}
##tnr = function(x, task) {
##	1 - fpr(x)  
##}
##fnr = function(x, task) {
##	1 - tpr(x)  
##}
##
##
##ppv = function(x, task) {
##	tp(x) / sum(x["response"] == x@task.desc["positive"])  
##}
##npv = function(x, task) {
##	tn(x) / sum(x["response"] == x@task.desc["negative"])  
##}
##fdr = function(x, task) {
##	fp(x) / sum(x["response"] == x@task.desc["positive"])  
##}
##mcc = function(x, task) {
##	(tp(x) * tn(x) -
##	fp(x) * fn(x)) /
##	sqrt(prod(table(x["truth"], x["response"])))
##}
##f1 = function(x, task) {
##	2 * tp(x) /
##	(sum(x["truth"] == x@task.desc["positive"]) + sum(x["response"] == x@task.desc["positive"]))  
##}
##gmean = function(x, task) {
##	sqrt(tpr(x)* tnr(x))
##}
##
##gpr = function(x, task) {
##	sqrt(ppv(x) * tpr(x))
##}
##
##auc = function(x, task) {
##	# ROCR does not work with NAs
##  # if we have only 
##  if (any(is.na(x["response"])) || length(unique(x["truth"])) == 1)
##		return(as.numeric(NA))
##	rpreds = as.ROCR.preds(x)
##	ROCR.performance(rpreds, "auc")@y.values[[1]]
##}
##
##
#
##### regression
#
#sse = make.measure(id="sse", minimize=TRUE, req.task.type="regr",
#  fun=function(pred.test, pred.train, model, task, pars) {
#    sum((pred.test["response"] - pred.test["truth"])^2)          
#  }
#)
#medse = make.measure(id="medse", minimize=TRUE, req.task.type="regr", 
#  fun=function(pred.test, pred.train, model, task, pars) {
#    median((pred.test["response"] - pred.test["truth"])^2)          
#  }
#)
#sae = make.measure(id="sae", minimize=TRUE, req.task.type="regr", 
#  fun=function(pred.test, pred.train, model, task, pars) {
#    sum(abs(pred.test["response"] - pred.test["truth"]))          
#  }
#)
#mae = make.measure(id="mae", minimize=TRUE, req.task.type="regr", 
#  fun=function(pred.test, pred.train, model, task, pars) {
#    mean(abs(pred.test["response"] - pred.test["truth"]))          
#  }
#)
#medae = make.measure(id="medae", minimize=TRUE, req.task.type="regr", 
#  fun=function(pred.test, pred.train, model, task, pars) {
#    median(abs(pred.test["response"] - pred.test["truth"]))          
#  }
#)
#
##### time
#
#
#
#time.all = make.measure(id="time.all", minimize=TRUE, 
#  fun=function(pred.test, pred.train, model, task, pars) {
#    model["time"] + pred.test["time"]           
#  }  
#)
