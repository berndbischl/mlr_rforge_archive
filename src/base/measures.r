#todo regr.binary extra
#costs
#'
#' A performance measure is evaluated after a single train/predict step and returns a single number to assess the quality
#' of the prediction (or maybe only the model, think AIC).
#' The measure itself knows whether it wants to be minimized or maximized and for what tasks it is applicable.
#' See below for a list of already implemented measures. 
#' If you want a measure for a misclassification cost matrix, look at \code{\link{make.cost.measure}}.
#' If you want to implement your own measure, look at \code{\link{make.measure}}. 
#' 
#' Classification (only mmce and acc can be used for multiclass problems): 
#' \itemize{ 
#' 		\item{\bold{mmce}}{\cr Mean misclassification error.}
#' 		\item{\bold{acc}}{\cr Accuracy.}
#' 		\item{\bold{tp}}{\cr True positives.}
#' 		\item{\bold{tpr}}{\cr True positive rate, also called hit rate or recall.}
#' 		\item{\bold{fp}}{\cr False positives, also called false alarms.}
#' 		\item{\bold{fpr}}{\cr False positive rate, also called false alarm rate or fall-out.}
#' 		\item{\bold{tn}}{\cr True negatives, also called correct rejections.}
#' 		\item{\bold{tnr}}{\cr True negative rate. Also called specificity.}
#' 		\item{\bold{fn}}{\cr False negatives, also called misses.}
#' 		\item{\bold{fnr}}{\cr False negative rate.}
#' 		\item{\bold{ppv}}{\cr Positive predictive value, also called precision.}
#' 		\item{\bold{npv}}{\cr Negative predictive value.}
#' 		\item{\bold{fdr}}{\cr False discovery rate.}
#' 		\item{\bold{f1}}{\cr F1 measure.}
#' 		\item{\bold{mcc}}{\cr Matthews correlation coefficient.}
#' 		\item{\bold{gmean}}{\cr G-mean, geomentric mean of recall and specificity.}
#' 		\item{\bold{gpr}}{\cr Geometric mean of precision and recall.}
#' 		\item{\bold{auc}}{\cr Area under the curve.}
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
#' }
#' 
#' General:
#' \itemize{ 
#' 		\item{\bold{time.fit}}{\cr Time of fitting the model}
#' 		\item{\bold{time.predict}}{\cr Time of predicting test set}
#' 		\item{\bold{time}}{\cr time.train + train.predict}
#' }
#'  
#' @title Performance measures.
measures = function() {}

#general
nvars = make.measure(id="nvars", minimize=TRUE,  
  fun=function(task, model, pred, extra.pars) {
    length(model["vars"])          
  }
)

time.fit = make.measure(id="time.train", minimize=TRUE, 
  fun=function(task, model, pred, extra.pars) {
    model["time"]
  }
)

time.predict = make.measure(id="time.predict", minimize=TRUE, 
  fun=function(task, model, pred, extra.pars) {
    pred["time"]
  }  
)

time.all = make.measure(id="time.all", minimize=TRUE, 
  fun=function(task, model, pred, extra.pars) {
    model["time"] + pred["time"]           
  }  
)

### regression ###

sse = make.measure(id="sse", minimize=TRUE, req.task.type="regr",
  fun=function(task, model, pred, extra.pars) {
    sum((pred["response"] - pred["truth"])^2)          
  }
)
mse = make.measure(id="mse", minimize=TRUE, req.task.type="regr",  
  fun=function(task, model, pred, extra.pars) {
    mean((pred["response"] - pred["truth"])^2)          
  }
)

medse = make.measure(id="medse", minimize=TRUE, req.task.type="regr", 
  fun=function(task, model, pred, extra.pars) {
    median((pred["response"] - pred["truth"])^2)          
  }
)
sae = make.measure(id="sae", minimize=TRUE, req.task.type="regr", 
  fun=function(task, model, pred, extra.pars) {
    sum(abs(pred["response"] - pred["truth"]))          
  }
)
mae = make.measure(id="mae", minimize=TRUE, req.task.type="regr", 
  fun=function(task, model, pred, extra.pars) {
    mean(abs(pred["response"] - pred["truth"]))          
  }
)
medae = make.measure(id="medae", minimize=TRUE, req.task.type="regr", 
  fun=function(task, model, pred, extra.pars) {
    median(abs(pred["response"] - pred["truth"]))          
  }
)


# classif_multi
mmce = make.measure(id="mmce", minimize=TRUE, req.task.type="classif",  
  fun=function(task, model, pred, extra.pars) {
    mean(pred["response"] != pred["truth"])          
  }
)

acc = make.measure(id="acc", minimize=FALSE, req.task.type="classif",  
  fun=function(task, model, pred, extra.pars) {
    mean(pred["response"] == pred["truth"])          
  }
)



# classif_two

auc = make.measure(id="auc", minimize=FALSE, req.task.type="binary" ,req.pred.type="prob",  
  fun=function(task, model, pred, extra.pars) {
    # ROCR does not work with NAs
    if (any(is.na(pred["response"])) || length(unique(pred["truth"])) == 1)
      return(as.numeric(NA))
    
    rpreds = as.ROCR.preds(pred)
    ROCR.performance(rpreds, "auc")@y.values[[1]]
  }  
)

tp = make.measure(id="tp", minimize=FALSE, req.task.type="classif", req.binary=TRUE,  
  fun=function(task, model, pred, extra.pars) {
    sum(pred["truth"] == pred["response"] & pred["response"] == pred@task.desc["positive"])  
  }
)

tn = make.measure(id="tn", minimize=FALSE, req.task.type="classif", req.binary=TRUE,  
  fun=function(task, model, pred, extra.pars) {
    sum(pred["truth"] == pred["response"] & pred["response"] == x@task.desc["negative"])  
  }
)

fp = make.measure(id="fp", minimize=TRUE, req.task.type="classif", req.binary=TRUE,  
  fun=function(task, model, pred, extra.pars) {
    sum(pred["truth"] != pred["response"] & pred["response"] == x@task.desc["positive"])  
  }
)

fn = make.measure(id="fn", minimize=TRUE, req.task.type="classif", req.binary=TRUE,  
  fun=function(task, model, pred, extra.pars) {
    sum(pred["truth"] != pred["response"] & pred["response"] == x@task.desc["negative"])  
  }
)


tpr = make.measure(id="tpr", minimize=FALSE, req.task.type="classif", req.binary=TRUE,  
  fun=function(task, model, pred, extra.pars) {
    tp@fun(task, model, pred, extra.pars) / 
      sum(pred["truth"] == pred@task.desc["positive"])    
  }
)

tnr = make.measure(id="tnr", minimize=FALSE, req.task.type="classif", req.binary=TRUE,  
  fun=function(task, model, pred, extra.pars) {
    tn@fun(task, model, pred, extra.pars) / 
      sum(pred["truth"] == pred@task.desc["negative"])  
  }
)

fpr = make.measure(id="fpr", minimize=TRUE, req.task.type="classif", req.binary=TRUE,  
  fun=function(task, model, pred, extra.pars) {
    fp@fun(task, model, pred, extra.pars) / 
      sum(pred["truth"] == pred@task.desc["negative"])  
  }
)

fnr = make.measure(id="fnr", minimize=TRUE, req.task.type="classif", req.binary=TRUE,  
  fun=function(task, model, pred, extra.pars) {
    fn@fun(task, model, pred, extra.pars) / 
      sum(pred["truth"] == pred@task.desc["positive"])  
  }
)

ppv = make.measure(id="ppv", minimize=FALSE, req.task.type="classif", req.binary=TRUE,  
  fun=function(task, model, pred, extra.pars) {
    tp@fun(task, model, pred, extra.pars) / 
      sum(x["response"] == x@task.desc["positive"])  
  }
)

npv = make.measure(id="npv", minimize=FALSE, req.task.type="classif", req.binary=TRUE,  
  fun=function(task, model, pred, extra.pars) {
    tn@fun(task, model, pred, extra.pars) / 
      sum(x["response"] == x@task.desc["negative"])  
  }
)

fdr = make.measure(id="fdr", minimize=TRUE, req.task.type="classif", req.binary=TRUE,  
  fun=function(task, model, pred, extra.pars) {
    fp@fun(task, model, pred, extra.pars) / 
      sum(x["response"] == x@task.desc["positive"])  
  }
)

mcc = make.measure(id="mcc", minimize=FALSE, req.task.type="classif", req.binary=TRUE,  
  fun=function(task, model, pred, extra.pars) {
    (tp@fun(task, model, pred, extra.pars) * 
    tn@fun(task, model, pred, extra.pars) - 
    fp@fun(task, model, pred, extra.pars) * 
    fn@fun(task, model, pred, extra.pars)) /  
    sqrt(prod(table(x["truth"], x["response"])))
  }
)

f1 = make.measure(id="f1", minimize=FALSE, req.task.type="classif", req.binary=TRUE,  
  fun=function(task, model, pred, extra.pars) {
    2*tp@fun(task, model, pred, extra.pars) / 
    (sum(x["truth"] == x@task.desc["positive"]) + sum(x["response"] == x@task.desc["positive"])) 
  }
)

gmean = make.measure(id="gmean", minimize=FALSE, req.task.type="classif", req.binary=TRUE,  
  fun=function(task, model, pred, extra.pars) {
    sqrt(tpr@fun(task, model, pred, extra.pars) * 
         tnr@fun(task, model, pred, extra.pars)) 
  }
)

gpr = make.measure(id="gpr", minimize=FALSE, req.task.type="classif", req.binary=TRUE,  
  fun=function(task, model, pred, extra.pars) {
    sqrt(ppv@fun(task, model, pred, extra.pars) * 
         tpr@fun(task, model, pred, extra.pars)) 
  }
)

