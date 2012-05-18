#' Fit models according to a resampling strategy.
#' 
#' Given a resampling strategy, which defines sets of training and test indices, 
#' fits the selected learner using the training sets and performs predictions for the training/test sets.
#' (This depends on what you selected in the resampling strategy, see parameter \code{predict} in \code{\link{makeResampleDesc}}.)
#' Then performance measures are calculated and aggregated. You are able to return all fitted models (parameter \code{models})
#' or extract specific parts of the models (parameter \code{extract}) as returning all of them completely might be memory intensive.    
#' 
#' For construction of the resampling strategies use the factory methods \code{\link{makeResampleDesc}} and 
#' \code{\link{makeResampleInstance}}.
#'
#' 
#' @param learner [\code{\link{Learner}}]\cr 
#'   Learning algorithm.   
#' @param task [\code{\link{LearnTask}}]\cr
#'   Learning task.
#' @param resampling [\code{\link{ResampleDesc}} or \code{\link{ResampleInstance}}]\cr
#'   Resampling strategy. If a description is passed, it is instantiated automatically.
#' @param measures [\code{\link{Measure}} | list of \code{\link{Measure}}]\cr
#'   Performance measures to evaluate. See \code{\link{measures}}.
#' @param models [logical(1)]\cr 
#'   Should all fitted models be returned? 
#'   Default is \code{FALSE}. 
#' @param extract [function(model)]\cr 
#'   Function used to extract information from a fitted model during resampling. 
#'   Is applied to every \code{\link{WrappedModel}} resulting from calls to \code{\link{train}} during resampling.
#'   Default is to extract nothing. 
#' @param show.info [logical(1)]\cr 
#'   Should a few informative lines about the current resampling iteration and the result be 
#'   logged to the R console? 
#'   Default is \code{TRUE}. 
#' @return List of:
#'   \item{measures.test [\code{data.frame}]}{Rows correspond to test sets in resampling iterations, columns to performance measures.}
#'   \item{measures.train [\code{data.frame}]}{Rows correspond to training sets in resampling iterations, columns to performance measures.}
#'   \item{aggr [named numeric]}{Vector of aggregated performance values. Names are coded like this <measure>.<aggregation>.}
#'   \item{pred [\code{\link{ResamplePrediction}}]}{Container for all predictions during resampling.}
#'   \item{models [list of \code{\link{WrappedModel}}]}{List of fitted models or \code{NULL}.}
#'   \item{extract [list]}{List of extracted parts from fitted models or \code{NULL}.}
#' @export
resample = function(learner, task, resampling, measures, models=FALSE, 
  extract=function(m){}, show.info=TRUE) {

  checkArg(learner, "Learner")
  checkArg(task, "LearnTask")
  # instantiate resampling
  if (is(resampling, "ResampleDesc")) 
    resampling = makeResampleInstance(resampling, task=task)
  checkArg(resampling, "ResampleInstance")
  if (missing(measures))
    measures = default.measures(task)
  if (is(measures, "Measure"))
    measures = list(measures)
  checkListElementClass(measures, "Measure")  
  checkArg(models, "logical", len=1, na.ok=FALSE)
  checkArg(extract, "function")
  checkArg(show.info, "logical", len=1, na.ok=FALSE)

  n = task@desc@size
  r = resampling@size
  if (n != r)
    stop(paste("Size of data set:", n, "and resampling instance:", r, "differ!"))
  
  rin = resampling
  iters = rin@desc@iters
  
  iter.results = mylapply(1:iters, resample.fit.iter, from="resample", learner=learner, task=task, 
    rin=rin, measures=measures, model=models, extract=extract, show.info=show.info)
  
  combineResampleResult(iter.results, measures, rin, models, show.info)
  
}

combineResampleResult = function(iter.results, measures, rin, models, show.info) {
  iters = length(iter.results)
  mids = sapply(measures, function(m) m@id)
  
  ms.test = lapply(iter.results, function(x) x$measures.test)
  ms.test = as.data.frame(matrix(Reduce(rbind, ms.test), nrow=iters))
  colnames(ms.test) = sapply(measures, function(pm) pm@id)
  rownames(ms.test) = NULL
  ms.test = cbind(iter=1:iters, ms.test)
  
  ms.train = extractSubList(iter.results, "measures.train")
  ms.train = as.data.frame(do.call(rbind, ms.train))
  colnames(ms.train) = mids
  rownames(ms.train) = NULL
  ms.train = cbind(iter=1:iters, ms.train)
  
  preds.test = extractSubList(iter.results, "pred.test")
  preds.train = extractSubList(iter.results, "pred.train")
  pred = new("ResamplePrediction", instance=rin, preds.test=preds.test, preds.train=preds.train)
  
  aggr = sapply(measures, function(m)  m@aggr@fun(task, ms.test[, m@id], ms.train[, m@id], m, rin@group, pred))
  names(aggr) = sapply(measures, measureAggrName)
  if (show.info) {
    messagef("[Resample] Result: %s", perfsToString(aggr))  
  }
  list(
    measures.train = ms.train,
    measures.test = ms.test,
    aggr = aggr,
    pred = pred,
    models = if(models) lapply(iter.results, function(x) x$model) else NULL, 
    extract = if(is.function(extract)) lapply(iter.results, function(x) x$extract) else NULL
  )
}  

