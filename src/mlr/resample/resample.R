#' Given a resampling strategy, which defines sets of training and test indices, 
#' fits the selected learner using the training sets and performs predictions for the training/test sets.
#' (This depends on what you selected in the resampling strategy, see parameter \code{predict} in \code{\link{makeResampleDesc}}.)
#' Then performance measures are calculated and aggregated. You are able to return all fitted models (parameter \code{models})
#' or extract specific parts of the models (parameter \code{extract}) as returning all of them completely might be memory intensive.    
#' 
#' For construction of the resampling strategies use the factory methods \code{\link{makeResampleDesc}} and 
#' \code{\link{makeResampleInstance}}.
#'
#' @title Fit models according to a resampling strategy.
#' @param learner [\code{\linkS4class{Learner}}]\cr 
#'   Learning algorithm.   
#' @param task [\code{\linkS4class{LearnTask}}]\cr
#'   Learning task.
#' @param resampling [\code{\linkS4class{ResampleDesc}} or \code{\linkS4class{ResampleInstance}}]\cr
#'   Resampling strategy. If a description is passed, it is instantiated automatically.
#' @param measures [\code{\linkS4class{Measure}} | list of \code{\linkS4class{Measure}}]\cr
#'   Performance measures to evaluate. See \code{\link{measures}}.
#' @param models [logical(1)]\cr 
#'   Should all fitted models be returned? 
#'   Default is \code{FALSE}. 
#' @param extract [function(model)]\cr 
#'   Function used to extract information from a fitted model during resampling. 
#'   Is applied to every \code{\linkS4class{WrappedModel}} resulting from calls to \code{\link{train}} during resampling.
#'   Default is to extract nothing. 
#' @param show.info [logical(1)]\cr 
#'   Should a few informative lines about the current resampling iteration and the result be 
#'   logged to the R console? 
#'   Default is \code{TRUE}. 
#' @return List of:
#'   \item{measures.test [\code{data.frame}]}{Rows correspond to test sets in resampling iterations, columns to performance measures.}
#'   \item{measures.train [\code{data.frame}]}{Rows correspond to training sets in resampling iterations, columns to performance measures.}
#'   \item{aggr [named numeric]}{Vector of aggregated performance values. Names are coded like this <measure>.<aggregation>.}
#'   \item{pred [\code{\linkS4class{ResamplePrediction}}]}{Container for all predictions during resampling.}
#'   \item{models [list of \code{\linkS4class{WrappedModel}}]}{List of fitted models or \code{NULL}.}
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
  mids = sapply(measures, function(m) m@id)
  
  rs = mylapply(1:iters, resample.fit.iter, from="resample", learner=learner, task=task, 
    rin=rin, measures=measures, model=models, extract=extract, show.info=show.info)
  ms.test = lapply(rs, function(x) x$measures.test)
  ms.test = as.data.frame(matrix(Reduce(rbind, ms.test), nrow=iters))
  colnames(ms.test) = sapply(measures, function(pm) pm@id)
  rownames(ms.test) = NULL
  ms.test = cbind(iter=1:iters, ms.test)
  
  ms.train = lapply(rs, function(x) x$measures.train)
  ms.train = as.data.frame(matrix(Reduce(rbind, ms.train), nrow=iters))
  colnames(ms.train) = mids
  rownames(ms.train) = NULL
  ms.train = cbind(iter=1:iters, ms.train)
  
  preds.test = lapply(rs, function(x) x$pred.test)
  preds.train = lapply(rs, function(x) x$pred.train)
  pred = new("ResamplePrediction", instance=rin, preds.test=preds.test, preds.train=preds.train)
  
  aggr = sapply(measures, function(m)  m@aggr@fun(task, ms.test[, m@id], ms.train[, m@id], m, rin@group, pred))
  names(aggr) = sapply(measures, measureAggrName)
  if (show.info) {
    logger.info("[Resample] Result:", perfsToString(aggr))  
  }
  list(
    measures.train = ms.train,
    measures.test = ms.test,
    aggr = aggr,
    pred = pred,
    models = if(models) lapply(rs, function(x) x$model) else NULL, 
    extract = if(is.function(extract)) lapply(rs, function(x) x$extract) else NULL
  )
}

