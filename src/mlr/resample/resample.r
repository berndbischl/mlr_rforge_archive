#' Given a resampling strategy, which defines sets of training and test indices, 
#' fits the selected learner using the training sets and performs predictions for the training/test sets.
#' (This depends on what you selected in the resampling strategy, see parameter \code{predict} in \code{\link{makeResampleDesc}}.)
#' Then performance measures are calculated and aggregated. You are able to return all fitted models (parameter \code{models})
#' or extract specific parts of the models (parameter \code{extract}) as returning all of them completely might be memory intensive.    
#' 
#' For construction of the resampling strategies use the factory methods \code{\link{makeResampleDesc}} and 
#' \code{\link{makeResampleInstance}}.
#'
#' @param learner [\code{\linkS4class{Learner}} | \code{\link{character}}]\cr 
#'   Learning algorithm.   
#' @param task [\code{\linkS4class{LearnTask}}] \cr
#'   Learning task.
#' @param resampling [\code{\linkS4class{ResampleDesc}} or \code{\linkS4class{ResampleInstance}}] \cr
#'   Resampling strategy. If a description is passed, it is instantiated automatically.
#' @param measures [\code{\linkS4class{Measure}} | list of \code{\linkS4class{Measure}}] \cr
#'   Performance measures to evaluate. See \code{\link{measures}}.
#' @param models [logical(1)] \cr 
#'   Should all fitted models be returned? Default is \code{FALSE}. 
#' @param extract [function(model)] \cr 
#'   Function used to extract information from a fitted model during resampling. 
#'   Is applied to every \code{\linkS4class{WrappedModel}} resulting from calls to \code{\link{train}} during resampling.
#'   Default is to extract nothing. 
#' 
#' @return List of \cr
#'   measures.test [\code{data.frame}] Rows correspond to test sets in resampling iterations, columns to performance measures.\cr
#'   measures.train [\code{data.frame}] Rows correspond to training sets in resampling iterations, columns to performance measures.\cr
#'   aggr [named numeric] Vector of aggregated performance values. Names are coded like this <measure>.<aggregation>.\cr
#'   pred [\code{\linkS4class{ResamplePrediction}}] Container for all predictions during resampling.\cr
#'   models [list of \code{\linkS4class{WrappedModel}}] List of fitted models or \code{NULL}.\cr
#'   extract [list] List of extracted parts from fitted models or \code{NULL}.
#' 
#' 
#' @export
#' @rdname resample 
#' 
#' @title Fit models according to a resampling strategy.

setGeneric(
  name = "resample",
  def = function(learner, task, resampling, measures, models, extract) {
    if (is.character(learner))
      learner = makeLearner(learner)
    check.arg(task, "LearnTask")
    if (is(resampling, "ResampleDesc")) 
      resampling = makeResampleInstance(resampling, task=task)
    if (missing(measures))
      measures = default.measures(task)
    if (is(measures, "Measure"))
      measures = list(measures)
    if (missing(models))
      models = FALSE
    if (missing(extract))
      extract = function(m){}
    standardGeneric("resample")
  }
)

#' @export
#' @rdname resample 
setMethod(
  f = "resample",
  signature = signature(learner="Learner", task="LearnTask", resampling="ResampleInstance", measures="list", models="logical", extract="function"),
  def = function(learner, task, resampling, measures, models, extract) {
    n = task["size"]
    r = resampling["size"]
    if (n != r)
      stop(paste("Size of data set:", n, "and resampling instance:", r, "differ!"))
    
    rin = resampling
    iters = rin@desc@iters
    
    if (is(rin, "ResampleInstance.nonseq")) {
      rs = mylapply(1:iters, resample.fit.iter, from="resample", learner=learner, task=task, 
        rin=rin, measures=measures, model=models, extract=extract)
    } else {
      rs  = list()
      stop("Sequential resampling not implemented yet!")
#      # sequential resampling cannot be (easily) parallized!
#      i = 1
#      while (!resample.done(rin)) {
#        train.i = rin["train.inds"][[i]]
#        test.i = rin["test.inds"][[i]]
#        m = train(learner, task, subset=train.i)
#        p = predict(m, task=task, subset=test.i)
#        ex = extract(m)
#        rs[[i]] = list(pred=p, extract=ex)
#        rin = resample.update(rin, task, m, p)
#        i = i + 1
#      }				
    }
    ms.test = lapply(rs, function(x) x$measures.test)
    ms.test = as.data.frame(matrix(Reduce(rbind, ms.test), nrow=iters))
    colnames(ms.test) = sapply(measures, function(pm) pm@id)
    rownames(ms.test) = NULL
    ms.test = cbind(iter=1:iters, ms.test)
    
    ms.train = lapply(rs, function(x) x$measures.train)
    ms.train = as.data.frame(matrix(Reduce(rbind, ms.train), nrow=iters))
    colnames(ms.train) = sapply(measures, function(pm) pm@id)
    rownames(ms.train) = NULL
    ms.train = cbind(iter=1:iters, ms.train)
    
    preds.test = lapply(rs, function(x) x$pred.test)
    preds.train = lapply(rs, function(x) x$pred.train)
    pred = new("ResamplePrediction", instance=rin, preds.test=preds.test, preds.train=preds.train)
    
    aggr = c()
    for (i in 1:length(measures)) {
      m = measures[[i]]
      mid = m@id
      p1 = ms.test[, mid]
      p2 = ms.train[, mid]
      a = sapply(m@aggr, function(a) a@fun(p1, p2, m, rin["group"], pred))
      names(a) = paste(mid, sapply(m@aggr, function(a) a@id), sep=".")
      aggr = c(aggr, a)
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
)

