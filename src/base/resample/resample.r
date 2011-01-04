#' Given a resampling strategy, which defines sets of training and test indices, 
#' fits the selected learner using the training sets and performs predictions for the test sets. 
#' For construction of the resampling strategies use the factory methods \code{\link{make.res.desc}} and 
#' \code{\link{make.res.instance}}.
#' 
#' Optionally either the complete, fitted models or - to save memory - extracted parts from the models
#' can be returned.
#'
#' @param learner [\code{\linkS4class{learner}} or \code{\link{character}}]\cr 
#'        Learning algorithm.   
#' @param task [\code{\linkS4class{learn.task}}] \cr
#'        Learning task.
#' @param resampling [\code{\linkS4class{resample.desc}} or \code{\linkS4class{resample.instance}}] \cr
#'        Resampling strategy. 
#' @param extract [\code{\link{function}}] \cr 
#'       Function used to extract information from fitted models, e.g. can be used to save the complete list of fitted models. 
#'        Default is to extract nothing. 
#' @return \code{\linkS4class{resample.prediction}}.
#' 
#' @export
#' @rdname resample 
#' 
#' @title Fit models according to a resampling strategy.

setGeneric(
  name = "resample",
  def = function(learner, task, resampling, measures, models, extract) {
    if (is.character(learner))
      learner = make.learner(learner)
    if (is(resampling, "resample.desc")) 
      resampling = make.res.instance(resampling, task=task)
    if (missing(measures))
      measures = default.measures(task)
    if (is(measures, "measure"))
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
  signature = signature(learner="learner", task="learn.task", resampling="resample.instance", measures="list", models="logical", extract="function"),
  def = function(learner, task, resampling, measures, models, extract) {
    n = task["size"]
    r = resampling["size"]
    if (n != r)
      stop(paste("Size of data set:", n, "and resampling instance:", r, "differ!"))
    
    rin = resampling
    iters = rin["iters"]
    
    if (is(rin, "resample.instance.nonseq")) {
      rs = mylapply(1:iters, resample.fit.iter, from="resample", learner=learner, task=task, 
        rin=rin, measures=measures, model=models, extract=extract)
    } else {
      rs  = list()
      # sequential resampling cannot be (easily) parallized!
      i = 1
      while (!resample.done(rin)) {
        train.i = rin["train.inds"][[i]]
        test.i = rin["test.inds"][[i]]
        m = train(learner, task, subset=train.i)
        p = predict(m, task=task, subset=test.i)
        ex = extract(m)
        rs[[i]] = list(pred=p, extract=ex)
        rin = resample.update(rin, task, m, p)
        i = i + 1
      }				
    }
    ms.test = lapply(rs, function(x) x$measures.test)
    ms.test = as.data.frame(matrix(Reduce(rbind, ms.test), nrow=iters))
    colnames(ms.test) = sapply(measures, function(pm) pm["id"])
    rownames(ms.test) = NULL
    ms.test = cbind(iter=1:iters, ms.test)
    
    ms.train = lapply(rs, function(x) x$measures.train)
    ms.train = as.data.frame(matrix(Reduce(rbind, ms.train), nrow=iters))
    colnames(ms.train) = sapply(measures, function(pm) pm["id"])
    rownames(ms.train) = NULL
    ms.train = cbind(iter=1:iters, ms.train)
    
    preds.test = lapply(rs, function(x) x$pred.test)
    preds.train = lapply(rs, function(x) x$pred.train)
    pred = new("resample.prediction", instance=rin, preds.test=preds.test, preds.train=preds.train)
    
    aggr = c()
    for (i in 1:length(measures)) {
      m = measures[[i]]
      mid = m["id"]
      p1 = ms.test[, mid]
      p2 = ms.train[, mid]
      a = sapply(m@aggr, function(a) a@fun(p1, p2, m, rin["group"], pred))
      names(a) = paste(mid, sapply(m@aggr, function(a) a["id"]), sep=".")
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

