#todo remove par.vals? 


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
      extract = list()
    standardGeneric("resample")
  }
)

#' @export
#' @rdname resample 
setMethod(
  f = "resample",
  signature = signature(learner="learner", task="learn.task", resampling="resample.instance", measures="list", models="logical", extract="list"),
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
        train.i = get.train.set(rin, i)
        ts = get.test.set(rin, i)
        test.i = ts$inds
        m = train(learner, task, subset=train.i)
        p = predict(m, task=task, subset=test.i)
        ex = extract(m)
        rs[[i]] = list(pred=p, extracted=ex)
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
      a = sapply(m@aggr, function(a) a@fun(p1, p2, rin["group"], pred))
      names(a) = paste(mid, sapply(m@aggr, function(a) a["id"]))
      aggr = c(aggr, a)
    }
    
    list(
      measures.train = ms.train,
      measures.test = ms.test,
      aggr = aggr,
      pred = pred,
      models = if(models) lapply(rs, function(x) x$model) else NULL, 
      extract = if(is.function(extract)) lapply(rs, function(x) x$extracted) else NULL
    )
  }
)

