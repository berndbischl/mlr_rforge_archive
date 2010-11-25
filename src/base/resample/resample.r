#todo remove par.vals? 


setGeneric(
  name = "resample",
  def = function(learner, task, resampling, vars, measures, predictions, models, extract) {
    if (is.character(learner))
      learner = make.learner(learner)
    if (is(resampling, "resample.desc")) 
      resampling = make.res.instance(resampling, task=task)
    if (missing(vars))
      vars = task["input.names"]
    if (length(vars) == 0)
      vars = character(0)
    if (missing(measures))
      measures = default.measures(task)
    if (missing(predictions))
      predictions = FALSE
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
  signature = signature(learner="learner", task="learn.task", resampling="resample.instance", vars="character", measures="list", 
    predictions = "logical", models="logical", extract="list"),
  def = function(learner, task, resampling, vars, measures, predictions, models, extract) {
    n = task["size"]
    r = resampling["size"]
    if (n != r)
      stop(paste("Size of data set:", n, "and resampling instance:", r, "differ!"))
    
    rin = resampling
    iters = rin["iters"]
    
    if (is(rin, "resample.instance.nonseq")) {
      rs = mylapply(1:iters, resample.fit.iter, from="resample", learner=learner, task=task, 
        rin=rin, vars=vars, measures=measures, prediction=predictions, model=models, extract=extract)
    } else {
      rs  = list()
      # sequential resampling cannot be (easily) parallized!
      i = 1
      while (!resample.done(rin)) {
        train.i = get.train.set(rin, i)
        ts = get.test.set(rin, i)
        test.i = ts$inds
        g = ts$group
        m = train(learner, task, subset=train.i, vars=vars)
        p = predict(m, task=task, subset=test.i, group=g)
        ex = extract(m)
        rs[[i]] = list(pred=p, extracted=ex)
        rin = resample.update(rin, task, m, p)
        i = i + 1
      }				
    }
    ms = lapply(rs, function(x) x$measures)
    ms = as.data.frame(matrix(Reduce(rbind, ms), nrow=iters))
    colnames(ms) = sapply(measures, function(pm) pm["id"])
    rownames(ms) = NULL
    ms = cbind(iter=1:iters, ms)
    preds.test = lapply(rs, function(x) x$pred.test)
    preds.train = lapply(rs, function(x) x$pred.train)
    list(
      measures = ms,
      pred = if (predictions) new("resample.prediction", instance=rin, preds.test=preds.test, preds.train=preds.train) else NULL,
      models = if(models) lapply(rs, function(x) x$model) else NULL, 
      extract = if(is.function(extract)) lapply(rs, function(x) x$extracted) else NULL
    )
  }
)

