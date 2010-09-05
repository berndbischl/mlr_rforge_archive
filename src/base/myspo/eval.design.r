eval.des.with.learner = function(des, learner, task, resampling,  measures, aggr, control) {
  print(str(des))
  ys = numeric(nrow(des))
  for (i in 1:nrow(des)) {
    pv = data.frame.row.to.list(des[i, ])
    on.learner = .mlr.local$errorhandler.setup$on.learner
    errorhandler.setup(on.learner="quiet")
    p = resample.fit(learner, task, resampling, par.vals=pv)
    errorhandler.setup(on.learner=on.learner)
    perf = performance(p, measures=measures, aggr=aggr)
    y = perf$aggr[1,1]
    # if maximize we simply multiply y with -1
    ys[i] = y * ifelse(control["minimize"], 1, -1)
  }
  return(ys)
}

eval.des.with.meta.model = function(des, meta.model, control) {
  pred.meta.model(meta.model, des)
}