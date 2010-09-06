choose.final.point = function(meta.model, constr.model, learner, task, resampling, measures, aggr, control) {
  seqdes = seq.design(control@par.descs, control@seq.des.points*10L, constr.model)
  y.meta = eval.des.with.meta.model(seqdes, meta.model)
  j = which.min(y.meta)
  x = data.frame.row.to.list(seqdes, j)
  y.meta = y.meta[j]
  repdes = seqdes[rep(j,30), ]
  y.real = eval.des.with.learner(repdes, learner, task, resampling, measures, aggr, control)
  y.real = mean(y.real)
  list(x=x, y.meta=y.meta, y.real=y.real, y.diff=y.meta-y.real)
}