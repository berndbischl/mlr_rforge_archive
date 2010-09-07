choose.final.point = function(meta.model, constr.model, fun, curdes, cury, control) {
  des = opt.meta.model.bfgs(1, meta.model, constr.model, par.descs, curdes, cury, control) 
   #seqdes = seq.design(control$par.descs, control$seq.des.points*10L, constr.model)
  y.meta = eval.des.with.meta.model(des, meta.model)
  repdes = des[rep(1,30), ]
  y.real = eval.des.with.fun(repdes, fun, control)
  y.real = mean(y.real)
  list(x=data.frame.row.to.list(des, 1), y.meta=y.meta, y.real=y.real, y.diff=y.meta-y.real)
}