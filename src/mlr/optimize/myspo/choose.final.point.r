choose.final.point = function(meta.model, constr.model, fun, curdes, cury, control) {
  #des = opt.meta.model.bfgs(1, meta.model, constr.model, par.set, curdes, cury, control) 
   #seqdes = seq.design(control$par.set, control$seq.des.points*10L, constr.model)
  
  #maybe do something different here for some stragegies? optimize meta model more thoroughly?....
  des = choose.new.points(1, meta.model, constr.model, curdes, cury, control)
  m = meta.model["learner.model"]
  min.i = which.min(cury)
  min.x = curdes[min.i,]
  
  if (control["seq.method"] == "DiceOptim.CL") {   
    km.mean = function(x) {
      x = matrix(x, 1, length(x))
      predict.km(object = m, newdata = x, type="SK")$mean
    } 
    or = optim(fn=km.mean, method="BFGS", par=min.x)
    des = as.data.frame(matrix(or$par, 1, length(or$par)))
    names(des) = names(curdes)
    des
  }
  
  #des = opt.meta.model.bfgs(1, meta.model, constr.model, par.set, curdes, cury, co
  y.meta = eval.des.with.meta.model(des, meta.model)
  repdes = des[rep(1,30),,drop=FALSE]
  y.real = eval.des.with.fun(repdes, fun, control)
  y.real = mean(y.real)
  list(x=data.frame.row.to.list(des, 1), y.meta=y.meta, y.real=y.real, y.diff=y.meta-y.real)
}