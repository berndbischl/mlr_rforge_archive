opt.meta.model.seq.des = function(n, meta.model, constr.model, par.descs, control) {
  seqdes = seq.design(par.descs, control$seq.des.points, constr.model)
  y = eval.des.with.meta.model(seqdes, meta.model)
  o = order(y)
  seqdes[o[1:n],]
}

opt.meta.model.bfgs = function(n, meta.model, constr.model, par.descs, control) {
  inds.num = which(sapply(par.descs, function(x) is(x, "par.desc.num")))
  inds.rest = (1:length(par.descs))[-inds.num] 
  names.num = sapply(par.descs, function(x) x["par.name"])[inds.num]
  lower = unlist(get.bounds(par.descs, "lower"))
  upper = unlist(get.bounds(par.descs, "upper"))
  opts = list()
  y = numeric(0)
  g = function(x) {
    nd = as.data.frame(x)
    colnames(nd) = num
    nd = cbind(nd, disc.vals)
    predict(meta.model, newdata=as.data.frame(x)
  }
  
  j = 0 
  while(j < 3) {
    start = unlist(sel.random.all.nums(par.descs))
    or = optim(g, start=start, method="L-BFGS-B")
    x = or$par
    y = c(y, or$val)
    opts = rbind(opts, cbind(or$par, disc.vals))
    j = j+1
  }
  
  opts = Reduce(rbind, opts)
  o = order(y)
  opts[o[1:n], ]
}


sel.random = function(par.descs, parclass) {
  z = list()
  for (i in seq(length=length(par.descs))) {
    pd = par.descs[[i]]
    if (is(pd, parclass))
      z[[pd["par.name"]]] = sample.pardesc(1, pd)
  }
  return(z)
}



get.bounds = function(par.descs, bound) {
  z = list()
  for (i in seq(length=length(par.descs))) {
    pd = par.descs[[i]]
    if (is(pd, "par.desc.num"))
      z[[pd["par.name"]]] = pd[bound]
  }
  return(z)
}  
  
 