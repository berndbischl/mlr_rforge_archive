opt.meta.model.seq.des = function(n, meta.model, constr.model, curdes, cury, control) {
  seqdes = seq.design(par.descs, control$seq.des.points, constr.model)
  y = eval.des.with.meta.model(seqdes, meta.model)
  o = order(y)
  seqdes[o[1:n],]
}

opt.meta.model.bfgs = function(n, meta.model, constr.model, curdes, cury, control) {
  inds.num = which(sapply(par.descs, function(x) is(x, "par.desc.double")))
  inds.rest = (1:length(par.descs))[-inds.num] 
  names.num = sapply(par.descs, function(x) x["par.name"])[inds.num]
  names.rest = sapply(par.descs, function(x) x["par.name"])[inds.rest]
  lower = unlist(get.bounds(par.descs, "lower"))
  upper = unlist(get.bounds(par.descs, "upper"))
  disc.ranges = get.ranges(par.descs)
  disc.grid = grid=expand.grid(disc.ranges, KEEP.OUT.ATTRS=FALSE)
  
  opts = list()
  y = numeric(0)

  g = function(x, disc.vals) {
    nd = as.list(x)
    disc.vals = lapply(seq(length=length(disc.vals)), function(k) factor(disc.vals[[k]], levels=disc.ranges[[k]]))
    names(disc.vals) = names.rest
    nd = c(nd, disc.vals)
    predict(meta.model, newdata=as.data.frame(nd))["response"]
  }
 
  bfgs = function(disc.vals) {
    #start = unlist(sel.random(par.descs, "par.desc.double"))
    start = unlist(curdes[which.min(cury), names.num])
    g1 = function(x) g(x, disc.vals=disc.vals)
    or = optim(fn=g1, par=start, method="L-BFGS-B")
    x = or$par
    y <<- c(y, or$value)
    cat("BFGS: ", start, "--->", x, " at ", y, " with evals ", or$count, "\n")
    opts[[length(opts)+1]] <<- as.data.frame(c(as.list(or$par), disc.vals))
  }
  
  for (j in 1:n) {
    if (length(inds.rest) > 0) 
      disc.vals = sel.random(par.descs, c("par.desc.log", "par.desc.disc"))
    else      
      disc.vals = list()
    bfgs(disc.vals)
  }
  
  opts = Reduce(rbind, opts)
  o = order(y)
  opts[o[1:n], ]
}


opt.meta.model.CL = function(n, meta.model, constr.model, curdes, cury, control) {
  par.descs = control$par.descs
  inds.num = which(sapply(par.descs, function(x) is(x, "par.desc.double")))
  inds.rest = (1:length(par.descs))[-inds.num] 
  names.num = sapply(par.descs, function(x) x["par.name"])[inds.num]
  names.rest = sapply(par.descs, function(x) x["par.name"])[inds.rest]
  lower = unlist(get.bounds(par.descs, "lower"))
  upper = unlist(get.bounds(par.descs, "upper"))
  disc.ranges = get.ranges(par.descs)
  disc.grid = grid=expand.grid(disc.ranges, KEEP.OUT.ATTRS=FALSE)
  model = meta.model["learner.model"]
  
  L = min(model@y)
  capture.output({res = max_qEI.CL(model, npoints = n, L = L, lower = lower, upper = upper)})
  as.data.frame(res$par)
}

sel.random = function(par.descs, parclass) {
  z = list()
  for (i in seq(length=length(par.descs))) {
    pd = par.descs[[i]]
    if (as.character(class(pd)) %in% parclass)
      z[[pd["par.name"]]] = sample.pardesc(1L, pd)
  }
  return(z)
}



get.bounds = function(par.descs, bound) {
  z = list()
  for (i in seq(length=length(par.descs))) {
    pd = par.descs[[i]]
    if (is(pd, "par.desc.double"))
      z[[pd["par.name"]]] = pd[bound]
  }
  return(z)
}  
  
get.ranges = function(par.descs) {
  z = list()
  for (i in seq(length=length(par.descs))) {
    pd = par.descs[[i]]
    if (is(pd, "par.desc.disc"))
      z[[pd["par.name"]]] = names(pd["vals"])
    if (is(pd, "par.desc.log"))
      z[[pd["par.name"]]] = c("TRUE", "FALSE")
  }
  return(z)
}  


