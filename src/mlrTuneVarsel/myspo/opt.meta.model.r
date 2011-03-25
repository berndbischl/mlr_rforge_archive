opt.meta.model.seq.des = function(n, meta.model, constr.model, curdes, cury, control) {
  seqdes = seq.design(par.set, control$seq.des.points, constr.model)
  y = eval.des.with.meta.model(seqdes, meta.model)
  o = order(y)
  seqdes[o[1:n],]
}

opt.meta.model.bfgs = function(n, meta.model, constr.model, curdes, cury, control) {
  inds.num = which(sapply(par.set, function(x) is(x, "Parameter.double")))
  inds.rest = (1:length(par.set))[-inds.num] 
  names.num = sapply(par.set, function(x) x["par.name"])[inds.num]
  names.rest = sapply(par.set, function(x) x["par.name"])[inds.rest]
  lower = unlist(get.bounds(par.set, "lower"))
  upper = unlist(get.bounds(par.set, "upper"))
  disc.ranges = get.ranges(par.set)
  disc.grid = grid=expand.grid(disc.ranges, KEEP.OUT.ATTRS=FALSE)
  
  opts = list()
  y = numeric(0)

  g = function(x, disc.vals) {
    nd = as.list(x)
    disc.vals = lapply(seq(length=length(disc.vals)), function(k) factor(disc.vals[[k]], levels=disc.ranges[[k]]))
    names(disc.vals) = names.rest
    nd = c(nd, disc.vals)
    predict(meta.model, newdata=as.data.frame(nd))@df$response
  }
 
  bfgs = function(disc.vals) {
    #start = unlist(sel.random(par.set, "Parameter.double"))
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
      disc.vals = sel.random(par.set, c("Parameter.log", "Parameter.disc"))
    else      
      disc.vals = list()
    bfgs(disc.vals)
  }
  
  opts = Reduce(rbind, opts)
  o = order(y)
  opts[o[1:n], ]
}


opt.meta.model.CL = function(n, meta.model, constr.model, curdes, cury, control) {
  par.set = control$par.set
  inds.num = which(sapply(par.set, function(x) is(x, "Parameter.double")))
  inds.rest = (1:length(par.set))[-inds.num] 
  names.num = sapply(par.set, function(x) x["par.name"])[inds.num]
  names.rest = sapply(par.set, function(x) x["par.name"])[inds.rest]
  lower = unlist(get.bounds(par.set, "lower"))
  upper = unlist(get.bounds(par.set, "upper"))
  disc.ranges = get.ranges(par.set)
  disc.grid = grid=expand.grid(disc.ranges, KEEP.OUT.ATTRS=FALSE)
  model = meta.model@learner.model
  
  L = min(model@y)
  capture.output({res = max_qEI.CL(model, npoints = n, L = L, lower = lower, upper = upper)})
  as.data.frame(res$par)
}

sel.random = function(par.set, parclass) {
  z = list()
  for (i in seq(length=length(par.set))) {
    pd = par.set[[i]]
    if (as.character(class(pd)) %in% parclass)
      z[[pd["par.name"]]] = sample.pardesc(1L, pd)
  }
  return(z)
}



#get.bounds = function(par.set, bound) {
#  z = list()
#  for (i in seq(length=length(par.set))) {
#    pd = par.set[[i]]
#    if (is(pd, "Parameter.double"))
#      z[[pd["par.name"]]] = pd[bound]
#  }
#  return(z)
#}  
  
get.ranges = function(par.set) {
  z = list()
  for (i in seq(length=length(par.set))) {
    pd = par.set[[i]]
    if (is(pd, "Parameter.disc"))
      z[[pd["par.name"]]] = names(pd["vals"])
    if (is(pd, "Parameter.log"))
      z[[pd["par.name"]]] = c("TRUE", "FALSE")
  }
  return(z)
}  


