
make.varsel.f = function(learner, task, resampling, measures, control) {
  function(p) {
    p2 = as.list(p)
    names(p2) = ns
    es = eval.state.tune(learner, task, resampling, measures, control, p2, "optim")
    perf = get.perf(es)
    logger.info(level="tune", paste(ns, "=", formatC(p, digits=3)), ":", formatC(perf, digits=3))
    ifelse(measures[[1]]["minimize"], 1 , -1) * perf
  }  
}



vars.to.logical = function(vars, all.vars) {
  if (is.list(vars)) {
    y = t(sapply(vars, function(x) all.vars %in% x))
    colnames(y) = all.vars
  } else {
    y = all.vars %in% vars
    names(y) = all.vars
  }
  y
}

vars.to.binary = function(vars, all.vars) {
  y=vars.to.logical(vars, all.vars)
  mode(y) = "integer"
  y
}

logical.to.vars = function(x, all.vars) {
  if (is.matrix(x)) {
    if (missing(all.vars))
      all.vars = colnames(x)
    lapply(1:nrow(x), function(i) all.vars[x[i,]])
  } else {
    if (missing(all.vars))
      all.vars = names(x)
    all.vars[x]
  }
}

binary.to.vars = function(x, all.vars) {
  mode(x) = "logical"
  logical.to.vars(x, all.vars)  
}


