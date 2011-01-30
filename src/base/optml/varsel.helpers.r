
make.varsel.f = function(learner, task, resampling, measures, par.set, control, log.fun) {
  function(p) {
    if (is.integer(p))
      p2 = binary.to.vars(p)
    y = eval.rf(learner, task, resampling, measures, par.set, control, p2) 
    log.fun(learner, task, resampling, measures, par.set, control, opt.path, p2, y)
    ifelse(measures[[1]]["minimize"], 1 , -1) * y[1]
  }  
}


log.fun.varsel = function(learner, task, resampling, measures, par.set, control, opt.path, x, y) {
  logger.info(level="opt", paste(length(x), " : ", formatC(perf, digits=3)))
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


