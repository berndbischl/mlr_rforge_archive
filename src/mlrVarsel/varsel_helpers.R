#todo: log.fun in eval.states! otherwise it does not always get called.

#make.varsel.f = function(learner, task, resampling, measures, par.set, control, log.fun) {
#  function(x) {
#    p2 = bits.to.features(x, task)
#    y = eval.rf(learner, task, resampling, measures, par.set, control, p2) 
#    log.fun(learner, task, resampling, measures, par.set, control, opt.path, p2, y)
#    ifelse(measures[[1]]@minimize, 1 , -1) * y[1]
#  }  
#}
#
#
log.fun.varsel = function(learner, task, resampling, measures, par.set, control, opt.path, x, y) {
  logger.info(level="opt", paste(length(x), " features : ", formatC(y, digits=3)))
  #logger.info(level="varsel", paste("varsel: forward=",forward, " features=", length(state$par), " perf=", round(get.perf(state), 3), " feat=", changed, sep=""))      
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


