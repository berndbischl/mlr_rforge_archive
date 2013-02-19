logFunSelFeatures = function(learner, task, resampling, measures, control, opt.path, x, y) {
  i = ifelse(getOptPathLength(opt.path) == 0, 1, max(opt.path$env$dob) + 1)
  messagef("[selectFeatures] %i: %i bits: %s", i, sum(x), mlr:::perfsToString(y))
}

featuresToLogical = function(vars, all.vars) {
  if (is.list(vars)) {
    y = t(sapply(vars, function(x) all.vars %in% x))
    colnames(y) = all.vars
  } else {
    y = all.vars %in% vars
    names(y) = all.vars
  }
  y
}

featuresToBinary = function(vars, all.vars) {
  y=featuresToLogical(vars, all.vars)
  mode(y) = "integer"
  y
}

logicalToFeatures = function(x, all.vars) {
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

binaryToFeatures = function(x, all.vars) {
  mode(x) = "logical"
  logicalToFeatures(x, all.vars)  
}

compare.diff = function(state1, state2, control, measure, threshold) {
  ifelse(measure$minimize, 1, -1) * (state1$y[1] - state2$y[1]) > threshold
}
