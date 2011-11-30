vote.majority = function(x) {
  tt = table(x)
  y = seq_along(tt)[tt == max(tt)]
  if (length(y) > 1L) 
    y = sample(y, 1L)
  names(tt)[y]
}

# selects the maximal name of the maximal element of a numerical vector - breaking ties at random
vote.max.val = function(x, names=names(x)) {
  y = seq_along(x)[x == max(x)]
  if (length(y) > 1L) 
    y = sample(y, 1L)
  return(names[y])
}


par.valnames.to.vals = function(names, par.set) {
  Map(function(par, n) par$values[[n]], par.set$pars, names)
}

checkColumnNames = function(data, target, exclude) {
  cns = colnames(data)
  x = duplicated(cns)
  if(any(x))
    stop("Duplicated column names in data.frame are not allowed: ", paste(cns[x], collapse=","))
  if (!(target %in% cns)) {
    stop(paste("Column names of data.frame don't contain target var:", target))
  }
  
  if (!all(exclude %in% cns))
    stop("Trying to exclude non-existing variables: ", setdiff(exclude, cns))
  if (target %in% exclude)
    stop("Trying to exclude target variable!")
  
}

checkWeightsAndBlocking = function(data, target, weights, blocking) {
  if(length(weights) > 0 && length(weights) != nrow(data))
    stop("Weights have to be of the same length as number of rows in data! Or pass none at all.")
  if(length(blocking) > 0 && length(blocking) != nrow(data))
    stop("Blockings have to be of the same length as number of rows in data! Or pass none at all.")
}


warn.wrapper = function(x, myfun, arg.names) {
  assign(".mlr.slave.warnings", character(0), envir = .GlobalEnv)
  
  withCallingHandlers({
      args = mget(arg.names, env=.GlobalEnv)
      args[[length(args)+1]] = x
      y = do.call(myfun, args)
    }, 
    warning = function(w) {
      sws = get(".mlr.slave.warnings", envir = .GlobalEnv) 
      assign(".mlr.slave.warnings", c(sws, w), envir = .GlobalEnv)
    }
  )
  sws = get(".mlr.slave.warnings", envir = .GlobalEnv) 
  if (length(sws) > 0)
    attr(y, ".mlr.slave.warnings") = sws
  return(y)
}


# compare 2 states.  
# TRUE : state2 is significantly better than state1  
# compare = function(state1, state2, control, measures, threshold) 


# use the difference in performance   
compare.diff = function(state1, state2, control, measure, threshold) {
  ifelse(measure@minimize, 1, -1) * (state1$y[1] - state2$y[1]) > threshold
}

makeOptPathDFFromMeasures = function(par.set, measures) {
  ns = sapply(measures, measureAggrName)
  if (any(duplicated(ns)))
    stop("Cannot create OptPath, measures do not have unique ids!")
  if (length(intersect(ns, names(par.set$pars))) > 0 ||
    length(intersect(ns, getParamIds(par.set, repeated=TRUE, with.nr=TRUE))) > 0)
    stop("Cannot create OptPath, measures ids and dimension names of input space overlap!")
  minimize = sapply(measures, function(m) m@minimize)
  makeOptPathDF(par.set, ns, minimize)
}

measureAggrName = function(measure) {
  paste(measure@id, measure@aggr@id, sep=".")
}


perfsToString = function(y) {
  paste(paste(names(y), "=", formatC(y, digits=3), sep=""), collapse=",")
}
