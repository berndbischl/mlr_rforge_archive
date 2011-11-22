c.factor = function(..., recursive=FALSE) {
  args <- list(...)
  for (i in seq_along(args))
    if (!is.factor(args[[i]]))
      args[[i]] = as.factor(args[[i]])
  ## The first must be factor otherwise we wouldn't be inside
  ## c.factor, its checked anyway in the line above.
  newlevels = sort(unique(unlist(lapply(args, levels))))
  ans = unlist(lapply(args, function(x) {
        m = match(levels(x), newlevels)
        m[as.integer(x)]
      }))
  levels(ans) = newlevels
  class(ans) = "factor"
  return(ans)
}


## do lapply recursively on deep lists
## FIXME: Use rapply instead? Possibly not useful because rapply does
##   not limit descend depth. Investigate if rec.lapply becomes a
##   bottleneck.
rec.lapply = function(xs, fun, depth=Inf) {
  if (!is.list(xs) || is.data.frame(xs) || depth==0) {
    return(fun(xs))
  }
  lapply(xs, function(x) rec.lapply(x, fun, depth-1))
}

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


# returns first non null el. 
coalesce = function (...) {
  l <- list(...)
  isnull <- sapply(l, is.null)
  l[[which.min(isnull)]]
}

## FIXME: 20100925 - Not used and possibly nonsense...
## list2dataframe = function(xs, rownames=NULL) {
##  ys = as.data.frame(do.call(rbind, xs))
##  rownames(ys) = rownames
##  return(ys)
## }

path2dataframe = function(path) {
  p = path[[1]]
  cns = c(names(p$par), names(p$perf), "evals", "event", "accept")
  df = matrix(0, length(path), length(cns))
  colnames(df) = cns
  n = length(p$par)
  m = length(p$perf)
  k = ncol(df)
  df = as.data.frame(df)
  df$event = as.character(df$event)
  for (i in 1:length(path)) {
    p = path[[i]]
    df[i, 1:n] = unlist(p$par)  
    df[i, (n+1):(k-2)] = c(unlist(p$perf), p$evals)  
    df[i, k-1] = p$event  
    df[i, k] = p$accept  
  }
  return(df)
}

##' Check if \code{e1} and \code{e2} are equal ignoring such fine
##' points as \code{1 != 1L}.
almost.equal <- function(e1, e2) {
  isTRUE(all.equal(e1, e2))
}


par.valnames.to.vals = function(names, par.set) {
  Map(function(par, n) par@constraints$vals[[n]], par.set@pars, names)
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


mylapply <- function(xs, f, from, ...) {
  ps = .mlr.local$parallel.setup
  if (ps$mode == "local" || ps$level != from) {
    y = lapply(xs, f, ...)
  } else {
    args = list(...)
    ns = names(args)
    
    for (i in seq(length(ns))) {
      export(ns[i], args[[i]])
    }
    
    if (ps$mode %in% c("sfCluster", "snowfall")){
      y = sfClusterApplyLB(x=xs, fun=warn.wrapper, myfun=f, arg.names=ns)   
    } else if (ps$mode == "multicore") {
      # todo check warnings
      y = mclapply(xs, function(x, ...) {.mlr.set.local.on.slave(.mlr.local);f(x, ...)}, ..., mc.cores=ps$cpus)
    } else {
      stop("Unknown parallel model: ", ps$mode)
    }
  }
  
  if (.mlr.local$logger.setup$global.level == "debug") {
    sizes = sapply(y, object.size)
    logger.debug(level="parallel", "mylapply returned sizes:", range(sizes))
  }
  
  if (length(y) > 0) {
    for (i in 1:length(y)) {
      x = y[[i]]
      if (is(x, "try-error")) {
        stop(paste("On slave:", x))
      }
      ws = attr(x, ".mlr.slave.warnings")
      if (!is.null(ws)) {
        warning(paste("On slave:", ws))
        attr(y[[i]], ".mlr.slave.warnings") = NULL
      }
    }
  }
  return(y)
}

# todo: remove?
eval.rf = function(learner, task, resampling, measures, par.set, bits.to.features, control, val) {
  if (is(control, "TuneControl")) {
    learner = setHyperPars(learner, par.vals=val)
  }
  if (is(control, "VarselControl")) {
    task = subsetData(task, vars=bits.to.features(val, task))
  }
  # todo 
# if (control["tune.threshold"]) 
#   type = "prob"
  r = resample(learner, task, resampling, measures=measures)
  return(r$aggr)
  
# th = as.numeric(NA)
# if (control["tune.threshold"]) { 
#   thr = tune.threshold(rf, measures, task, minimize=control@minimize, thresholds=control["thresholds"])
#   rf = thr$pred
#   th = thr$th
# }
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
  if (length(intersect(ns, names(par.set@pars))) > 0 ||
    length(intersect(ns, getParamIds(par.set, repeated=TRUE, with.nr=TRUE))) > 0)
    stop("Cannot create OptPath, measures ids and dimension names of input space overlap!")
  minimize = sapply(measures, function(m) m@minimize)
  new("OptPathDF", par.set, ns, minimize)
}

measureAggrName = function(measure) {
  paste(measure@id, measure@aggr@id, sep=".")
}


dataFrameRowToList = function(df, par.set, i) {
  df = df[i,,drop=FALSE]
  pars = par.set@pars
  col = 0
  x = list()
  for (i in 1:length(pars)) {
    p = pars[[i]]
    cc = rev(col)[1]
    if (p@type %in% c("numericvector", "integervector")) 
      col = (cc + 1) : (cc + length(lower(p)))   
    else 
      col = cc + 1    
    
    if (p@type == "numericvector") 
      x[[p@id]] = as.numeric(df[,col])  
    else if (p@type %in% c("integer", "integervector")) 
      x[[p@id]] = as.integer(round(df[,col]))
    else if (p@type == "discrete") {
      if(p@constraints$vals.class == "list")
        x[[p@id]] = p@constraints$vals[[df[,col]]]
      else
      if (is.factor(df[, col]))
        x[[p@id]] = as.character(df[,col])
      else
        x[[p@id]] = df[,col]
    } else 
      x[[p@id]] = df[,col]
  }
  return(x)
}

perfsToString = function(y) {
  paste(paste(names(y), "=", formatC(y, digits=3), sep=""), collapse=",")
}
