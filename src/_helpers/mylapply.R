mylapply <- function(xs, f, from, ...) {
  ps = .mlr.conf$parallel.setup
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
      y = mclapply(xs, function(x, ...) {.mlr.set.local.on.slave(.mlr.conf);f(x, ...)}, ..., mc.cores=ps$cpus)
    } else {
      stop("Unknown parallel model: ", ps$mode)
    }
  }
  
  if (.mlr.conf$logger.setup$global.level == "debug") {
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
