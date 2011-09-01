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


# inserts elements from x2 into x1, overwriting elements of equal names
# if el.names contains names which are nor present in x2, they are disregarded
insert = function(xs1, xs2, el.names) {
  if (missing(el.names)) {
    xs1[names(xs2)] <- xs2
  } else {
    el.names = intersect(el.names, names(xs2))
    xs1[el.names] <- xs2[el.names]
  }
	return(xs1)
}

# inserts elements from x2 into x1, only if names in x2 are already present in x1 
insert.matching = function(xs1, xs2) {
	ns = intersect(names(xs1), names(xs2))
	xs1[ns] = xs2[ns]
	return(xs1)
}


##' Split arguments into 'control' and 'other' arguments.
##'
##' Find all elements in list \code{args} whose name is contained in
##' \code{arg.names} and call function \code{control} on these. The
##' result of this is returned as the \code{control} element of the
##' list returned. All remaining elements in \code{args} are returned
##' as the \code{args} element of the return list.
##'
##' @param control [function] \cr Function to apply to the elements of
##'   \code{args} named in \code{arg.names}.
##'
##' @param arg.names [character] \cr List of argument names to extract
##'   from \code{args}.
##'
##' @param args [list] \cr List of named arguments to be split into
##'   control and other arguments.
##'
##' @return List with elements \code{control} and \code{args}.
##' @export
args.to.control = function(control, arg.names, args) {
	# put stuff into special list and remove it from args
	ctrl.args = insert(list(), args, arg.names)
	ctrl = do.call(control, ctrl.args)
	args[arg.names] = NULL
	return(list(control=ctrl, args=args))
}


check.list.type = function(xs, type, name=deparse(substitute(xs))) {
  ## FIXME: Better use inherits like this?
  ##   sapply(xs, function(x) inherits(x, type))
  
	fs = lapply(type, function(tt) switch(tt,
    character=is.character,                          
    numeric=is.numeric,
    logical=is.logical,
    integer=is.integer,
    list=is.list,
    data.frame=is.data.frame,
    function(x) is(x, tt)
    ))
	types = paste(type, collapse=", ")	
	all(sapply(seq_along(xs), function(i) {
				x = xs[[i]]
				ys = sapply(fs, function(f) f(x))
				if(!any(ys))
					stop("List ", name, " has element of wrong type ", class(x), " at position ", i, ". Should be: ", types)
				any(ys)
	}))
}


##' Returns TRUE if all entries in the name attribute of \code{xs} valid names.
all.els.named = function(xs) {
	ns = names(xs)
	(length(xs) == 0) || (!is.null(ns) && !any(is.na(ns)) && !any(ns == ""))
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
##	ys = as.data.frame(do.call(rbind, xs))
##	rownames(ys) = rownames
##	return(ys)
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

check.getter.args = function(x, arg.names, j, ...) {
	args = list(...)
	ns = names(args)
	for (i in seq_along(args)) {
		n = ns[i]
		a = args[[i]]
		# condition because of spurious extra arg (NULL) bug in "["
		if ( !(is.null(a) && (is.null(n) || length(a) == 0)) ) {
			if (is.null(n) || length(a) == 0)
				stop("Using unnamed extra arg ", a, " in getter of ", class(x), "!")
			if (!(n %in% arg.names))
				stop("Using unallowed extra arg ", paste(n, a, sep="="), " in getter of ", class(x), "!")
		}
	}
}

require.packs = function(packs, for.string) {
  # this should be a bit faster...
  packs.ok = sapply(packs, function(x) paste("package", x, sep = ":") %in% search())
  packs = packs[!packs.ok]
  packs.ok = sapply(packs, function(x) require(x, character.only = TRUE))
	if (length(packs.ok) == 0)
		packs.ok = TRUE
	if(!all(packs.ok)) {
		ps = paste(packs[!packs.ok], collapse=" ")
		stop(paste("For", for.string, "please install the following packages:", ps))
    ## DOIT: Possibly add option to run install.packages() if interactive() is TRUE?
    ##  Check adverse effects regarding parallelization.
	}
	return(packs.ok)
}

##' Check if \code{e1} and \code{e2} are equal ignoring such fine
##' points as \code{1 != 1L}.
almost.equal <- function(e1, e2) {
  isTRUE(all.equal(e1, e2))
}


par.valnames.to.vals = function(names, par.set) {
  Map(function(par, n) par@constraints$vals[[n]], par.set@pars, names)
}

# converts a row of a data.frame to a list
# - factors are converted to chars
data.frame.row.to.list = function(x, i) {
  x = as.list(x[i,])
  x = lapply(x, function(y) if(is.factor(y)) as.character(y) else y)
}

check.arg = function(x, cl, len, choices, lower=NA, upper=NA) {
  s = deparse(substitute(x))
  cl2 = class(x)
  if (!is(x, cl)) 
    stop("Argument ", s, " must be of class ", cl, " not: ", cl2, "!")
  len2 = length(x)
  if (!missing(len) && len2 != len)
      stop("Argument ", s, " must be of length ", len, " not: ", len2, "!")
  if (!missing(choices) && !(x %in% choices))
    stop("Argument ", s, " must be any of: ", paste(choices, collapse=","), "!")
  if (!missing(choices) && !(x %in% choices))
    stop("Argument ", s, " must be any of: ", paste(choices, collapse=","), "!")
  if (is.numeric(x) && !is.na(lower) && (is.na(x) || x < lower))
    stop("Argument ", s, " must be greater than or equal ", lower, "!")
  if (is.numeric(x) && !is.na(upper) && (is.na(x) || x > upper))
    stop("Argument ", s, " must be less than or equal ", upper, "!")
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
  
  if (.mlr.local$logger.setup$global.level == "debug" && .mlr.local$logger.setup$sublevel == "parallel") {
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


eval.states = function(learner, task, resampling, measures, par.set, bits.to.features, control, opt.path, pars, 
  eol=as.integer(NA), dob=as.integer(NA)) {
  
  y = mylapply(xs=pars, from="opt", f=eval.rf, learner=learner, task=task, resampling=resampling, 
    measures=measures, par.set=par.set, bits.to.features=bits.to.features, control=control)
  n = length(pars)
  if (length(dob) == 1)
    dob = rep(dob, n)
  if (length(eol) == 1)
    eol = rep(eol, n)
  for (i in 1:n) 
    addPathElement(opt.path, x=as.list(pars[[i]]), y=y[[i]], dob=dob[i], eol=eol[i])
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
  if (length(intersect(ns, names(par.set@pars))) > 0 ||
      length(intersect(ns, getRepeatedParameterIDs(par.set, TRUE))) > 0)
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
        x[[p@id]] = df[,col]
    } else 
      x[[p@id]] = df[,col]
  }
  return(x)
}


getRepeatedParameterIDs = function(par.set, with.nr) {
  ns = lapply(par.set@pars, function(x) 
      if (x@type %in% c("numericvector", "integervector")) {
        m = length(x@constraints$lower)
        if (m > 1 && with.nr)
          paste(rep(x@id, m), 1:m, sep="")
        else
          rep(x@id, m)
      } else 
        x@id
  )
  Reduce(c, ns)
}


