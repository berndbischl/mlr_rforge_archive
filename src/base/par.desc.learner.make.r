##double    
#if (missing(data.type))
#  data.type = ifelse(is.integer(lower) || is.integer(upper) || is.integer(default), "integer", "numeric")
#.Object@lower = lower					
#.Object@upper = upper
#if (missing(default))
#  default = 
#    .Object@deafult
#if (!(default == "missing" || (lower <= default && upper >= default)))
#  stop("Default value of par. ", name, " has to be in lower/upper limits or 'missing'!")
#callNextMethod(.Object, name, default, when, flags, requires)
#}





#.Object@name = name						
#.Object@default = default						
#.Object@when = when
#if (!(when %in% c("train", "predict", "both")))
#  stop("par.desc ", name, " : Arg 'when' can only be 'train', 'predict' or 'both', not '", when, "'!")
#.Object@flags = flags
#if (length(flags) > 0) { 
#  ns = names(flags)
#  if (!all.els.named(flags) || any(duplicated(ns)))
#    stop("par.desc: ", name, " : All elements of flag list have to be uniquely named!")
#  if (!all(ns %in% c("optimize", "pass.default")))
#    stop("par.desc: ", name, " : Only flags 'optimize' and 'pass.default' are supported!")
#  if (!all(sapply(flags, function(x) is.logical(x) || length(x)==1)))
#    stop("par.desc: ", name, " : Only boolean flags are supported!")
#}
#.Object@requires = requires						
#return(.Object)

#
##' Constructor.
#setMethod(
#  f = "initialize",
#  signature = signature("par.desc.disc"),
#  def = function(.Object, name, default="missing", when="train", vals, flags=list(), requires=expression(TRUE)) {
#    if (default != "missing") {
#      if (is.character(default) && default %in% names(vals))
#        default = vals[[default]]
#      y = sapply(vals, function(x) isTRUE(all.equal(x, default)))
#      if (!(any(y)))
#        stop("Default value of par. ", name,  " has to be among allowed values!")
#    }
#    .Object@vals = vals					
#    callNextMethod(.Object, name, default, when, flags, requires)
#  }
#)


numeric.learner.parameter <- function(name, lower=-Inf, upper=Inf,
                                      default, when="train",
                                      flags=NULL, requires=expression()) {
  p <- numeric.learner(name, lower, upper)
  learner.parameter.from.parameter(p, default, when, flags, requires)
}


integer.learner.parameter <- function(name, lower=-Inf, upper=Inf,
                                      default, when="train",
                                      flags=NULL, requires=expression()) {
  p <- integer.learner(name, lower, upper)
  learner.parameter.from.parameter(p, default, when, flags, requires)
}


discrete.learner.parameter <- function(name, vals,
                                      default, when="train",
                                      flags=NULL, requires=expression()) {
  p <- discrete.learner(name, vals)
  learner.parameter.from.parameter(p, default, when, flags, requires)
}

logical.learner.parameter <- function(name,
                                      default, when="train",
                                      flags=NULL, requires=expression()) {
  p <- logical.learner(name, vals)
  learner.parameter.from.parameter(p, default, when, flags, requires)
}

learner.parameter.from.parameter <- function(p, default, when, flags, requires) {
  if (!is.list(flags))
    stop("'flags' must be a list.")
  if (!is.expression(requires))
    stop("'requires' must be an R expression.")
  if (!missing(default) && !is.feasible(default, p))
    stop("'default' must be missing or a feasible parameter setting.")  
  check.arg(when, "character", 1)
  
  pp <- new("par.desc.learner",
            name=p@name, type=p@type, constraints=p@constraints,
            has.default=!missing(default),
            default=if (missing(default)) NULL else default,
            when=when, flags=flags, requires=requires)
}
