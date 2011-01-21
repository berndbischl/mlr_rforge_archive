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

numeric.learner.parameter <- function(name, lower=-Inf, upper=Inf,
                                      default, when="train",
                                      flags=list(), requires=expression()) {
  p <- numeric.parameter(name, lower, upper)
  learner.parameter.from.parameter(p, default, when, flags, requires)
}


integer.learner.parameter <- function(name, lower=-.Machine$integer.max, upper=.Machine$integer.max,
                                      default, when="train",
                                      flags=list(), requires=expression()) {
  p <- integer.parameter(name, lower, upper)
  learner.parameter.from.parameter(p, default, when, flags, requires)
}


discrete.learner.parameter <- function(name, vals,
                                      default, when="train",
                                      flags=list(), requires=expression()) {
  p <- discrete.parameter(name, vals)
  learner.parameter.from.parameter(p, default, when, flags, requires)
}

logical.learner.parameter <- function(name,
                                      default, when="train",
                                      flags=list(), requires=expression()) {
  p <- logical.parameter(name)
  learner.parameter.from.parameter(p, default, when, flags, requires)
}


untyped.learner.parameter <- function(name, default, when="train", flags=list(), requires=expression()) {
  p <- untyped.parameter(name)
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
