##double    
#if (missing(data.type))
#  data.type = ifelse(is.integer(lower) || is.integer(upper) || is.integer(default), "integer", "numeric")
#.Object@lower = lower					
#.Object@upper = upper
#if (missing(default))
#  default = 
#    .Object@deafult
#if (!(default == "missing" || (lower <= default && upper >= default)))
#  stop("Default value of par. ", id, " has to be in lower/upper limits or 'missing'!")
#callNextMethod(.Object, id, default, when, flags, requires)
#}




makeNumericLearnerParameter <- function(id, lower=-Inf, upper=Inf,
                                      default, when="train",
                                      flags=list(), requires=expression()) {
  p <- numeric.parameter(id, lower, upper)
  learner.parameter.from.parameter(p, default, when, flags, requires)
}


makeIntegerLearnerParameter <- function(id, lower=-.Machine$integer.max, upper=.Machine$integer.max,
                                      default, when="train",
                                      flags=list(), requires=expression()) {
  p <- integer.parameter(id, lower, upper)
  learner.parameter.from.parameter(p, default, when, flags, requires)
}


makeDiscreteLearnerParameter <- function(id, vals,
                                      default, when="train",
                                      flags=list(), requires=expression()) {
  p <- discrete.parameter(id, vals)
  learner.parameter.from.parameter(p, default, when, flags, requires)
}

makeLogicalLearnerParameter <- function(id,
                                      default, when="train",
                                      flags=list(), requires=expression()) {
  p <- logical.parameter(id)
  learner.parameter.from.parameter(p, default, when, flags, requires)
}


untyped.learner.parameter <- function(id, default, when="train", flags=list(), requires=expression()) {
  p <- untyped.parameter(id)
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
  
  pp <- new("LearnerParameter",
            id=p@id, type=p@type, constraints=p@constraints,
            has.default=!missing(default),
            default=if (missing(default)) NULL else default,
            when=when, flags=flags, requires=requires)
}
