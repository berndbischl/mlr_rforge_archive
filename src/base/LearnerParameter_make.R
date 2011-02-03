#' Numerical parameter for a learner.
#' @param id [character(1)]
#'   Name of parameter.
#' @param lower [numeric(1)] \cr
#'   Lower bound. Default is \code{-Inf}.
#' @param upper [numeric(1)] \cr
#'   Upper bound. Default is \code{Inf}.
#' @param default [numeric(1)]
#'   Default value used in learner. If this argument is missing, it means no default value is available.
#' @param when [character(1)]
#'   When is the parameter used in the corresponding learner, either 'train', 'predict' or 'both'. Default is 'train'.
#' @return  \code{\linkS4class{Parameter}}
#' @export 
makeNumericLearnerParameter <- function(id, lower=-Inf, upper=Inf,
                                      default, when="train",
                                      flags=list(), requires=expression()) {
  p <- makeNumericParameter(id, lower, upper)
  learner.parameter.from.parameter(p, default, when, flags, requires)
}

#' Numerical vector parameter for a learner.
#' @param id [character(1)]
#'   Name of parameter.
#' @param lower [numeric(1)] \cr
#'   Lower bound. Default is \code{-Inf}.
#' @param upper [numeric(1)] \cr
#'   Upper bound. Default is \code{Inf}.
#' @param default [numeric(1)]
#'   Default value used in learner. If this argument is missing, it means no default value is available.
#' @param when [character(1)]
#'   When is the parameter used in the corresponding learner, either 'train', 'predict' or 'both'. Default is 'train'.
#' @return  \code{\linkS4class{Parameter}}
#' @export 
makeNumericVectorLearnerParameter <- function(id, lower=-Inf, upper=Inf,
  default, when="train",
  flags=list(), requires=expression()) {
  p <- makeNumericParameter(id, lower, upper)
  learner.parameter.from.parameter(p, default, when, flags, requires)
}


#' Integer parameter for a learner.
#' @param id [character(1)]
#'   Name of parameter.
#' @param lower [numeric(1)] \cr
#'   Lower bound. Default is \code{-.Machine$integer.max}.
#' @param upper [numeric(1)] \cr
#'   Upper bound. Default is \code{.Machine$integer.max}.
#' @param default [integer(1)]
#'   Default value used in learner. If this argument is missing, it means no default value is available.
#' @param when [character(1)]
#'   When is the parameter used in the corresponding learner, either 'train', 'predict' or 'both'. Default is 'train'.
#' @return  \code{\linkS4class{Parameter}}
#' @export 
makeIntegerLearnerParameter <- function(id, lower=-.Machine$integer.max, upper=.Machine$integer.max,
                                      default, when="train",
                                      flags=list(), requires=expression()) {
  p <- makeIntegerParameter(id, lower, upper)
  learner.parameter.from.parameter(p, default, when, flags, requires)
}


#' Numerical parameter for a learner.
#' @param id [character(1)]
#'   Name of parameter.
#' @param vals [list | vector] \cr
#'   Lower bound. Default is \code{-Inf}.
#' @param upper [numeric(1)] \cr
#'   Upper bound. Default is \code{Inf}.
#' @param default [any]
#'   Default value used in learner. If this argument is missing, it means no default value is available.
#' @param when [character(1)]
#'   When is the parameter used in the corresponding learner, either 'train', 'predict' or 'both'. Default is 'train'.
#' @return  \code{\linkS4class{Parameter}}
#' @export 
makeDiscreteLearnerParameter <- function(id, vals,
                                      default, when="train",
                                      flags=list(), requires=expression()) {
  p <- makeDiscreteParameter(id, vals)
  learner.parameter.from.parameter(p, default, when, flags, requires)
}

#' Logical parameter for a learner.
#' @param id [character(1)]
#'   Name of parameter.
#' @param default [logical(1)]
#'   Default value used in learner. If this argument is missing, it means no default value is available.
#' @param when [character(1)]
#'   When is the parameter used in the corresponding learner, either 'train', 'predict' or 'both'. Default is 'train'.
#' @return  \code{\linkS4class{Parameter}}
#' @export 
makeLogicalLearnerParameter <- function(id,
                                      default, when="train",
                                      flags=list(), requires=expression()) {
  p <- makeLogicalParameter(id)
  learner.parameter.from.parameter(p, default, when, flags, requires)
}


#' Untyped parameter for a learner.
#' @param id [character(1)]
#'   Name of parameter.
#' @param default [any]
#'   Default value used in learner. If this argument is missing, it means no default value is available.
#' @param when [character(1)]
#'   When is the parameter used in the corresponding learner, either 'train', 'predict' or 'both'. Default is 'train'.
#' @return  \code{\linkS4class{Parameter}}
#' @export 
makeUntypedLearnerParameter <- function(id, default, when="train", flags=list(), requires=expression()) {
  p <- makeUntypedParameter(id)
  learner.parameter.from.parameter(p, default, when, flags, requires)
}


#' Function parameter for a learner.
#' @param id [character(1)]
#'   Name of parameter.
#' @param default [any]
#'   Default value used in learner. If this argument is missing, it means no default value is available.
#' @param when [character(1)]
#'   When is the parameter used in the corresponding learner, either 'train', 'predict' or 'both'. Default is 'train'.
#' @return  \code{\linkS4class{Parameter}}
#' @export 
makeFunctionLearnerParameter <- function(id, default, when="train", flags=list(), requires=expression()) {
  p <- makeFunctionParameter(id)
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
