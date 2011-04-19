#' Numerical parameter for a learner.
#' @param id [\code{character(1)}]
#'   Name of parameter.
#' @param lower [numeric(1)] \cr
#'   Lower bound. Default is \code{-Inf}.
#' @param upper [numeric(1)] \cr
#'   Upper bound. Default is \code{Inf}.
#' @param default [numeric(1)]
#'   Default value used in learner. If this argument is missing, it means no default value is available.
#' @param pass.default [logical(1)]
#'   Should the default value be always passed to the learner? Default is \code{FALSE}. 
#' @param when [\code{character(1)}]
#'   When is the parameter used in the corresponding learner, either 'train', 'predict' or 'both'. Default is 'train'.
#' @param requires [\code{expression}]
#'   R expression over the other parameters to define requirements which make this parameter effective. 
#' @return  \code{\linkS4class{Parameter}}
#' @export 
makeNumericLearnerParameter <- function(id, lower=-Inf, upper=Inf,
                                      default, pass.default=FALSE, when="train",
                                      requires=expression()) {
  p <- makeNumericParameter(id, lower, upper)
  learner.parameter.from.parameter(p, default, pass.default, when, requires)
}

#' Numerical vector parameter for a learner.
#' @param id [\code{character(1)}]
#'   Name of parameter.
#' @param dim [integer(1)]
#'   Dimension of vector. Can be \code{NA}, which means dimension in unknown. Default is \code{NA}.
#' @param lower [numeric(1)] \cr
#'   Lower bound. Default is \code{-Inf}.
#' @param upper [numeric(1)] \cr
#'   Upper bound. Default is \code{Inf}.
#' @param default [numeric(1)]
#'   Default value used in learner. If this argument is missing, it means no default value is available.
#' @param pass.default [logical(1)]
#'   Should the default value be always passed to the learner? Default is \code{FALSE}. 
#' @param when [\code{character(1)}]
#'   When is the parameter used in the corresponding learner, either 'train', 'predict' or 'both'. Default is 'train'.
#' @param requires [\code{expression}]
#'   R expression over the other parameters to define requirements which make this parameter effective. 
#' @return  \code{\linkS4class{Parameter}}
#' @export 
makeNumericVectorLearnerParameter <- function(id, dim=NA, lower=-Inf, upper=Inf, default, pass.default=FALSE, when="train", requires=expression()) {
  if (length(dim) == 1 && (is.na(dim) || (is.numeric(dim) && as.integer(dim) == dim)))
    dim = as.integer(dim)
  check.arg(dim, "integer", 1)
  dim2 = dim
  if (is.na(dim)) {
    check.arg(lower, "numeric", 1)
    check.arg(upper, "numeric", 1)
    dim2 = 1
    unknown.dim = TRUE
  } else {
    unknown.dim = FALSE
  } 
  p = makeNumericVectorParameter(id, dim=dim2, lower=lower, upper=upper)
  p = learner.parameter.from.parameter(p, default, pass.default, when, requires)
  p@constraints$unknown.dim = unknown.dim
  return(p)
}


#' Integer parameter for a learner.
#' @param id [\code{character(1)}]
#'   Name of parameter.
#' @param lower [numeric(1)] \cr
#'   Lower bound. Default is \code{-.Machine$integer.max}.
#' @param upper [numeric(1)] \cr
#'   Upper bound. Default is \code{.Machine$integer.max}.
#' @param default [integer(1)]
#'   Default value used in learner. If this argument is missing, it means no default value is available.
#' @param pass.default [logical(1)]
#'   Should the default value be always passed to the learner? Default is \code{FALSE}. 
#' @param when [\code{character(1)}]
#'   When is the parameter used in the corresponding learner, either 'train', 'predict' or 'both'. Default is 'train'.
#' @param requires [\code{expression}]
#'   R expression over the other parameters to define requirements which make this parameter effective. 
#' @return  \code{\linkS4class{Parameter}}
#' @export 
makeIntegerLearnerParameter <- function(id, lower=-.Machine$integer.max, upper=.Machine$integer.max,
                                      default, pass.default=FALSE, when="train",
                                      requires=expression()) {
  p <- makeIntegerParameter(id, lower, upper)
  learner.parameter.from.parameter(p, default, pass.default, when, requires)
}

#' Integer vector parameter for a learner.
#' @param id [\code{character(1)}]
#'   Name of parameter.
#' @param dim [integer(1)]
#'   Dimension of vector. Can be \code{NA}, which means dimension in unknown. Default is \code{NA}.
#' @param lower [integer(1)] \cr
#'   Lower bound. Default is \code{-.Machine$integer.max}.
#' @param upper [integer(1)] \cr
#'   Upper bound. Default is \code{Machine$integer.max}.
#' @param default [numeric(1)]
#'   Default value used in learner. If this argument is missing, it means no default value is available.
#' @param pass.default [logical(1)]
#'   Should the default value be always passed to the learner? Default is \code{FALSE}. 
#' @param when [\code{character(1)}]
#'   When is the parameter used in the corresponding learner, either 'train', 'predict' or 'both'. Default is 'train'.
#' @param requires [\code{expression}]
#'   R expression over the other parameters to define requirements which make this parameter effective. 
#' @return  \code{\linkS4class{Parameter}}
#' @export 
makeIntegerVectorLearnerParameter <- function(id, dim=NA, lower=-Inf, upper=Inf, default, pass.default=FALSE, when="train", requires=expression()) {
  if (length(dim) == 1 && (is.na(dim) || (is.numeric(dim) && as.integer(dim) == dim)))
    dim = as.integer(dim)
  check.arg(dim, "integer", 1)
  dim2 = dim
  if (is.na(dim)) {
    check.arg(lower, "numeric", 1)
    check.arg(upper, "numeric", 1)
    dim2 = 1
    unknown.dim = TRUE
  } else {
    unknown.dim = FALSE
  } 
  p = makeIntegerVectorParameter(id, dim=dim2, lower=lower, upper=upper)
  p = learner.parameter.from.parameter(p, default, pass.default, when, requires)
  p@constraints$unknown.dim = unknown.dim
  return(p)
}



#' Numerical parameter for a learner.
#' @param id [\code{character(1)}]
#'   Name of parameter.
#' @param vals [list | vector] \cr
#'   Possible values.
#' @param default [any]
#'   Default value used in learner. If this argument is missing, it means no default value is available.
#' @param pass.default [logical(1)]
#'   Should the default value be always passed to the learner? Default is \code{FALSE}. 
#' @param when [\code{character(1)}]
#'   When is the parameter used in the corresponding learner, either 'train', 'predict' or 'both'. Default is 'train'.
#' @param requires [\code{expression}]
#'   R expression over the other parameters to define requirements which make this parameter effective. 
#' @return  \code{\linkS4class{Parameter}}
#' @export 
makeDiscreteLearnerParameter <- function(id, vals,
                                      default, pass.default=FALSE, when="train",
                                      requires=expression()) {
  p <- makeDiscreteParameter(id, vals)
  learner.parameter.from.parameter(p, default, pass.default, when, requires)
}

#' Logical parameter for a learner.
#' @param id [\code{character(1)}]
#'   Name of parameter.
#' @param default [logical(1)]
#'   Default value used in learner. If this argument is missing, it means no default value is available.
#' @param pass.default [logical(1)]
#'   Should the default value be always passed to the learner? Default is \code{FALSE}. 
#' @param when [\code{character(1)}]
#'   When is the parameter used in the corresponding learner, either 'train', 'predict' or 'both'. Default is 'train'.
#' @param requires [\code{expression}]
#'   R expression over the other parameters to define requirements which make this parameter effective. 
#' @return  \code{\linkS4class{Parameter}}
#' @export 
makeLogicalLearnerParameter <- function(id,
                                      default, pass.default=FALSE, when="train",
                                      requires=expression()) {
  p <- makeLogicalParameter(id)
  learner.parameter.from.parameter(p, default, pass.default, when, requires)
}


#' Untyped parameter for a learner.
#' @param id [\code{character(1)}]
#'   Name of parameter.
#' @param default [any]
#'   Default value used in learner. If this argument is missing, it means no default value is available.
#' @param pass.default [logical(1)]
#'   Should the default value be always passed to the learner? Default is \code{FALSE}. 
#' @param when [\code{character(1)}]
#'   When is the parameter used in the corresponding learner, either 'train', 'predict' or 'both'. Default is 'train'.
#' @param requires [\code{expression}]
#'   R expression over the other parameters to define requirements which make this parameter effective. 
#' @return  \code{\linkS4class{Parameter}}
#' @export 
makeUntypedLearnerParameter <- function(id, default, pass.default=FALSE, when="train", requires=expression()) {
  p <- makeUntypedParameter(id)
  learner.parameter.from.parameter(p, default, pass.default, when, requires)
}


#' Function parameter for a learner.
#' @param id [\code{character(1)}]
#'   Name of parameter.
#' @param default [any]
#'   Default value used in learner. If this argument is missing, it means no default value is available.
#' @param pass.default [logical(1)]
#'   Should the default value be always passed to the learner? Default is \code{FALSE}. 
#' @param when [\code{character(1)}]
#'   When is the parameter used in the corresponding learner, either 'train', 'predict' or 'both'. Default is 'train'.
#' @param requires [\code{expression}]
#'   R expression over the other parameters to define requirements which make this parameter effective. 
#' @return  \code{\linkS4class{Parameter}}
#' @export 
makeFunctionLearnerParameter <- function(id, default, pass.default=FALSE, when="train", requires=expression()) {
  p <- makeFunctionParameter(id)
  learner.parameter.from.parameter(p, default, pass.default, when, requires)
}

learner.parameter.from.parameter <- function(p, default, pass.default, when, requires) {
  if (!is.expression(requires))
    stop("'requires' must be an R expression.")
  if (!missing(default) && !isFeasible(p, default))
    stop(p@id, " : 'default' must be missing or a feasible parameter setting.")  
  check.arg(when, "character", 1)
  
  new("LearnerParameter",
            id=p@id, type=p@type, constraints=p@constraints,
            has.default=!missing(default),
            default=if (missing(default)) NULL else default,
            pass.default = pass.default,
            when=when, requires=requires)
}
