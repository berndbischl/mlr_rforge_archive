#' @include VarselControl.R
roxygen()

#' @exportClass VarselControlExhaustive
#' @rdname VarselControlExhaustive 

setClass(
  "VarselControlExhaustive",
  contains = c("VarselControl")
)

#' Constructor.
setMethod(
    f = "initialize",
    signature = signature("VarselControlExhaustive"),
    def = function(.Object, path, same.resampling.instance, max.vars) {
      callNextMethod(.Object, path=path, same.resampling.instance=same.resampling.instance, max.vars=max.vars, maxit=.Machine$integer.max)
    }
)


#' Control structure for exhaustive variable selection. 
#' 
#' @param path [\code{logical(1)}]\cr
#'   Should optimization path be saved? Default is TRUE.
#' @param same.resampling.instance [logical(1)] \cr
#'    Should the same resampling instance be used for all evaluations to reduce variance? Default is \code{TRUE}.
#' @param max.vars [\code{integer}] \cr 
#'   Maximal number of allowed variables searched sets. Default is max. integer.
#'        
#' @return Control structure.
#' @exportMethod makeVarselControlExhaustive
#' @rdname makeVarselControlExhaustive 
#' @title Control structure for exhaustive variable selection. 


setGeneric(
  name = "makeVarselControlExhaustive",
  def = function(path, same.resampling.instance, max.vars) {
    if (missing(path))
      path = TRUE
    if (missing(same.resampling.instance))
      same.resampling.instance = TRUE
    if (missing(max.vars))
      max.vars = .Machine$integer.max
    if (is.numeric(max.vars))
      max.vars = as.integer(max.vars)
    standardGeneric("makeVarselControlExhaustive")
  }
)

#' @rdname makeVarselControlExhaustive 

setMethod(
  f = "makeVarselControlExhaustive",
  signature = signature(path="logical", same.resampling.instance="logical", max.vars="integer"),
  def = function(path, same.resampling.instance, max.vars) {
    new("VarselControlExhaustive", path=path, same.resampling.instance=same.resampling.instance, max.vars=max.vars)
  }
)



