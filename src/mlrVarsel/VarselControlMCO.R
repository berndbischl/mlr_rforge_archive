#' Control object for MCO variable selection. 
#' 
#' @exportClass VarselControlMCO
#' @title Control object for MCO variable selection.

setClass(
  "VarselControlMCO",
  contains = c("VarselControl"),
  representation = representation(
    mu = "integer",
    ref.point = "numeric",
    mut.prob = "numeric",
    cross.prob = "numeric"
  )
)

#' Constructor.
setMethod(
  f = "initialize",
  signature = signature("VarselControlMCO"),
  def = function(.Object, same.resampling.instance, maxit, mu, ref.point, mut.prob, cross.prob) {
    .Object@mu = mu
    .Object@ref.point = ref.point
    .Object@mut.prob = mut.prob
    .Object@cross.prob = cross.prob
    .Object = callNextMethod(.Object, path=TRUE, same.resampling.instance, maxit=maxit, max.vars=.Machine$integer.max)
    return(.Object)
  }
)

#' Control structure for random variable selection. 
#' 
#' @param same.resampling.instance [logical(1)] \cr
#'   Should the same resampling instance be used for all evaluations to reduce variance? Default is \code{TRUE}.
#'        
#' @return Control structure.
#' @exportMethod makeVarselControlMCO
#' @title Control structure for random variable selection. 


setGeneric(
  name = "makeVarselControlMCO",
  def = function(same.resampling.instance, maxit, mu, ref.point, mut.prob, cross.prob) {
    if (missing(same.resampling.instance))
      same.resampling.instance = TRUE
    standardGeneric("makeVarselControlMCO")
  }
)

#' @rdname makeVarselControlMCO 

setMethod(
  f = "makeVarselControlMCO",
  signature = signature(same.resampling.instance="logical", maxit="integer", mu="integer", ref.point="numeric", mut.prob="numeric", cross.prob="numeric"),
  def = function(same.resampling.instance, maxit, mu, ref.point, mut.prob, cross.prob) {
    new("VarselControlMCO", same.resampling.instance=same.resampling.instance, maxit, mu, ref.point, mut.prob, cross.prob)
  }
)


setMethod(f = "show",  signature = signature("VarselControlMCO"), def = function(object) {
    cat(
      "Control object for varselMCO\n",
      "Same resampling instance: ", object@same.resampling.instance, "\n",
      "maxit=", object@maxit, " mu=", object@mu, " mut.prob=", object@mut.prob, " cross.prob=", object@cross.prob, "\n",
      "ref.point=", paste(object@ref.point, collapse=","), "\n",  
      sep=""
    )
})

