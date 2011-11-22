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
    prob.init = "numeric",
    prob.mut.learner = "numeric",
    prob.mut.bit = "numeric",
    mut.hp.eta = "numeric",
    mut.hp.prob = "numeric",
    prob.cx = "numeric"
  )
)

#' Constructor.
setMethod(
  f = "initialize",
  signature = signature("VarselControlMCO"),
  def = function(.Object, same.resampling.instance, maxit, mu, ref.point, prob.init, prob.mut.learner, prob.mut.bit, mut.hp.eta, mut.hp.prob, prob.cx) {
    .Object@mu = mu
    .Object@ref.point = ref.point
    .Object@prob.init = prob.init
    .Object@prob.mut.learner = prob.mut.learner
    .Object@prob.mut.bit = prob.mut.bit
    .Object@mut.hp.eta = mut.hp.eta
    .Object@mut.hp.prob = mut.hp.prob
    .Object@prob.cx = prob.cx
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
  def = function(same.resampling.instance, maxit, mu, ref.point, prob.init, prob.mut.learner, prob.mut.bit, mut.hp.eta, mut.hp.prob, prob.cx) {
    if (missing(same.resampling.instance))
      same.resampling.instance = TRUE
    if (missing(ref.point))
      ref.point = as.numeric(NA)
    checkArg(mu, "integer", 1L, lower=1L)
    checkArg(prob.init, "numeric", 1, lower=0, upper=1)
    checkArg(prob.mut.learner, "numeric", 1, lower=0, upper=1)
    checkArg(prob.mut.bit, "numeric", 1, lower=0, upper=1)
    checkArg(prob.cx, "numeric", 1, lower=0, upper=1)
    checkArg(mut.hp.eta, "numeric", 1)
    checkArg(mut.hp.prob, "numeric", 1, lower=0, upper=1)
    standardGeneric("makeVarselControlMCO")
  }
)

#' @rdname makeVarselControlMCO 

setMethod(
  f = "makeVarselControlMCO",
  signature = signature(same.resampling.instance="logical", maxit="integer", mu="integer", ref.point="numeric", 
    prob.init="numeric", prob.mut.learner="numeric", prob.mut.bit="numeric",
    mut.hp.eta="numeric", mut.hp.prob="numeric",
    prob.cx="numeric"),
  def = function(same.resampling.instance, maxit, mu, ref.point, prob.init, prob.mut.learner, prob.mut.bit,  mut.hp.eta, mut.hp.prob, prob.cx) {
    new("VarselControlMCO", same.resampling.instance=same.resampling.instance, maxit, mu, ref.point, prob.init, prob.mut.learner, prob.mut.bit, mut.hp.eta, mut.hp.prob, prob.cx)
  }
)


#' @rdname undocumented
setMethod(f = "show",  signature = signature("VarselControlMCO"), def = function(object) {
    cat(
      "Control object for varselMCO\n",
      "Same resampling instance: ", object@same.resampling.instance, "\n",
      "maxit=", object@maxit, " mu=", object@mu, "\n",
      "prob.init=", object@prob.init,  " prob.mut.learner=", object@prob.mut.learner, " prob.mut.bit=", object@prob.mut.bit, "\n",
      "mut.hp.eta=", object@mut.hp.eta,  " mut.hp.prob=", object@mut.hp.prob, "\n", 
      "prob.cx=", object@prob.cx, "\n",
      "ref.point=", paste(object@ref.point, collapse=","), "\n",  
      sep=""
    )
})

