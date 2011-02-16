##' @include Prediction.R
#roxygen()
##' @include Measure.R
#roxygen()
#
##' Measures the quality of Predictions w.r.t. some performance measures.
##' 
##' @param pred [\code{\linkS4class{Prediction}}] \cr
##'   Prediction object to evaluate.
##' 
##' @return A single numerical performance value.
##' 
##' @exportMethod performance
##' @rdname performance
##'
##' @title Measure performance.
#
#
#
#setGeneric(
#  name = "loss",
#  def = function(pred, fun, type) {
#    if (missing(type))
#      type = "response"
#    standardGeneric("loss")
#  }
#)
#
##' @rdname performance
#
#setMethod(
#  f = "loss",
#  signature = signature(pred="Prediction", fun="function", type="character"),
#  def = function(pred, fun, type) {
#    if (type == "response")
#      fun(pred["truth"], pred@df$response)
#    else if (type == "prob")
#      fun(pred["truth"], pred["prob"])
#    else if (type == "decision")
#      fun(pred["truth"], pred["decision"])
#    else
#      stop("Wrong type!")
#  }
#)
#
#
#
#

