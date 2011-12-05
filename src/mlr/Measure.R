#' A measure object encapsulates a function to evaluate the performance of a prediction.
#' Information about already implemented measures can be obtained here: \code{\link{measures}}.
#' User-defined measures can be created with \code{\link{makeMeasure}}.
#' 
#' A learner is trained on a a training set d1, results in a model m, predicts another set d2 (which may be a different one
#' or the training set), resulting in the prediction. The performance measure can now be defined using all of the information of
#' the original task, the fitted model and the prediction.   
#' 
#' @slot id Name of the measure.
#' @slot minimize Should the performance measure be minimized?
#' @slot classif Is the measure applicable for classification?   
#' @slot regr Is the measure applicable for regression?   
#' @slot only.binary Can the measure only be used for binary classification?
#' @slot req.pred Does the calculation require a prediction object?
#' @slot req.task Does the calculation require the task? 
#' @slot req.model Does the calculation require the model?
#' @slot allowed.pred.types Which prediction types are allowed for this measure? Subset of \dQuote{response},\dQuote{prob}.   
#' @slot fun Calculation function.
#' @slot extra.args Extra parameters for calculation.
#' @slot aggr Associated aggregation function.
#' 
#' @exportClass Measure
#' @seealso \code{\link{measures}}, \code{\link{makeMeasure}}
#' @title Class for performance measures.



setClass(
  "Measure",
  representation = representation(
    id = "character",
    fun = "function",
    extra.args = "list",
    minimize = "logical",
    regr = "logical",
    classif = "logical",
    only.binary = "logical",
    req.pred = "logical",
    req.model = "logical",
    req.task = "logical",
    allowed.pred.types = "character",
    aggr = "Aggregation"
  )
)


setMethod("show", "Measure", function(object) {
  catf("Performance measure: %s", object@id)
  catf("Minimize: %s", object@minimize)
  catf("Aggregated by: %s", object@aggr@id)
})


