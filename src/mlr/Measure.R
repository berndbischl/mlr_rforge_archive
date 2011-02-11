#' @include object.r
roxygen()

#' A measure object encapsulates a function to evaluate the performance of a prediction.
#' Information about already implemented measures can be obtained here: \code{\link{measures}}.
#' User-defined measures can be created with \code{\link{make.measure}}. Also look at the extension point in the web tutorial.
#' 
#' A learner is trained on a a training set d1, results in a model m, predicts a test set d2, resulting in the final prediction.
#' In order to   
#' 
#' Getter.\cr
#' 
#' \describe{
#' 	\item{id [string]}{Name of the measure.}
#' 	\item{minimize [boolean]}{Should the performance measure be minimized?}
#'  \item{req.pred [boolean]}{Does the calculation require the prediction of the test set?).}
#'  \item{req.task [boolean]}{Does the calculation require the task? Usually the case when you want to look a feature values in order to calculate the performance.).}
#' 	\item{req.task.type [character]}{For which tasks can the measure be used?}
#' 	\item{req.binary [boolean]}{Can the measure only be used for binary classification?}
#' 	\item{req.pred.type [character]}{For which prediction can the measure be used, e.g. only for probabilities?}
#' 	\item{fun [function]}{Calculation function.}
#'  \item{pars [list]}{Extra parameters for calculation.}
#' }
#' 
#' @exportClass measure
#' @seealso \code{\link{measures}}, \code{\link{make.measure}}, \code{\link{make.cost.measure}}
#' @title Class for performance measures.



setClass(
  "Measure",
  contains = c("object"),
  representation = representation(
    id = "character",
    fun = "function",
    extra.pars = "list",
    minimize = "logical",
    req.task.type = "character",
    req.binary = "logical",
    req.pred.type = "character",
    req.pred = "logical",
    req.model = "logical",
    req.task = "logical",
    aggr = "list"
  )
)

#' @rdname measure-class
setMethod(
  f = "[",
  signature = signature("Measure"),
  def = function(x,i,j,...,drop) {
    callNextMethod()
  }
)

#' @rdname to.string
setMethod(
  f = "to.string",
  signature = signature("Measure"),
  def = function(x) {
    return(
      paste(
        "Performance measure: ", x@id, "\n",
        "Minimize: ", x["minimize"], "\n",
        "Aggregated by: ", paste(sapply(x["aggr"], function(a) a@id), collapse=","),
        sep=""
      )
    )
  }
)


