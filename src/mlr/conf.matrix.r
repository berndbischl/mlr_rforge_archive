#' Calculates confusion matrix for (possibly resampled) prediction. 
#' Rows indicate true classes, columns predicted classes.
#' 
#' @param pred [\code{\linkS4class{Prediction}}] \cr
#'   Result of a prediction.
#' @param relative [logical] \cr 
#' 	If TRUE rows are normalized to show relative frequencies.
#' 
#' @return A confusion matrix.
#' 
#' @exportMethod getConfMatrix
#' @rdname getConfMatrix
#' @seealso \code{\link[klaR]{errormatrix}}
#' @title Confusion matrix.

setGeneric(
  name = "getConfMatrix",
  def = function(pred, relative) {
    if (missing(relative))
      relative=FALSE
    standardGeneric("getConfMatrix")
  }
)

#' @export
#' @rdname getConfMatrix
setMethod(
  f = "getConfMatrix",
  signature = signature(pred="Prediction", relative="logical"),
  def = function(pred, relative) {
    return(errormatrix(pred@df$truth, pred@df$response, relative=relative))
  }
)
