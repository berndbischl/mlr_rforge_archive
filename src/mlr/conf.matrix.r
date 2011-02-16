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
#' @export
#' 
#' @seealso \code{\link[klaR]{errormatrix}}
#' 
#' @title Confusion matrix.

setGeneric(
  name = "conf.matrix",
  def = function(pred, relative) {
    if (missing(relative))
      relative=FALSE
    standardGeneric("conf.matrix")
  }
)

#' @export
#' @rdname conf.matrix 
setMethod(
  f = "conf.matrix",
  signature = signature(pred="Prediction", relative="logical"),
  def = function(pred, relative) {
    return(errormatrix(pred["truth"], pred@df$response, relative=relative))
  }
)
