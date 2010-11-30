#' Calculates confusion matrix for (possibly resampled) prediction. 
#' Rows indicate true classes, columns predicted classes.
#' 
#' @param result [\code{\linkS4class{prediction}}] \cr
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
  signature = signature(pred="prediction", relative="logical"),
  def = function(pred, relative) {
    return(errormatrix(result["truth"], result["response"], relative=relative))
  }
)
