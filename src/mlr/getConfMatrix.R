#' Calculates confusion matrix for (possibly resampled) prediction. 
#' Rows indicate true classes, columns predicted classes.
#' @title Confusion matrix.
#' @param pred [\code{\linkS4class{Prediction}}] \cr
#'   Result of a prediction.
#' @param relative [\code{logical(1)}]\cr 
#' 	If TRUE rows are normalized to show relative frequencies.
#' @return A confusion matrix.
#' @export
#' @seealso \code{\link[klaR]{errormatrix}}
getConfMatrix = function(pred, relative=FALSE) {
  checkArg(pred, "Prediction")
  checkArg(relative, "logical", len=1L, na.ok=FALSE)
  errormatrix(pred@df$truth, pred@df$response, relative=relative)
}
