#' Set threshold of prediction object (if prediction was for classification). 
#' Creates corresponding discrete class response for the newly set threshold. 
#' For binary classification: The positive class is predicted if the probability or decision value exceeds the threshold. 
#' For multiclass: Probabilities/decision values are divided by corresponding thresholds and the class with maximum resulting value is selected. 
#' 
#' @param pred [\code{\linkS4class{prediction}}] \cr
#'   Prediction object.
#' @param threshold [numeric] \cr
#'   Threshold to produce class labels. For multiclass classification has to be a named vector, where names correspond to class labels.
#'   For binary classification it should be a single numerical value, but the former format for multiclass is also allowed. 
#' 		    
#' @return \code{\linkS4class{prediction}} with changed threshold and corresponding response.
#' @exportMethod set.threshold
#' @title Set threshold of prediction object.
#' @rdname set.threshold 

setGeneric(
  name = "set.threshold",
  def = function(pred, threshold) {
    standardGeneric("set.threshold")			
  }
)

#' @rdname set.threshold 
setMethod(
  f = "set.threshold",
  
  signature = signature(
    pred = "prediction", 
    threshold = "numeric" 
  ),
  
  def = function(pred, threshold) {
    td = pred@desc
    if (!td["is.classif"])
      stop("Threshold can only be set for classification predictions!")
    if (pred["type"] != "prob")
      stop("Threshold can currently only be set for type 'prob'!")
    levs = td["class.levels"]
    if (length(levs) == 2 && is.numeric(threshold) && length(threshold) == 1) {
      threshold = c(threshold, 1-threshold)
      names(threshold) = c(td["positive"], td["negative"])
    }
    p = pred["prob", class=levs]
    # resort so we have same order in threshold and p
    threshold = threshold[levs] 
    resp = sapply(1:nrow(p), function(i) vote.max.val(p[i,]/threshold, levs))
    resp = factor(resp, levels=levs)
    pred@df$response = resp
    pred@threshold = threshold
    return(pred)
  } 
)



