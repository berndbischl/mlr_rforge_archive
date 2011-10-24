#' Set threshold of rediction object (if prediction was for classification). 
#' Creates corresponding discrete class response for the newly set threshold. 
#' For binary classification: The positive class is predicted if the probability value exceeds the threshold. 
#' For multiclass: Probabilities are divided by corresponding thresholds and the class with maximum resulting value is selected.
#' The result of both are equivalent if in the multi-threshold case the labels are greater than 0 and sum to 1.  
#' 
#' @param pred [\code{\linkS4class{Prediction}}] \cr
#'   Prediction object.
#' @param threshold [\code{numeric}] \cr
#'   Threshold to produce class labels. Has to be a named vector, where names correspond to class labels.
#'   Only if \code{pred} is a prediction object resulting from binary classification
#'   it can be a single numerical threshold for the positive class. 
#' 		    
#' @return \code{\linkS4class{Prediction}} with changed threshold and corresponding response.  
#' @exportMethod setThreshold
#' @title Set threshold of prediction object.
#' @rdname setThreshold 

setGeneric(
  name = "setThreshold",
  def = function(pred, threshold) {
    standardGeneric("setThreshold")			
  }
)

#' @rdname setThreshold 
setMethod(
  f = "setThreshold",
  
  signature = signature(
    pred = "Prediction", 
    threshold = "numeric" 
  ),
  
  def = function(pred, threshold) {
    td = pred@task.desc
    if (td@type != "classif")
      stop("Threshold can only be set for classification predictions!")
    if (pred@predict.type != "prob")
      stop("Threshold can only be set for predict.type 'prob'!")
    levs = td@class.levels
    if (length(levs) == 2 && is.numeric(threshold) && length(threshold) == 1) {
      threshold = c(threshold, 1-threshold)
      names(threshold) = c(td@positive, td@negative)
    } 
    if (length(threshold > 1) && !setequal(levs, names(threshold))) 
      stop("Threshold names must correspond to classes!")
    p = getProb(pred, class=levs)
    # resort so we have same order in threshold and p
    threshold = threshold[levs] 
    pred@df$response = factor(max.col(t(t(p) / threshold)), levels=seq_along(levs), labels=levs)
    pred@threshold = threshold
    return(pred)
  } 
)





