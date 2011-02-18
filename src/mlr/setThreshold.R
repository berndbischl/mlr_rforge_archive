#' Set threshold of rediction object (if prediction was for classification). 
#' Creates corresponding discrete class response for the newly set threshold. 
#' For binary classification: The positive class is predicted if the probability or decision value exceeds the threshold. 
#' For multiclass: Probabilities/decision values are divided by corresponding thresholds and the class with maximum resulting value is selected.
#' The result of both are equivalent if in the multi-threshold case the labels are greater than 0 and sum to 1.  
#' 
#' @param pred [\code{\linkS4class{Prediction}} | double matrix] \cr
#'   Prediction object or matrix of probabilities. If a matrix, column names have to be class labels.
#' @param threshold [numeric] \cr
#'   Threshold to produce class labels. Has to be a named vector, where names correspond to class labels.
#'   Only if pred is a prediction object resulting from binary classification
#'   it can be a single numerical threshold for the positive class. 
#' 		    
#' @return Either a \code{\linkS4class{Prediction}} with changed threshold and corresponding response,  
#'   or a factor if \code{pred} was a matrix. 
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
    p = getScore(pred, class=levs)
    # resort so we have same order in threshold and p
    threshold = threshold[levs] 
    resp = sapply(1:nrow(p), function(i) vote.max.val(p[i,]/threshold, levs))
    resp = factor(resp, levels=levs)
    pred@df$response = resp
    pred@threshold = threshold
    return(pred)
  } 
)

#' @rdname setThreshold 
setMethod(
  f = "setThreshold",
  
  signature = signature(
    pred = "matrix", 
    threshold = "numeric" 
  ),
  
  def = function(pred, threshold) {
    levs = colnames(pred)
    if (length(threshold) != length(levs) || !all.els.named(threshold) || any(names(threshold) != levs))
      stop("Threshold must have same names as columns of prob matrix!")
    # resort so we have same order in threshold and p
    threshold = threshold[levs] 
    resp = sapply(1:nrow(pred), function(i) vote.max.val(pred[i,]/threshold, levs))
    factor(resp, levels=levs)
  } 
)


