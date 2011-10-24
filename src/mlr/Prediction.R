#' Result from \code{\link{predict}}. 
#' Use \code{as.data.frame} to access all information in a convenient format.   
#' 
#' Getter.\cr
#' 
#' \describe{
#'  \item{predict.type [\code{character(1)}]}{Type set in predict function: "response" or "prob".}
#'  \item{id [numeric]}{Vector of index numbers of predicted cases from the task.}
#'  \item{response [numeric | factor]}{Predicted response values.}
#'  \item{truth [numeric | factor]}{True target values.}
#'  \item{prob [numeric | matrix] Optional parameters: class}{Predicted probabilities. For binary class: Only the probabilities for the positive class are returned.}
#'  \item{threshold [numeric]}{Threshold set in predict function.}
#' }
#' 
#' @exportClass Prediction
#' @title Prediction.
#' @seealso \code{\link{performance}}


#todo: roxygen does not like long lines?
#Predicted probabilities. If it's a binary problem only the probabilities for the positive class are returned. With "class" you can specifically select which columns of the prob matrix should be returned. Columns names of the returned matrix are always the respective class labels.

setClass(
		"Prediction",
		representation = representation(
				predict.type = "character",
				df = "data.frame",
				threshold = "numeric",
				task.desc = "TaskDesc",
				time = "numeric"
		)
)

#' Constructor.

setMethod(
		f = "initialize",
		signature = signature("Prediction"),
		def = function(.Object, task.desc, predict.type, df, threshold, time) {
			if (missing(df))
				return(make.empty(.Object))
			.Object@predict.type = predict.type			
			.Object@df = df			
			.Object@threshold = threshold			
			.Object@task.desc = task.desc	
			.Object@time = time			
			return(.Object)
		}
)


makePrediction = function(task.desc, id, truth, predict.type, y, time) {
	xs = list()
	# if null no col in df present
	xs[["id"]] = id
	xs[["truth"]] = truth
  if (predict.type == "response") {
    xs[["response"]] = y
  } else if (predict.type == "prob"){
		xs[["prob"]] = y
  }
	df = as.data.frame(xs)
  
  # fix columnnames for prob if strage chars are in factor levels
	# todo: review this!
  cns = colnames(df)
	i = grep("prob.", cns)
	if (length(i) > 0)
		colnames(df)[i] = paste("prob.", colnames(xs[["prob"]]), sep="")

  cns = colnames(df)
	
  if (predict.type == "prob") {
    th = rep(1/length(task.desc@class.levels), length(task.desc@class.levels))
    names(th) = task.desc@class.levels
    p = new("Prediction", task.desc, predict.type, df, th, time)
    return(setThreshold(p, th))
  } else {
    return(new("Prediction", task.desc, predict.type, df, as.numeric(NA), time))
  }  
}

#'  Convert to data.frame
#' @rdname Prediction-class 
#' @export
setMethod(
		f = "as.data.frame",
		signature = signature("Prediction"),
		def = function(x, row.names = NULL, optional = FALSE,...) {
			return(x@df)
		}
)


setMethod("show", "Prediction", function(object) {
  cat(
    "Prediction\n",
    paste(capture.output(str(as.data.frame(object))), collapse="\n"), 
    "\n", sep=""
  )
})

#' Get probabilities for some classes.
#' @param pred [\code{\linkS4class{Prediction}}] 
#'   Prediction object.
#' @param class [character] 
#'   Names of classes. Default is either all classes for multi-class problems or the positive class for binary classification.
#' @return Data.frame with numerical columns or a numerical vector if length of \code{class} is 1. 
#'   Order of columns is defined by \code{class}.
#' @exportMethod getProb
#' @rdname getProb

setGeneric(name = "getProb", 
  def = function(pred, class) {
    check.arg(pred, "Prediction")
    if (pred@task.desc@type != "classif")
      stop("Prediction was not generated from a ClassifTask!")
    if (missing(class)) {
      if (length(pred@task.desc@class.levels) == 2)
        class = pred@task.desc@positive
      else
        class = pred@task.desc@class.levels
    }
    standardGeneric("getProb")
})

#' @rdname getProb
setMethod(f = "getProb", 
  signature = signature("Prediction", "character"), 
  def = function(pred, class) {
    if (pred@predict.type != "prob")
      stop("Probabilities not present in Prediction object!")
    cns = colnames(pred@df)
    class2 = paste(pred@predict.type, class, sep=".")
    if (!all(class2 %in% cns))
      stop("Trying to get scores for nonexistant classes:", paste(class, collapse=","))
    y = pred@df[, class2]
    if (length(class) > 1)
      colnames(y) = class
    return(y)
})

#c.Prediction = function(...) {
#	preds = list(...)
#	id = Reduce(c, lapply(preds, function(x) x@id))
#	response = Reduce(c, lapply(preds, function(x) x@response))
#	target = Reduce(c, lapply(preds, function(x) x@target))
#	weights = Reduce(c, lapply(preds, function(x) x@weights))
#	prob = Reduce(rbind, lapply(preds, function(x) x@prob))
#	return(new("Prediction", task.desc=preds[[1]]@desc, id=id, response=response, target=target, weights=weights, prob=prob));
#}
