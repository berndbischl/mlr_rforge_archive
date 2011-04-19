#' Result from \code{\link{predict}}. 
#' Use \code{as.data.frame} to access all information in a convenient format.   
#' 
#' Getter.\cr
#' 
#' \describe{
#'  \item{type [\code{character(1)}]}{Type set in predict function: "response", "prob", or "decision".}
#'  \item{id [numeric]}{Vector of index numbers of predicted cases from the task.}
#'  \item{response [numeric | factor]}{Predicted response values.}
#'  \item{truth [numeric | factor]}{True target values.}
#'  \item{prob [numeric | matrix] Optional parameters: class}{Predicted probabilities. For binary class: Only the probabilities for the positive class are returned.}
#'  \item{decision [matrix]}{Predicted decision values.}
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
		contains = c("object"),
		representation = representation(
				type = "character",
				df = "data.frame",
				threshold = "numeric",
				desc = "TaskDesc",
				time = "numeric"
		)
)

#' Constructor.

setMethod(
		f = "initialize",
		signature = signature("Prediction"),
		def = function(.Object, task.desc, type, df, threshold, time) {
			if (missing(df))
				return(make.empty(.Object))
			.Object@type = type			
			.Object@df = df			
			.Object@threshold = threshold			
			.Object@desc = task.desc	
			.Object@time = time			
			return(.Object)
		}
)


makePrediction = function(task.desc, id, truth, type, y, time) {
	xs = list()
	# if null no col in df present
	xs[["id"]] = id
	xs[["truth"]] = truth
  if (type == "response") {
    xs[["response"]] = y
  } else if (type == "prob"){
		xs[["prob"]] = y
	} else if (type == "decision"){
		xs[["dec"]] = y
	}
	df = as.data.frame(xs)
  
  # fix columnnames for prob if strage chars are in factor levels
	# todo: review this!
  cns = colnames(df)
	i = grep("prob.", cns)
	if (length(i) > 0)
		colnames(df)[i] = paste("prob.", colnames(xs[["prob"]]), sep="")

  cns = colnames(df)
  i = grep("dec.", cns)
  if (length(i) > 0)
    colnames(df)[i] = paste("dec.", colnames(xs[["dec"]]), sep="")
  
	
  if (type != "response") {
    th = rep(1/length(getClassLevels(task.desc)), length(getClassLevels(task.desc)))
    names(th) = getClassLevels(task.desc)
    p = new("Prediction", task.desc, type, df, th, time)
    return(setThreshold(p, th))
  } else {
    return(new("Prediction", task.desc, type, df, as.numeric(NA), time))
  }  
}


#' Getter.
#' @rdname Prediction-class


setMethod(
		f = "[",
		signature = signature("Prediction"),
		def = function(x,i,j,...,drop) {
			args = list(...)
			class = args$class
			
			if (i == "id")
				return(x@df$id)
			if (i == "iter")
				return(x@df$iter)
			if (i == "decision") {
				cns = colnames(x@df)
				return(x@df[, grep("^decision", cns)])
			}
			callNextMethod()
		}
)

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


#' @rdname to.string

setMethod(
		f = "to.string",
		signature = signature("Prediction"),
		def = function(x) {
			return(
					paste(
							"Prediction\n",
							paste(capture.output(str(as.data.frame(x))), collapse="\n"), 
							"\n", sep=""
					)
			)
		}
)



#' Get probabilities or decision values for some classes.
#' @param pred [\code{\linkS4class{Prediction}}] 
#'   Prediction object.
#' @param class [character] 
#'   Names of classes. Default is either all classes for multi-class problems or the positive class for binary classification.
#' @return Data.frame with numerical columns or a numerical vector if length of \code{class} is 1. 
#'   Order of columns is defined by \code{class}.
#' @exportMethod getScore
#' @rdname getScore

setGeneric(name = "getScore", 
  def = function(pred, class) {
    check.arg(pred, "Prediction")
    if (pred@desc@type != "classif")
      stop("Prediction was not generated from a ClassifTask!")
    if (missing(class)) {
      if (length(getClassLevels(pred)) == 2)
        class = pred@desc["positive"]
      else
        class = getClassLevels(pred)
    }
    standardGeneric("getScore")
})

#' @rdname getScore
setMethod(f = "getScore", 
  signature = signature("Prediction", "character"), 
  def = function(pred, class) {
    if (!(pred@type %in% c("prob", "decision")))
      stop("Neither probabilities nor decision values present in Prediction object!")
    cns = colnames(pred@df)
    class2 = paste(pred@type, class, sep=".")
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
#	decision = Reduce(rbind, lapply(preds, function(x) x@decision))
#	return(new("Prediction", task.desc=preds[[1]]@desc, id=id, response=response, target=target, weights=weights, prob=prob, decision=decision));
#}
