#' Result from \code{\link{predict}}. 
#' Use \code{as.data.frame} to access all information in a convenient format.   
#' 
#' Getter.\cr
#' 
#' \describe{
#'  \item{type [string]}{Type set in predict function: "response", "prob", or "decision".}
#'  \item{id [numeric]}{Vector of index numbers of predicted cases from the task.}
#'  \item{response [numeric | factor]}{Predicted response values.}
#'  \item{truth [numeric | factor]}{True target values.}
#'  \item{prob [numeric | matrix] Optional parameters: class}{Predicted probabilities. For binary class: Only the probabilities for the positive class are returned.}
#'  \item{decision [matrix]}{Predicted decision values.}
#'  \item{threshold [numeric]}{Threshold set in predict function.}
#' }
#' 
#' @exportClass prediction
#' @title Prediction.
#' @seealso \code{\link{performance}}


#todo: roxygen does not like long lines?
#Predicted probabilities. If it's a binary problem only the probabilities for the positive class are returned. With "class" you can specifically select which columns of the prob matrix should be returned. Columns names of the returned matrix are always the respective class labels.

setClass(
		"prediction",
		contains = c("object"),
		representation = representation(
				type = "character",
				df = "data.frame",
				threshold = "numeric",
				data.desc = "data.desc",
				task.desc = "task.desc",
				time = "numeric"
		)
)

#' Constructor.

setMethod(
		f = "initialize",
		signature = signature("prediction"),
		def = function(.Object, data.desc, task.desc, type, df, threshold, time) {
			if (missing(df))
				return(make.empty(.Object))
			.Object@type = type			
			.Object@df = df			
			.Object@threshold = threshold			
			.Object@data.desc = data.desc			
			.Object@task.desc = task.desc	
			.Object@time = time			
			return(.Object)
		}
)


make.prediction = function(data.desc, task.desc, id, truth, type, y, time) {
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
    th = rep(1/data.desc["class.nr"], data.desc["class.nr"])
    names(th) = data.desc["class.levels"]
    p = new("prediction", data.desc, task.desc, type, df, th, time)
    return(set.threshold(p, th))
  } else {
    return(new("prediction", data.desc, task.desc, type, df, as.numeric(NA), time))
  }  
}


#' Getter.
#' @rdname prediction-class


setMethod(
		f = "[",
		signature = signature("prediction"),
		def = function(x,i,j,...,drop) {
			args = list(...)
			class = args$class
			
			if (i == "id")
				return(x@df$id)
			if (i == "response")
				return(x@df$response)
			if (i == "truth")
				return(x@df$truth)
			if (i == "iter")
				return(x@df$iter)
			if (i == "prob") {
				cns = colnames(x@df)
				cns = cns[grep("^prob", cns)]
				# prob was not selected as type in predict
				if (length(cns) == 0)
					return(NULL)
				# no class chosen and we are binary: return prob for pos. class
				if (is.null(class) && x@data.desc["is.binary"]) {
					return(x@df[, paste("prob", x@task.desc["positive"], sep=".")])
				}
				if (is.null(class))
					class = x@data.desc["class.levels"]
				cns2 = sapply(strsplit(cns, "prob."), function(z) z[2])
				jj = which(cns2 %in% class)
				y = x@df[, cns[jj]]
				if (is.data.frame(y)) {
          y = as.matrix(y)
					colnames(y) = cns2[jj]
        }
				return(y)
			}
			if (i == "decision") {
				cns = colnames(x@df)
				return(x@df[, grep("^decision", cns)])
			}
			callNextMethod()
		}
)

#'  Convert to data.frame
#' @rdname prediction-class 
#' @export
setMethod(
		f = "as.data.frame",
		signature = signature("prediction"),
		def = function(x, row.names = NULL, optional = FALSE,...) {
			return(x@df)
		}
)


#' @rdname to.string

setMethod(
		f = "to.string",
		signature = signature("prediction"),
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


#c.prediction = function(...) {
#	preds = list(...)
#	id = Reduce(c, lapply(preds, function(x) x@id))
#	response = Reduce(c, lapply(preds, function(x) x@response))
#	target = Reduce(c, lapply(preds, function(x) x@target))
#	weights = Reduce(c, lapply(preds, function(x) x@weights))
#	prob = Reduce(rbind, lapply(preds, function(x) x@prob))
#	decision = Reduce(rbind, lapply(preds, function(x) x@decision))
#	return(new("prediction", data.desc=preds[[1]]@data.desc, task.desc=preds[[1]]@task.desc, id=id, response=response, target=target, weights=weights, prob=prob, decision=decision));
#}
