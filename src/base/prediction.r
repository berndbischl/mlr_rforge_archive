
##' @export 
#
#
#setClass("prediction",
#		representation(fitted="ANY")
#)
#
#setClass(
#		"p.regr",
#		contains = "prediction"
#)
#
#setClass(
#  "p.class",
#  contains = "prediction",
#  representation(probs="matrix")
#)
#
#
##----------------- constructor ---------------------------------------------------------
#
#
#make.prediction <- function(fitted) {
#	if (is.numeric(fitted))
#		return(new("p.regr", fitted=fitted))
#	if (is.factor(fitted))
#		return(new("p.class", classes=fitted))
#}
#
#
#setMethod(
#		f = "initialize",
#		signature = "prediction",
#		def = function(.Object, fitted) {
#			.Object@fitted = fitted
#			return(.Object)
#		}
#)
#
#
#
#setMethod(
#  f = "initialize",
#  signature = "p.class",
#  def = function(.Object, learn.task, classes=NULL, probs=NULL) {
#	  if (is.null(probs)) {
#		  .Object@probs = matrix(nrow=0, ncol=0)		
#		  .Object@fitted <- classes
#	} else {
#		.Object@probs <- probs
#		cn = colnames(probs)
#		classes = apply(probs,1, function(x) cn[which.max(x)])
#		.Object@fitted = factor(classes, levels=ct["class.levels"])
#	}
#	return(.Object)
#  }
#)
#
#setMethod(
#		f = "initialize",
#		signature = "p.regr",
#		def = function(.Object, fitted) {
#			callNextMethod(.Object, fitted)
#		}
#)


#----------------- accuracy ---------------------------------------------------------



setGeneric(
		name = "performance",
		def = function(true.y, pred.y, weights, measure, costs) {
			if(missing(weights)) {
				weights <- rep(1, length(true.y))
			}
			if(missing(measure)) {
				if (is.factor(true.y))
					measure <- make.measure("mmce")
				if (is.numeric(true.y))
					measure <- make.measure("mse")
			}
			if(missing(costs))
				costs <- NULL 
			standardGeneric("performance")
		}
)

#' @export 


setMethod(
		f = "performance",
		signature = c(true.y="ANY", pred.y="ANY", weights="numeric", measure="list", costs="ANY"),
		def = function(true.y, pred.y, weights, measure, costs) {
			return(measure$fun(true.y, pred.y, weights))
		}
)

setMethod(
		f = "performance",
		signature = c(true.y="ANY", pred.y="ANY", weights="numeric", measure="character", costs="ANY"),
		def = function(true.y, pred.y, weights, measure, costs) {
			return(make.measure(measure)$fun(true.y, pred.y, weights))
		}
)



