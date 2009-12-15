#' @include task.classif.r
#' @include task.regr.r

setClass(
		"loss",
		representation = representation(
				name = "character",
				fun = "function",
				aggregate = "function",
				aggr.name = "character",
				spread = "function",
				spread.name = "character"
		)
)


setClass(
		"mce.costs",
		contains = c("loss"),
		representation = representation(
				costs = "matrix"
		)
)

make.loss <- function(name, aggregate=NULL, aggr.name="aggr", spread=NULL, spread.name="spread") {
	if (name=="sae") {
		ms <- list(fun = function(true.y, pred.y, weights) sum(abs(true.y - pred.y)),
				aggregate = sum, aggr.name="sum", spread=IQR, spread.name="IQR", minimize=TRUE)
	} else if (name=="abs") {
		ms <- list(fun = function(true.y, pred.y, weights) median(abs(true.y - pred.y)),
				aggregate = median, aggr.name="median", spread=IQR, spread.name="IQR", minimize=TRUE)
	} else if (name=="squared") {
		ms <- new("loss", name=name, fun=function(true.y, pred.y, weights) (true.y - pred.y)^2, 
				aggregate=mean, aggr.name="mean", spread=sd, spread.name="sd")
		#	} else if (name=="smce") {
#		ms <- new("loss", name, function(true.y, pred.y, weights) as.numeric(true.y != pred.y), sum, "sum", sd, "sd")
	} else if (name=="zero-one") {
		ms <- new("loss", name=name, fun=function(true.y, pred.y, weights) as.numeric(true.y != pred.y), 
				aggregate=mean, aggr.name="mean", spread=sd, spread.name="sd")
	} else if (name=="mber") {
		ms <- list(fun = function(true.y, pred.y, weights) {
					em <- errormatrix(true.y, pred.y, relative =TRUE)
					n <- ncol(em)
					return(sum(em[1:(n-1), n]))
				},
				aggregate = mean, aggr.name="mean", spread=sd, spread.name="sd", minimize=TRUE)
	} else {
		stop(paste("Measure", name, "does not exist!"))
	}
	if (!is.null(aggregate)) {
		ms$aggregate <- aggregate
		ms$aggr.name <- aggr.name
	}
	if (!is.null(spread)) {
		ms$spread <- spread
		ms$spread.ane <- spread.name
	}
	return(ms)
}

setGeneric(
		name = "default.loss",
		def = function(learn.task) {
			standardGeneric("default.loss")
		}
)

setMethod(
		f = "default.loss",
		signature = c(learn.task="classif.task"),
		def = function(learn.task) {
			return(make.loss("zero-one"))
		}
)

setMethod(
		f = "default.loss",
		signature = c(learn.task="regr.task"),
		def = function(learn.task) {
			return(make.loss("squared"))
		}
)






