#' @include task.classif.r
#' @include task.regr.r

setGeneric(
		name = "make.default.measure",
		def = function(learn.task) {
			standardGeneric("make.default.measure")
		}
)

setMethod(
		f = "make.default.measure",
		signature = c(learn.task="classif.task"),
		def = function(learn.task) {
			return(make.measure("mmce"))
		}
)

setMethod(
		f = "make.default.measure",
		signature = c(learn.task="regr.task"),
		def = function(learn.task) {
			return(make.measure("mse"))
		}
)

#' @export
make.measure <- function(name, aggregate=NULL, aggr.name="aggr", spread=NULL, spread.name="spread") {
	if (name=="sae") {
		ms <- list(fun = function(true.y, pred.y, weights) sum(abs(true.y - pred.y)),
				aggregate = sum, aggr.name="sum", spread=IQR, spread.name="IQR", minimize=TRUE)
	} else if (name=="mae") {
		ms <- list(fun = function(true.y, pred.y, weights) median(abs(true.y - pred.y)),
				aggregate = median, aggr.name="median", spread=IQR, spread.name="IQR", minimize=TRUE)
	} else if (name=="sse") {
		ms <- list(fun = function(true.y, pred.y, weights) sum((true.y - pred.y)^2),
				aggregate = sum, aggr.name="sum", spread=sd, spread.name="sd", minimize=TRUE)		
	} else if (name=="mse") {
		ms <- list(fun = function(true.y, pred.y, weights) mean((true.y - pred.y)^2),
				aggregate = mean, aggr.name="mean", spread=sd, spread.name="sd", minimize=TRUE)		
	} else if (name=="smce") {
		ms <- list(fun = function(true.y, pred.y, weights) sum(true.y != pred.y),
				aggregate = sum, aggr.name="sum", spread=IQR, spread.name="IQR", minimize=TRUE)
	} else if (name=="mmce") {
		ms <- list(fun = function(true.y, pred.y, weights) mean(true.y != pred.y),
				aggregate = mean, aggr.name="mean", spread=sd, spread.name="sd", minimize=TRUE)
	} else if (name=="mber") {
		ms <- list(fun = function(true.y, pred.y, weights) {
					em <- errormatrix(true.y, pred.y, realtive=TRUE)
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


	
