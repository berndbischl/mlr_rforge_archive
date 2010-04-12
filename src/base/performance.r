#' @include prediction.r
roxygen()

#' Measures the quality of predictions w.r.t. some loss function.
#' 
#' @param pred [\code{\linkS4class{prediction}}] \cr
#' 		  Prediction object to evaluate.
#' @param measures [see \code{\link{measures}}]
#'        Performance measures. 
#' @param aggr [see \code{\link{aggregations}}]
#'        Aggregation functions. 
#' 		  Ignored if not a \code{\linkS4class{resample.prediction}}
#' @param losses [see \code{\link{losses}}]
#'        Loss functions. 
#' @param task [\code{\linkS4class{learn.task}}]\cr 
#'        Optionally specifies learning task, very rarely needed.
#' 
#' @return A list with with possibly two named components: "measures" is a data.frame of performance values. 
#'         "losses" is a data.frame of losses. 
#' 
#' @exportMethod performance
#' @rdname performance
#' 
#' @usage performance(pred, measures, aggr, losses, task)
#'
#' @title Measure performance and losses of prediction.



setGeneric(
		name = "performance",
		def = function(pred, measures, aggr, losses, task) {
			if (missing(measures))
				measures=default.measures(pred@task.desc)
			measures = make.measures(measures)
			if (missing(losses))
				losses=list()
			losses = make.losses(losses)
			if(missing(aggr))
				aggr = default.aggr(pred@task.desc)
			aggr = make.aggrs(aggr)
			standardGeneric("performance")
		}
)

#' @rdname performance

setMethod(
		f = "performance",
		signature = signature(pred="prediction", measures="list", aggr="list", losses="list"),
		def = function(pred, measures, aggr, losses, task) {
			x = pred
			td = x@task.desc
			dd = x@data.desc
			ms = sapply(measures, function(f) f(x, task=task))	
			if (is.null(pred["id"]))
				ls = lapply(losses, function(f) f(x, task=task))
			else
				ls = lapply(losses, function(f) cbind(id=x["id"], f(x, task=task)))
			
#			if(length(ms[[1]]) != 1)
#				stop("Measure has to return a scalar value!")
			ls = as.data.frame(Reduce(rbind, ls))
			g = function(x) {
				n = attr(x, "name")
				if (is.null(n)) 
					return(NA)
				else 
					return(n)
			}
			names(ms) = sapply(measures, g)
			if (length(losses) > 0) {
				cns = sapply(losses, g)
				if (!is.null(pred["id"]))
					cns = c("id", cns)
				colnames(ls) = cns
			}
			
			if (length(losses) > 0)
				return(list(measures=ms, losses=ls))
			return(list(measures=ms))
		}
)



