#' @include resample.result.r
roxygen()



setGeneric(
		name = "resample.performance",
		def = function(learn.task, resample.instance, resample.result, measure) {
			if (missing(measure))
				measure <- make.default.measure(learn.task)
			if (is.character(measure))
				measure <- make.measure(measure)
			standardGeneric("resample.performance")
		}
)

#' @export

setMethod(
		f = "resample.performance",
		signature = c(learn.task="learn.task", resample.instance="resample.instance", resample.result="resample.result", measure="list"),
		def = function(learn.task, resample.instance, resample.result, measure) {
			n <- resample.result["iters"]
			rin <- resample.instance
			perf <- numeric(n)
			for(i in 1:n)  {
				trues.i <- get.test.targets(learn.task, rin, i)
				preds.i <- resample.result["fitted", i]
				w.i <- learn.task@weights[rin["test.inds", i]]
				perf[i] <- performance(preds.i, trues.i, w.i, measure, costs=costs)
			}
			
			perf.aggr = measure$aggregate(perf)
			perf.spread = ifelse(is.na(perf.aggr), NA, measure$spread(perf))
			res <- list(values=perf, aggr=perf.aggr, spread=perf.spread)
			#ag <- measure$aggr.name 
			#sp <- measure$spread.name
			#names(res)[[2]] <- ag
			#names(res)[[3]] <- sp
			return(res)
		}
)

