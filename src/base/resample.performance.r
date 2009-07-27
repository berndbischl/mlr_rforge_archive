#' @include resample.result.r
roxygen()



setGeneric(
		name = "resample.performance",
		def = function(learn.task, resample.instance, resample.result, measure, remove.duplicated) {
			if (missing(measure))
				measure <- make.default.measure(learn.task)
			if (is.character(measure))
				measure <- make.measure(measure)
			if (missing(remove.duplicated))
				remove.duplicated <- TRUE
			standardGeneric("resample.performance")
		}
)

#' @export

setMethod(
		f = "resample.performance",
		signature = c(learn.task="learn.task", resample.instance="resample.instance", resample.result="resample.result", measure="list", remove.duplicated="logical"),
		def = function(learn.task, resample.instance, resample.result, measure, remove.duplicated) {
			n <- resample.result["iters"]
			res.i <- learn.task["resampled"]
			rin <- resample.instance
			perf <- numeric(n)
			for(i in 1:n)  {
				trues.i <- get.test.targets(learn.task, rin, i)
				preds.i <- resample.result["fitted", i]
				w.i <- learn.task@weights[rin["test.inds", i]]
				if (remove.duplicated && length(res.i)>0) {
					train.inds <- rin["train.inds", i]
					test.inds <- rin["test.inds", i]
					f1 <- res.i[train.inds]
					f2 <- res.i[test.inds]
					remove.i <- f2 %in% f1
					trues.i <- trues.i[!remove.i]
					preds.i <- preds.i[!remove.i]
					w.i <- w.i[!remove.i] 
				}
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

