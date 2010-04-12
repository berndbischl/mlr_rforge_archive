#' @include resample.prediction.r
roxygen()

#' @rdname performance

setMethod(
		f = "performance",
		signature = c(pred="resample.prediction", measures="list", losses="list", aggr="list"),
		def = function(pred, measures, aggr, losses, task) {
			n <- pred["iters"]
			rin <- pred["instance"]
			is = 1:n
			# todo: better use split here?
			ps = as.list(pred)
			perfs = lapply(ps, function(p) performance(p, measures=measures, losses=losses, task=task))
			ms = Reduce(rbind, lapply(perfs, function(x) x$measure))
			# ensure a matrix if we just get a single row in ms
			if (!is.matrix(ms))
				ms = as.matrix(t(ms))
			ms2 = lapply(aggr, function(f) apply(ms, 2, f))
			j = which(names(aggr) == "combine")
			if (length(j) > 0) {
				ms2[[j]] = callNextMethod(pred=pred, measures=measures, losses=list(), aggr=list(), task=task)$measures
			}
			ms2 = Reduce(rbind, ms2)
			if (!is.matrix(ms2))
				ms2 = as.matrix(t(ms2))
			colnames(ms) = names(measures)
			colnames(ms2) = names(measures)
			rownames(ms) = c(is)
			rownames(ms2) = names(aggr)
			
			ls = lapply(is, function (i) {
				cbind(iter=i, perfs[[i]]$losses)
			} )
			ls = as.data.frame(Reduce(rbind, ls))
			
			ms = as.data.frame(ms)
			ms2 = as.data.frame(ms2)
			
			
			if (length(losses) > 0)
				return(list(measures=ms, agg=ms2, losses=ls))
			return(
				list(measures=ms, agg=ms2))
		}
)

