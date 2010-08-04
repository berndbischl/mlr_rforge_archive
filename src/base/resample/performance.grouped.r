#' @include prediction.grouped.r
roxygen()

#' @rdname performance

setMethod(
		f = "performance",
		signature = c(pred="grouped.prediction", measures="list", losses="list", aggr="list"),
		def = function(pred, measures, aggr, losses, task) {
			preds = as.list(pred)
			grp.perf = lapply(preds, function(p) performance(p, measures=measures, losses=losses, task=task))
			gp = lapply(grp.perf, function(x) x$measures)
			ms = as.data.frame(Reduce(rbind, gp))
			rownames(ms) = NULL
			# we only have one aggregate function for groups
			aggr = aggr[[1]]
			ms2 = aggr(ms, pred["group"])
			ms = cbind(group=names(preds), ms)
			ls = callNextMethod(pred=pred, measures=list(), losses=losses, aggr=list(), task=task)$losses
			if (length(losses) > 0)
				return(list(measures=ms, aggr=ms2, losses=ls))
			else
				return(list(measures=ms, aggr=ms2))
	}
)

