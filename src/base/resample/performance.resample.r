#' @include resample.prediction.r
roxygen()

#' @rdname performance

setMethod(
		f = "performance",
		signature = c(pred="resample.prediction", measures="list", losses="list", aggr="list"),
		def = function(pred, measures, aggr, losses, task) {
			rin = pred["instance"]
			preds = as.list(pred)
			is = 1:rin["iters"]
			perfs = lapply(preds, function(p) performance(p, measures=measures, aggr=rin["group.aggr"], losses=losses, task=task))
			# add iter index to measure data.frame
			ms.list = lapply(is, function (i) cbind(iter=i, perfs[[i]]$measures))
			# put together without aggregation
			ms.all = Reduce(rbind, ms.list)
			ms.aggr.group = Reduce(rbind, lapply(perfs, function(x) {if(is.null(x$aggr)) c() else x$aggr}))

			# ensure a matrix if we just get a single row in ms
			if (!is.matrix(ms.aggr.group))
				aggr.group = as.matrix(t(aggr.group))
			ms.aggr = lapply(aggr, function(f) apply(ms.aggr.group, 2, f))
			print(ms.aggr)
			j = which(names(aggr) == "combine")
			if (length(j) > 0) {
				ms2[[j]] = callNextMethod(pred=pred, measures=measures, losses=list(), aggr=list(), task=task)$measures
			}
			ms.aggr = Reduce(rbind, ms.aggr)
			if (!is.matrix(ms.aggr))
				ms.aggr = as.matrix(t(ms.aggr))
			print(ms.aggr)
			colnames(ms.aggr) = names(measures)
			rownames(ms.all) = rownames(ms.aggr.group) = NULL 
			rownames(ms.aggr) = names(aggr)
			
			ls = lapply(is, function (i) cbind(iter=i, perfs[[i]]$losses))
			ls = as.data.frame(Reduce(rbind, ls))
			
			ms.all = as.data.frame(ms.all)
			ms.aggr.group = as.data.frame(ms.aggr.group)
			ms.aggr = as.data.frame(ms.aggr)

			result = list(measures=ms.all, aggr=ms.aggr)	
			if (length(losses) > 0)
				result$losses = ls
			result$aggr.group = ms.aggr.group 
			return(result)
		}
)

