#' @export
conf.matrix = function(learn.task, resample.instance, resample.result, relative=FALSE) {
	# todo check that its classif.task!!!
	n <- resample.result["iters"]
	res.i <- learn.task["resampled"]
	rin <- resample.instance
	lev <- learn.task["class.levels"]
	trues <- c()
	preds <- c()
	for(i in 1:n)  {
		trues.i <- get.test.targets(learn.task, resample.instance, i)
		preds.i <- resample.result["fitted", i]
		trues <- c(trues, as.character(trues.i))
		preds <- c(preds, as.character(preds.i))
		# todo what about remove.duplicated		
	}
	trues <- factor(trues, levels=lev)
	preds <- factor(preds, levels=lev)
	return(errormatrix(trues, preds, relative=relative))
}
