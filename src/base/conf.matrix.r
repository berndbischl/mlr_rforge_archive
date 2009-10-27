#' \code{conf.matrix} generates the confusion matrix, see \code{\link[klaR]{errormatrix}}. 
#' 
#' 
#' @param learn.task [\code{\linkS4class{learn.task}}] \cr 
#' 	Specifies the learning task for the problem.
#' @param resample.instance [\code{\linkS4class{resample.instance}}] \cr
#'   Specifies the training and test indices of the resampled data. 
#' @param resample.result [\code{\linkS4class{resample.result}}] \cr
#'   Result of \code{\link{resample.fit}}, i.e. the perfomed predictions.
#' @param relative [logical] \cr 
#' 	If TRUE rows are normalized to show relative frequencies.
#' 
#' @return A confusion matrix.
#' 
#' @export
#' @rdname conf.matrix
#' 
#' @usage conf.matrix(learn.task, resample.instance, resample.result, relative)
#' 
#' @examples 
#' data(iris)
#' ct <- make.classif.task("lda", data=iris, target="Species")
#' rin <- make.cv.instance(size=nrow(iris), iters=10)
#' fit <- resample.fit(ct, resample.instance = rin)
#' conf.matrix(learn.task = ct, resample.instance = rin, resample.result = fit, relative = FALSE)
#' 
#' @seealso \code{\link[klaR]{errormatrix}}
#' 
#' @title conf.matrix


conf.matrix = function(learn.task, resample.instance, resample.result, relative=FALSE) {
	# todo check that its classif.task!!!
	n <- resample.result["iters"]
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
