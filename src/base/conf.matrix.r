#' Calculates confusion matrix for test data of a \code{\link{resample.fit}}, 
#' see \code{\link[klaR]{errormatrix}}. 
#' 
#' @param task [\code{\linkS4class{learn.task}}] \cr 
#' 	Specifies the learning task for the problem.
#' @param result [\code{\linkS4class{resample.result}}] \cr
#'   Result of \code{\link{resample.fit}}, i.e. the perfomed predictions.
#' @param relative [logical] \cr 
#' 	If TRUE rows are normalized to show relative frequencies.
#' 
#' @return A confusion matrix.
#' 
#' @export
#' @rdname conf.matrix
#' 
#' @usage conf.matrix(task, result, relative)
#' 
#' @examples 
#' data(iris)
#' ct <- make.classif.task(data=iris, target="Species")
#' res <- make.cv.instance(size=nrow(iris), iters=3)
#' rf <- resample.fit("lda", ct, res)
#' conf.matrix(ct, rf)
#' 
#' @seealso \code{\link[klaR]{errormatrix}}
#' 
#' @title Confusion matrix


conf.matrix = function(task, result, relative=FALSE) {
	# todo check that its classif.task!!!
	n <- result["iters"]
	rin <- result["instance"]
	lev <- task["class.levels"]
	trues <- c()
	preds <- c()
	for(i in 1:n)  {
		trues.i <- get.test.targets(task, rin, i)
		preds.i <- result["fitted", i]
		trues <- c(trues, as.character(trues.i))
		preds <- c(preds, as.character(preds.i))
		# todo what about remove.duplicated		
	}
	trues <- factor(trues, levels=lev)
	preds <- factor(preds, levels=lev)
	return(errormatrix(trues, preds, relative=relative))
}
