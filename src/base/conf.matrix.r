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
#' res <- make.res.desc("cv", iters=3)
#' rf <- resample.fit("lda", ct, res)
#' conf.matrix(ct, rf)
#' 
#' @seealso \code{\link[klaR]{errormatrix}}
#' 
#' @title Confusion matrix


conf.matrix = function(result, relative=FALSE) {
	return(errormatrix(result["truth"], result["response"], relative=relative))
}
