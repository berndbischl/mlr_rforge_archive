#' Add another learner to a benchmark experiment.
#' The same resampling pairing is used as for the other learners.  
#' 
#' @param learner [\code{\linkS4class{wrapped.learner}} or \code{\link{character}}] \cr
#' 		  Added learner.
#' @param task [\code{\linkS4class{learn.task}}] \cr
#'        Learning task.
#' @param result [\code{\linkS4class{bench.result}}] \cr
#'        Result of previous experiment. 
#' 
#' @return [\code{\linkS4class{bench.result}}]
#' 
#' @usage bench.add(learner, task, result)
#' 
#' @seealso \code{\link{bench.exp}} 
#' @export 
#' @title Add learner to benchmark experiment 
#'
#' @examples
#' ct <- make.classif.task(data=iris, target="Species")
#' learners <- c("lda", "qda")
#' res <- make.cv.instance(iters=5, size=nrow(iris))
#' be <- bench.exp(learners, ct, res)
#' be <- bench.add("rpart.classif", task=ct, result=be)


bench.add <- function(learner, task, result) {
	be = bench.exp(learner, task, result@resampling)
	result@perf = cbind(result@perf, be@perf) 
	result@tuned.pars = c(result@tuned.pars, be@tuned.pars)
	result@conf.mats = c(result@conf.mats, be@conf.mats)
	return(result)
}