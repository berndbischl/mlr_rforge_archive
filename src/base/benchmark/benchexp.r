#' Complete benchmark experiment for a learn task.
#' Allows you to compare a list of learning algorithms by measuring the test error w.r.t. to a given resampling strategy.  
#' Experiments are paired, meaning always the same training / test sets are used for the different learners.  
#' 
#' @param learners [\code{\link{list}} of \code{\linkS4class{wrapped.learner}} or \code{\link{character}}] \cr
#' 		  Defines the learning algorithms which should be compared.
#' @param task [\code{\linkS4class{learn.task}}] \cr
#'        Learning task.
#' @param resampling [\code{\linkS4class{resample.desc}} or \code{\linkS4class{resample.instance}}] \cr
#'        Resampling strategy. 
#' 
#' @return A matrix of test error. Columns correspond to learners, row to the iteration of the resampling strategy.
#' 
#' @usage bench.exp(learners, task, resampling)
#' 
#' @note You can also get automatic, internal tuning by using \code{\link{make.tune.wrapper}} with your learner. 
#' 
#' @seealso \code{\link{benchmark}}, \code{\link{make.tune.wrapper}} 
#' @export 
#' @aliases bench.exp 
#' @title Bencnmark experiment for multiple learners 
#'
#' @examples
#' ct <- make.classif.task(data=iris, target="Species")
#' # very small grid for svm hyperpars 
#' r <- list(C=2^seq(-1,1), sigma=2^seq(-1,1))
#' inner.res <- make.cv.desc(iters=3)   
#' svm.tuner <- make.tune.wrapper("kernlab.svm.classif", method="grid", resampling=inner.res, control=grid.control(ranges=r))
#' learners <- c("lda", "qda", svm.tuner)
#' res <- make.cv.instance(iters=5, size=nrow(iris))
#' bench.exp(learners, ct, res)
#'  
#' @title bench.exp

bench.exp <- function(learners, task, resampling) {
	if (length(learners) == 1) {
		learners = list(learners)
	}
	learners = as.list(learners)
	bs = matrix(-1, nrow=resampling["iters"], ncol=length(learners))
	learner.names <- sapply(learners, function(x) { 
		if(is(x, "character"))
			return(x)
		if(is(x, "tune.wrapper"))
			x = x@base.learner
		return(class(x))
	})
	colnames(bs) = learner.names
	tuned = as.list(rep(NA, length(learners)))
	cms = as.list(rep(NA, length(learners)))
	for (i in 1:length(learners)) {
		wl = learners[[i]]
		bm = benchmark(wl, task, resampling)
		bs[,i] = bm$result$test.perf
		if (is(wl, "tune.wrapper"))
			tuned[[i]] = bm$result
		if (is(task, "classif.task"))
			cms[[i]] = bm$conf
	}
	names(tuned) = learner.names
	names(cms) = learner.names
	# reduce to non-list for convenience
	if (length(learners) == 1) {
		tuned=tuned[[1]] 
		cms=cms[[1]]
	}
	if (is(task, "classif.task"))
		return(list(perf = bs, tuned.pars=tuned, conf.mat=cms))
	else
		return(list(perf = bs, tuned.pars=tuned))
	}