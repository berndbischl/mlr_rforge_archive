#' @include resample.fit.r
roxygen()

#' \code{tune} optimizes the hyperparameters of a learner for a classification or regression problem. 
#' Given some ranges for one or more hyperparameters, it estimates the performance 
#' of the learner for each possible combination of the proposed values by 
#' using a resampling method (e.g. cross-validation) and returns the best parameter set and its 
#' performance. 
#'
#' @param learn.task [\code{\linkS4class{learn.task}}] \cr
#'   	Specifies the learning task for the problem.
#' @param resample.instance [\code{\linkS4class{resample.instance}}] \cr
#'   	Specifies the training and test indices of the resampled data. 
#' @param ranges [\code{\link{list}}] \cr 
#' 		A list of named vectors of possible values for each hyperparameter. 
#'      You can also pass a list of such list if it does not make sense to search a complete 
#'      cross-product of range values.     
#' @param measure [\code{\link{character}}/\code{\link{list}}] \cr 
#' 		Name of performance measure to optimize or a list describing your own performance measure. 
#' 		The default is mean misclassification error. 
#' 	   
#'             
#' @return A list containing the best parameter set, its aggregated performance over all resampling 
#' iterations, a measure of spread and a data frame containing the same values for all evaluated 
#' combinations of parameter values. 
#' 
#' @export
#' 
#' @usage tune(learn.task, resample.instance, ranges, measure = "mmce")
#'
#' @examples
#' library(mlr) 
#' ct <- make.classif.task(learner="kernlab.svm.classif", data=iris, formula=Species~.)
#' ranges <- list(kernel="rbfdot", C=2^seq(-1,1), sigma=2^seq(-1,1))
#' ri <- make.cv.instance(size=nrow(iris), iters=3)  
#' tune(learn.task=ct, resample.instance=ri, ranges=ranges)
#'  
#' @title tune


tune <- function(learn.task, resample.instance, ranges, measure) {
	if (missing(measure))
		measure <- make.default.measure(learn.task)
	if (is.character(measure))
		measure <- make.measure(measure)
	# if ranges are list of lists 
	if(all(sapply(ranges, is.list))) {
		trs <- lapply(ranges, function(r) tune.1(learn.task=learn.task, resample.instance=resample.instance, ranges=r, measure=measure))
		perf <- Reduce(rbind.fill, trs)
	}else {
		perf <- tune.1(learn.task=learn.task, resample.instance=resample.instance, ranges=ranges, measure=measure)
	}
	return(make.tune.result(perf, measure))  
}



row2parset <- function (row){
	row <- as.list(row)
	row <- lapply(row, function(x) {
				x.n <- suppressWarnings(as.numeric(x))
				ifelse(is.na(x.n), as.character(x), x.n)
			})
}

tune.1 <- function(learn.task, resample.instance, ranges, measure) {
	grid <- expand.grid(ranges, KEEP.OUT.ATTRS = FALSE)
	# expand grid converts char vectors to factors
	for (i in 1:ncol(grid))
		if(is.factor(grid[,i])) grid[,i] <- as.character(grid[,i])
	
	parsets <- lapply(1:nrow(grid), function(i) row2parset(grid[i,,drop=F]))	
	
	
	wrapper <- function(i) {
		caller <- "tune"
		resample.result <- resample.fit(learn.task, resample.instance, parsets[[i]])
		cp <- resample.performance(learn.task=learn.task, resample.instance=resample.instance, resample.result=resample.result, measure=measure)
		return(c(cp$aggr, cp$spread))
	}
	
	if (.parallel.setup$mode == "snowfall") {
		sfExport("learn.task")
		sfExport("resample.instance")
		if (.parallel.setup$level == "tune") {
			sfExport("parsets")
			sfExport("measure")
		}
	} 
	
	if (.parallel.setup$mode == "snowfall" && .parallel.setup$level == "tune") {
		perf <- sfSapply(1:nrow(grid), wrapper)
	} else {
		perf <- sapply(1:nrow(grid), wrapper)
	}
	
	performances <- grid
	performances$aggr <- perf[1,] 
	performances$spread <- perf[2,]
	return(performances)
}

make.tune.result <- function(perf, measure) {
	if (measure$minimize)
		best.i <- which.min(perf$aggr)
	else
		best.i <- which.max(perf$aggr) 
	cn <- colnames(perf)
	cn <- setdiff(cn, c("aggr", "spread"))
	best.parameters <- row2parset(perf[best.i, cn, drop=F])
	best.performance <- perf[best.i, "aggr"] 
	best.spread <- perf[best.i, "spread"] 
	# put aggr/spread at end
	perf <- perf[, c(cn, "aggr", "spread")]
	# more informative names 
#	ag <- paste("aggr=", measure$aggr.name, sep="") 
#	sp <- paste("spread=", measure$spread.name, sep="")
	ag <- measure$aggr.name 
	sp <- measure$spread.name
	colnames(perf)[ncol(perf)-1] <- ag
	colnames(perf)[ncol(perf)]   <- sp
	return(list(best.parameters=best.parameters, best.performance=best.performance, best.spread=best.spread, performances = perf))
}

