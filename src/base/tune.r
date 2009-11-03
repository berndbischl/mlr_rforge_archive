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
#' 		A list of named vectors/lists of possible values for each hyperparameter. 
#'      You can also pass a list of such ranges by using [\code{\link{combine.ranges}}] 
#'      in the rare case when it does not make sense to search a complete cross-product of range values.     
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
#' @usage tune(learn.task, resample.instance, ranges, measure)
#'
#' @examples
#' library(mlr) 
#' ct <- make.classif.task(learner="kernlab.svm.classif", data=iris, target="Species")
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
	# if theres more than one ranges 
	if(all((names(ranges) == "ranges"))) {
		trs <- lapply(ranges, function(r) {tune.1(learn.task=learn.task, resample.instance=resample.instance, ranges=r, measure=measure)})
		trs2 <- lapply(1:length(ranges), function(i) make.tune.result(trs[[i]], measure, ranges[[i]]))
		ps <- lapply(trs2, function(x) x$performances)
		bps <- sapply(trs2, function(x) x$best.performance)
		bss <- sapply(trs2, function(x) x$best.spread)
		bpars <- lapply(trs2, function(x) x$best.parameters)
		i <- which.min(bps)
		perf <- Reduce(rbind.fill, ps)
		# reorder
		cn <- colnames(perf)
		perf <- perf[, c(setdiff(cn, c(measure$aggr.name, measure$spread.name)), measure$aggr.name, measure$spread.name)]
		return(list(best.parameters=bpars[[i]], best.performance=bps[i], best.spread=bss[i], performances = perf))
	}else {
		tr <- tune.1(learn.task=learn.task, resample.instance=resample.instance, ranges=ranges, measure=measure)
		return(make.tune.result(tr, measure, ranges))
	}
}



row2parset <- function (indices.row, ranges){
	indices.row <- as.integer(indices.row)
	result <- as.list(rep(0,length(indices.row)))
	names(result) <- names(ranges)
	for (i in 1:length(indices.row)) {
		result[[i]] <- (ranges[[i]])[[indices.row[i]]]
	}
	return(result)
}

tune.1 <- function(learn.task, resample.instance, ranges, measure) {
	check.ranges(ranges)
	
	cr <- convert.ranges(ranges)
	grid.labels <- expand.grid(cr$labels, KEEP.OUT.ATTRS = FALSE)
	grid.indices <- expand.grid(cr$indices, KEEP.OUT.ATTRS = FALSE)
		
	# expand grid converts char vectors to factors
#	for (i in 1:ncol(grid.labels))
#		if(is.factor(grid.labels[,i])) grid[,i] <- as.character(grid[,i])
	
	parsets <- lapply(1:nrow(grid.indices), function(i) row2parset(grid.indices[i,,drop=F], ranges))	
	
	
	wrapper <- function(i) {
		caller <- "tune"
		st <- system.time(resample.result <- resample.fit(learn.task, resample.instance, parsets[[i]]))
		cp <- resample.performance(learn.task=learn.task, resample.result=resample.result, measure=measure)
		return(c(cp$aggr, cp$spread, st["elapsed"]))
	}
	
	.ps <- .mlr.local$parallel.setup
	
	if (.ps$mode %in% c("snowfall", "sfCluster")) {
		sfExport("learn.task")
		sfExport("resample.instance")
		if (.ps$level == "tune") {
			sfExport("parsets")
			sfExport("measure")
		}
	} 
	
	if (.ps$mode %in% c("snowfall", "sfCluster") && .ps$level == "tune") {
		perf <- sfClusterApplyLB(1:nrow(grid.indices), wrapper)
	} else {
		perf <- lapply(1:nrow(grid.indices), wrapper)
	}
	perf <- Reduce(rbind, perf)
	colnames(perf) <- c("aggr", "spread", "time")
	rownames(perf) <- NULL
	performances <- grid.indices
	performances$aggr <- perf[,1] 
	performances$spread <- perf[,2]
	performances$time <- perf[,3]
	return(performances)
}

make.tune.result <- function(perf, measure, ranges) {
	cr <- convert.ranges(ranges)
	if (measure$minimize)
		best.i <- which.min(perf$aggr)
	else
		best.i <- which.max(perf$aggr)
	cn <- colnames(perf)
	cn <- setdiff(cn, c("aggr", "spread", "time"))
	perf2 <- perf[,cn, drop=FALSE]
	best.parameters <- row2parset(perf[best.i, cn, drop=F], ranges)
	best.performance <- perf[best.i, "aggr"] 
	best.spread <- perf[best.i, "spread"] 
	# put aggr/spread at end
	perf2 <- sapply(1:ncol(perf2), function(i) cr$labels[[i]][perf[,cn[i]]])
	perf2 <- as.data.frame(perf2)
	colnames(perf2) <- cn
	perf2 <- cbind(perf2, perf[,c("aggr", "spread", "time")])
	# more informative names 
#	ag <- paste("aggr=", measure$aggr.name, sep="") 
#	sp <- paste("spread=", measure$spread.name, sep="")
	ag <- measure$aggr.name 
	sp <- measure$spread.name
	colnames(perf2)[ncol(perf)-2] <- ag
	colnames(perf2)[ncol(perf)-1]   <- sp
	return(list(best.parameters=best.parameters, best.performance=best.performance, best.spread=best.spread, performances = perf2))
}

convert.ranges <- function(ranges) {
	indices <- lapply(ranges, function(x) 1:length(x))
	labels <- lapply(ranges, function(x) if(length(names(x)) > 0) {return(names(x))} else {as.character(x)})
	return(list(indices=indices, labels=labels))
}



