tune.grid <- function(learner, task, resampling, loss, control) {
	ranges = control$ranges
	# if theres more than one ranges 
	if(all((names(ranges) == "ranges"))) {
		trs <- lapply(ranges, function(r) {tune.1(learner, task, resampling, r, loss)})
		trs2 <- lapply(1:length(ranges), function(i) make.tune.result(trs[[i]], loss, ranges[[i]]))
		ps <- lapply(trs2, function(x) x$all.perfs)
		bps <- sapply(trs2, function(x) x$perf)
		bss <- sapply(trs2, function(x) x$spread)
		bpars <- lapply(trs2, function(x) x$par)
		print(bps)
		i <- which.min(bps)
		perf <- Reduce(rbind.fill, ps)
		# reorder
		cn <- colnames(perf)
		perf <- perf[, c(setdiff(cn, c("mean", "sd")), "mean", "sd")]
		return(list(best.parameters=bpars[[i]], best.performance=bps[i], best.spread=bss[i], performances = perf))
	}else {
		tr <- tune.1(learner, task, resampling, ranges, loss)
		return(make.tune.result(tr, loss, ranges))
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

tune.1 <- function(learner, task, resampling, ranges, loss) {
	check.ranges(ranges)

	cr <- convert.ranges(ranges)
	grid.labels <- expand.grid(cr$labels, KEEP.OUT.ATTRS = FALSE)
	grid.indices <- expand.grid(cr$indices, KEEP.OUT.ATTRS = FALSE)
	
	# expand grid converts char vectors to factors
#	for (i in 1:ncol(grid.labels))
#		if(is.factor(grid.labels[,i])) grid[,i] <- as.character(grid[,i])
	
	parsets <- lapply(1:nrow(grid.indices), function(i) row2parset(grid.indices[i,,drop=F], ranges))	
	
#	if (.ps$mode %in% c("snowfall", "sfCluster")) {
#		sfExport("learner")
#		sfExport("task")
#		sfExport("resample.instance")
#		if (.ps$level == "tune") {
#			sfExport("parsets")
#			sfExport("measure")
#		}
#	} 
	
	perf = eval.parsets(parsets, names(ranges), resampling)

	performances <- grid.indices
	performances[, c("aggr", "spread", "time")]  <- perf[,1:3] 
	return(performances)
}

make.tune.result <- function(perf, loss, ranges) {
	cr <- convert.ranges(ranges)
	best.i <- which.min(perf$aggr)
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
	#ag <- measure$aggr.name 
	#sp <- measure$spread.name
	ag = "mean" 
	sp = "sd" 
	colnames(perf2)[ncol(perf)-2] <- ag
	colnames(perf2)[ncol(perf)-1]   <- sp
	return(list(par=best.parameters, perf=best.performance, all.perfs = perf2))
}

convert.ranges <- function(ranges) {
	indices <- lapply(ranges, function(x) 1:length(x))
	labels <- lapply(ranges, function(x) if(length(names(x)) > 0) {return(names(x))} else {as.character(x)})
	return(list(indices=indices, labels=labels))
}



