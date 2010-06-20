#' Container for the results of a benchmark experiment.
#' 
#' Getter. \cr
#' The following getters all return list of lists of objects: perf, prediction, opt.result.
#' The first list iterates the tasks, the second one the learners, both are named by respective IDs.
#' You can reduce these list by using the optional arguments 'task' and 'learner'. 'Drop' is by default TRUE, which means that 
#' the list structures are simplified as much as possible, if you don't want this set 'drop' to FALSE. 
#' 
#' \describe{
#'   \item{learners [character]}{IDs of learners used in experiment.}
#'   \item{tasks [character]}{IDs of tasks used in experiment.}
#'   \item{measures [character]}{Names of measures recorded in experiment.}
#' 	 \item{iters [numeric]}{Named numerical vector which lists the number of iterations for every task. Names are IDs of task.}
#' 	 \item{prediction [see above] }{List of list of predictions for every task/learner. }
#' 	 \item{conf.mat [see above] }{List of list of confusion matrices for every task/learner. }
#' 	 \item{opt.result [see above] }{List of list of  \code{\linkS4class{opt.result}} for every task/learner. Entry is NULL if no optimization was done.}
#' 	 \item{opt.perf [see above] }{List of list of performance vectors of optimal settings for every task/learner. Note that this performance refers to the inner resampling! Entry is NULL if no optimization was done.}
#' 	 \item{opt.par [see above] }{List of list of  \code{\linkS4class{opt.result}} for every task/learner. Entry is NULL if no optimization was done.}
#' 	 \item{opt.path [see above] }{List of list of optimization paths for every task/learner. Entry is NULL if no optimization was done.}
#' 	 \item{tuned.par [see above] }{List of list of optimal hyperparameters for every task/learner. Entry is NULL if no tuning was done.}
#' 	 \item{sel.var [see above] }{List of list of optimal features for every task/learner. Entry is NULL if no feature selection was done.}
#'   \item{perf [list]}{Lists for every data set and for every performance measure the performances of the different learners in each iteration.}
#' }
#' 
#' @rdname bench.result-class
#' @exportClass bench.result
#' @title bench-result
#' @seealso \code{\link{bench.exp}}


setClass(
		"bench.result",
		contains = c("object"),
		representation = representation(
				task.descs = "list",
				data.descs = "list",
				resamplings = "list",
				perf = "list",
				predictions = "list",
				models = "list",
				conf.mats = "list",
				opt.results = "list" 
		)
)

#' @rdname bench.result-class

setMethod(
		f = "[",
		signature = signature("bench.result"),
		def = function(x,i,j,...,drop) {
			mylistdrop = function(y) {
				if(!is.list(y) || length(y) != 1)
					return(y)
				else
					return(mylistdrop(y[[1]]))
			}
			
			mydrop = function(y) {
				if(!drop)
					return(y)
				z = mylistdrop(y)
				rec.lapply(z, function (w) if(is.array(w)) drop(w) else w)
			}
			
			if (i == "iters") {
				return(sapply(x@perf, function(y) return(dim(y)[1] - 1)))
			}
			if (i == "learners") {
				return(dimnames(x@perf[[1]])[[2]])
			}
			if (i == "measures") {
				return(dimnames(x@perf[[1]])[[3]])
			}
			if (i == "tasks") {
				return(names(x@perf))
			}

			
			args = list(...)
			as.data.frame = args$as.data.frame
			
			task = args$task
			if (is.null(task))
				task = x["tasks"]
			learner = args$learner
			if (is.null(learner))
				learner = x["learners"]
			measure = args$measure
			if (is.null(measure))
				measure = x["measures"]
			iter = args$iter
			if (is.null(iter))
				iter = lapply(x["iters"][task], function(y) 1:y)
			aggr = args$aggr
			if (is.null(aggr))
				aggr=list()
			aggr = make.aggrs(aggr)
			
			
			if (i == "prediction"){
				xs = lapply(task, function(y) x@predictions[[y]][learner])
				return(xs)
			}		
			ors = lapply(task, function(y) x@opt.results[[y]][learner])
			if (i == "opt.result"){
				return(mydrop(ors))
			}
			if (i == "opt.par"){
				return(mydrop(rec.lapply(ors, function(y) y["par"])))
			}
			if (i == "tuned.par"){
				return(mydrop(rec.lapply(ors, function(y) y["tuned.par"])))
			}
			if (i == "sel.var"){
				return(mydrop(rec.lapply(ors, function(y) y["sel.var"])))
			}
			if (i == "opt.perf"){
				return(mydrop(rec.lapply(ors, function(y) y["perf"])))
			}
			if (i == "opt.path"){
				return(mydrop(rec.lapply(ors, function(y) y["path", as.data.frame=as.data.frame])))
			}
			if (i == "conf.mat"){
				return(mydrop(lapply(task, function(y) x@conf.mats[[y]][learner])))
			}
			
			if (i == "perf") {
				# reduce to selected tasks
				p = x@perf[task]
				# reduce to selected elements
				if (is.null(aggr$combine))
					g = function(arr, is) arr[is, learner, measure, drop=FALSE]
				else			
					g = function(arr, is) arr[c(is, "combine"), learner, measure, drop=FALSE]
				p = Map(g, p, is=iter)
				# aggregate
				if (length(aggr) > 0) {
					gg = function(arr) {
						lapply(names(aggr), function(nn) {
									if (nn == "combine")
										h = function(y) y[length(y)]
									else {
										# dont choose combine el from array
										last.el = ifelse(is.null(aggr$combine), 0, 1)
										h = function(y) aggr[[nn]](y[1:(length(y)-last.el)])
									}
									t(apply(arr, c(2,3), h))
								})
					}	
					p = lapply(p, gg) 
					# put all aggr. values as columns together
					# combine aggr names with measure names
					aggr.ms = sapply(names(aggr), function(a) paste(a, measure, sep="."))
					p = lapply(p, function(arrs) {
								y = Reduce(rbind, arrs)
								rownames(y) = aggr.ms  
								return(y)
					})
				}
				return(mydrop(p))
			}
			callNextMethod()
		}
)






#' @rdname to.string

setMethod(
		f = "to.string",
		signature = signature("bench.result"),
		def = function(x) {
			p = x["perf", aggr=list(mean=mean, sd=sd)]
			p = paste(capture.output(p), collapse="\n")
		}
)



#' @export

as.ROCR.preds = function(x) {
	if(!require(ROCR)) {
		stop(paste("Package ROCR is missing!"))
	}
	rfs = x@resample.fits
	res = list()
	# tasks
	for (i in 1:length(rfs)) {
		td = x@task.descs[[i]]
		dd = x@data.descs[[i]]
		tn = td["id"]
		if(dd["class.nr"] != 2) {
			stop("Task", tn, "has more than 2 classes!")
		}
		res2 = list()
		# learners
		for (j in 1:length(rfs[[i]])) {
			rf = as.data.frame(rfs[[i]][[j]])
			if (is.null(rf$prob))
				res2[[j]] = NA
			else 				
				res2[[j]] = prediction(predictions=rf$prob, rf$target)
		}
		names(res2) = dimnames(x@perf)[[2]]
		res[[i]] = res2
	}
	names(res) = dimnames(x@perf)[[4]]
	return(res)
} 


### todo: pretty print method for this case: only aggregated values, always the same learners



