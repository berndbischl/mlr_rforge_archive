
#' Container for the results of a benchmark experiment.
#' @exportClass bench.result

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
				opts = "list", 
				paths = "list" 
		)
)

#' Getter.
#' @param x bench.result object
#' @param i [character]
#' \describe{
#'   \item{perf}{Performance matrix.}
#'   \item{tuned.pars}{Values of tuned parameters. NA if no tuning was done.}
#'   \item{conf.mats}{Confusion matrices - only for classification.}
#' }
#' @param j [integer or character] \cr Select subset of learners.
#' 
#' @rdname bench.result-class

setMethod(
		f = "[",
		signature = signature("bench.result"),
		def = function(x,i,j,...,drop) {

			
			if (i == "iters") {
				return(lapply(x@perf, function(y) return(dim(y)[1] - 1)))
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
			arg.names = names(args)
			
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
			
			
			if (i == "opt"){
				xs = lapply(task, function(y) x@opts[[y]][learner[[y]]])
				return(xs)
			}
			if (i == "path"){
				xs = lapply(task, function(y) x@paths[[y]][learner[[y]]])
				return(xs)
			}
			if (i == "conf.mat"){
				xs = lapply(task, function(y) x@conf.mats[[y]][learner[[y]]])
				return(xs)
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
				#print(str(p))			
				# aggregate
				if (length(aggr) > 0) {
					g = function(arr) {
						lapply(names(aggr), function(nn) {
									if (nn == "combine")
										h = function(y) y[length(y)]
									else 
										h = function(y) aggr[[nn]](y[1:(length(y))-1])
									t(apply(arr, c(2,3), h))
								})
					}	
					p = lapply(p, g) 
					# put all aggr. values as columns together
					# combine aggr names with measure names
					aggr.ms = sapply(names(aggr), function(a) paste(a, measure, sep="."))
					p = lapply(p, function(arrs) {
								y = Reduce(rbind, arrs)
								rownames(y) = aggr.ms  
								return(y)
					})
				}
				return(p)
			}
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



