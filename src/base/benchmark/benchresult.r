
setClass(
		"bench.result",                                                     
		representation = representation(
				task.descs = "list",
				data.descs = "list",
				resamplings = "list",
				perf = "array",
				tuned.pars = "list", 
				conf.mats = "list",
				resample.fits = "list"
		)
)




#' Getter.
#' @param x bench.result object
#' @param i [character]
#' \describe{
#'   \item{perf}{Performance matrix.}
#'   \item{tuned.pars}{Values of tuned paramters. NA if no tuning was done.}
#'   \item{conf.mats}{Confusion matrices - only for classification.}
#' }
#' @param j [integer or character] \cr Select subset of learners.
#' 
#' @rdname getter,learn.task-method
#' @aliases learn.task.getter getter,learn.task-method
#' @title Getter for learn.task

setMethod(
		f = "[",
		signature = signature("bench.result"),
		def = function(x,i,j,...,drop) {
			
			if (!missing(i)) {
				if (i == "iter") {
					return(dim(x@perf)[1] - 1)
				}
				if (i == "learners") {
					return(dimnames(x@perf)[[2]])
				}
				if (i == "measures") {
					return(dimnames(x@perf)[[3]])
				}
				if (i == "tasks") {
					return(dimnames(x@perf)[[4]])
				}
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
				iter = 1:x["iter"]
			aggr = args$aggr
			if (is.null(aggr))
				aggr=default.aggr()
			aggr = make.aggrs(aggr)
			
			if (!missing(i)) {
				if (i == "tuned.pars"){
					if (missing(j))
						j = 1:ncol(x@perf)
					if (length(j) == 1)
						return(x@tuned.pars[[j]])
					else
						return(x@tuned.pars[j])
				}
				if (i == "conf.mat"){
					xs = x@conf.mats[task]
					xs = lapply(xs, function(y) y[learner])
					return(xs)
				}
			}
			p = x@perf[c(iter, "combine"), learner, measure, task, drop=FALSE]
			if (length(aggr) > 0) {
				p = lapply(names(aggr), function(nn) {
						if (nn == "combine")
							g = function(y) y[length(y)]
						else 
							g = function(y) aggr[[nn]](y[1:(length(y))-1])
						apply(p, c(2,3,4), g)
				}) 
				p = Reduce(function(v,w) abind(v,w, along=2), p)
				# combine aggr names with measure names
				dimnames(p)[[2]] = sapply(names(aggr), function(a) paste(a, measure, sep="."))
			}
			return(p)
		}
)





#' Conversion to string.

setMethod(
		f = "to.string",
		signature = signature("bench.result"),
		def = function(x) {
			p = x[aggr=list(mean=mean, sd=sd)]
			p = paste(capture.output(p), collapse="\n")
		}
)


#' Prints the object by calling as.character.

setMethod(
		f = "print",
		signature = signature("bench.result"),
		def = function(x, ...) {
			cat(to.string(x))
		}
)

#' Shows the object by calling as.character.

setMethod(
		f = "show",
		signature = signature("bench.result"),
		def = function(object) {
			cat(to.string(object))
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
		tn = td["name"]
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







