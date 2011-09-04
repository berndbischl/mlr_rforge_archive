#todo: getter for predictions?

#' Container for the results of a benchmark experiment.
#' 
#' Getter. \cr
#' 
#' The "perf" getter is probably the most common one, it returns a list of 3 dim. arrays of performance values for every data set.
#' The dimension are: learners, resampling iterations and measures.
#' You can reduce the list or the contained array by using the optional arguments "task", "learner", "Measure", "iter" and "aggr". 
#' "task" and "learners" must be set to a char vector of IDs repectively, "measure" to names of recorded performance measures in the experiment,
#' "iter" to an integer vector of selected resampling interations. The default for these is to select everything. 
#' "aggr" can be used to aggregate the results accross the resampling interations (see \code{\link{aggregations}}). 
#' The default is not to do any aggregation. You can also set "aggr" to "resampling" which does the default aggregation 
#' of the used resampling stratgegy.    
#' 
#' The following getters all return list of lists of objects: aggrs, predictions, conf.mats
#' The first list iterates the tasks, the second one the learners, both are named by respective IDs.
#' You can reduce these lists by using the optional arguments 'task' and 'learner'. 
#' 
#' The following getters all return list of lists of lists: models, opt.results
#' The first list iterates the tasks, the second one the learners, both are named by respective IDs, the third list iterates the
#' resampling iterations. You can reduce these lists by using the optional arguments 'task' and 'learner'. 
#' 
#' \describe{
#' 	 \item{iters [numeric]}{Named numerical vector which lists the number of iterations for every task. Names are IDs of task.}
#'   \item{aggrs [see above] }{List of list of aggregated performance vectors for every task/learner. }
#'   \item{conf.mats [see above] }{List of list of confusion matrices for every task/learner. }
#' 	 \item{predictions [see above] }{List of list of predictions for every task/learner. }
#'   \item{models [see above] }{List of list of list of models for every task/learner/iteration. Entry is NULL if no models were saved.}
#' 	 \item{opt.results [see above] }{List of list of list of \code{\linkS4class{opt.result}} for every task/learner/iteration. Entry is NULL if no optimization was done.}
#' }
#' 
#' @rdname BenchResult-class
#' @exportClass BenchResult
#' @title bench-result
#' @seealso \code{\link{benchmark}}


setClass(
		"BenchResult",
		representation = representation(
				task.descs = "list",
        learners = "list",
        resamplings = "list",
        measures = "list",
				res.results = "list",
				opt.results = "list", 
        input.names = "list"
		)
)


#'  Convert to array of performance values.
#' @rdname BenchResult-class 
#' @export
setMethod(
  f = "as.array",
  signature = signature("BenchResult"),
  def = function(x, tasks=names(x@task.descs), learners=names(x@learners), sets=c("test", "train"), measures=names(x@measures), drop=FALSE, ...) {
    iters = unique(iters(x))
    if (length(iters) != 1)
      stop("Resamplings in benchmark have different numbers of iterations, restrict as.array to a single task!")
    dimns = list(1:iters, sets, learners, measures, tasks)
    y = array(NA, dim=sapply(dimns, length), dimnames=dimns)
    # be sure to remove iter column
    for (j in 1:length(tasks)) {
      for (i in 1:length(learners)) {
        for (k in sets) {
          if (k == "test")
            y[,k,i,,j] = as.matrix(x@res.results[[tasks[j]]][[learners[i]]]$measures.test[,measures])
          else
            y[,k,i,,j] = as.matrix(x@res.results[[tasks[j]]][[learners[i]]]$measures.train[,measures])
        }
      }
    }
    return(y[,,,,,drop=drop])
  }
)


#' Extract tuned parameters for one learner from a \code{\linkS4class{BenchResult}}. 
#' 
#' @param br [\code{\linkS4class{BenchResult}}]\cr 
#'   Result of benchmark experiment.   
#' @param task.id [\code{character(1)}]\cr 
#'   Id of task used in \code{br}. If there was only one task, this argument can be missing.    
#' @param learner.id [\code{character(1)}]\cr 
#'   Id of tuned learner used in \code{br}. If there was only one learner, this argument can be missing.    
#' @param as.data.frame [logical(1)]\cr 
#'   Should the result be returned as a data.frame or a list of lists? Default is \code{TRUE}.    
#'        
#' @return For data.frame: Columns correspond to parameter ids, rows to resampling iterations, 
#'   every row contains the best settings found in this resampling iteration. For list: A list of list, the outer list has as many
#'   elements as resampling iterations. The ith elemnt of this list are the best found parameter settings found in iteration i as a list.   
#' @exportMethod getTunedParameters
#' @title Extract tuned parameters from BenchResult.
#' @rdname getTunedParameters

setGeneric(
  name = "getTunedParameters",
  def = function(br, task.id, learner.id, as.data.frame) {
    check.arg(br, "BenchResult")
    tns = names(br@task.descs)
    lns = names(br@learners)
    if (missing(task.id) && length(tns)==1)
      task.id = tns
    if (missing(learner.id) && length(lns)==1)
      learner.id = lns
    if (missing(as.data.frame))
      as.data.frame=TRUE
    standardGeneric("getTunedParameters")
  }
)

#' @rdname getTunedParameters 
setMethod(
  f = "getTunedParameters",
  signature = signature(br="BenchResult", task.id="character", learner.id="character", as.data.frame="logical"),
  def = function(br, task.id, learner.id, as.data.frame) {
    tns = names(br@task.descs)
    lns = names(br@learners)    
    if (!(task.id %in% tns))
      stop("Task id ", task.id, " was not used in BenchResult, only: ", paste(tns, collapse=","))
    if (!(learner.id %in% lns))
      stop("Learner ", learner.id, " was not used in BenchResult, only: ", paste(lns, collapse=","))
    x = br@opt.results[[task.id]][[learner.id]]
    if (is.null(x) || !is(x[[1]]@control, "TuneControl"))
      stop("Learner id ", learner.id, " was not tuned in BenchResult!")
    if (as.data.frame)
      as.data.frame(Reduce(rbind, lapply(x, function(y) as.data.frame(y@x))))
    else
      lapply(x, function(y) y@x)
  } 
)

#' Extract optimized features for one learner from a \code{\linkS4class{BenchResult}}. 
#' 
#' @param br [\code{\linkS4class{BenchResult}}]\cr 
#'   Result of benchmark experiment.   
#' @param task.id [\code{character(1)}]\cr 
#'   Id of task used in \code{br}. If there was only one task, this argument can be missing.    
#' @param learner.id [\code{character(1)}]\cr 
#'   Id of learner with variable selection used in \code{br}. If there was only one learner, this argument can be missing.    
#' @return For data.frame: Integer columns correspond to feature/bit names, rows to resampling iterations, 
#'   every row contains the best bits found in this resampling iteration. For list: A list of character vectors, the outer list has as many
#'   elements as resampling iterations. The ith element of this list is best vector of bits/features found in iteration i.   
#' @exportMethod getSelectedFeatures
#' @title Extract optimized features from BenchResult.
#' @rdname getSelectedFeatures

setGeneric(
  name = "getSelectedFeatures",
  def = function(br, task.id, learner.id, as.data.frame) {
    check.arg(br, "BenchResult")
    tns = names(br@task.descs)
    lns = names(br@learners)
    if (missing(task.id) && length(tns)==1)
      task.id = tns  
    if (missing(learner.id) && length(lns)==1)
      learner.id = lns
    if (missing(as.data.frame))
      as.data.frame=TRUE
    standardGeneric("getSelectedFeatures")
  }
)

#' @rdname getSelectedFeatures 
setMethod(
  f = "getSelectedFeatures",
  signature = signature(br="BenchResult", task.id="character", learner.id="character", as.data.frame="logical"),
  def = function(br, task.id, learner.id, as.data.frame) {
    tns = names(br@task.descs)
    lns = names(br@learners)
    if (!(task.id %in% tns))
      stop("Task id ", task.id, " was not used in BenchResult, only: ", paste(tns, collapse=","))
    if (!(learner.id %in% lns))
      stop("Learner ", learner.id, " was not used in BenchResult, only: ", paste(lns, collapse=","))
    x = br@opt.results[[task.id]][[learner.id]]
    if (is.null(x) || !is(x[[1]]@control, "VarselControl"))
      stop("Learner id ", learner.id, " was not used for varsel in BenchResult!")
    if (as.data.frame) {
      avs = br["input.names"][[task.id]]
      as.data.frame(Reduce(rbind, init=NULL, lapply(x, function(y) vars.to.binary(y@x, all.vars=avs))))
    } else
      lapply(x, function(y) y@x)
  } 
)

setMethod(f = "show", signature = signature("BenchResult"), def = function(object) {
  s = ""
  tt = names(x@task.descs)
  ll = names(x@learners)
  k = length(x@res.results[[1]][[1]]$aggr)
  y = ""
  for (a in tt) {
    w = matrix(NA, nrow=length(ll), ncol=k)
    for (j in 1:length(ll)) {
      w[j, ] = x@res.results[[a]][[j]]$aggr
    }
    rownames(w) = ll
    colnames(w) = names(x@res.results[[a]][[1]]$aggr)
    w = paste(capture.output(w), collapse="\n")
    y = paste(y, "\n\n", a, "\n", w, sep="")
  }
  cat(y)
})

