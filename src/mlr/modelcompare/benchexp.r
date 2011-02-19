#' Complete benchmark experiment to compare different learning algorithms
#' across one or more tasks w.r.t. a given resampling strategy.
#' Experiments are paired, meaning always the same training / test sets are used for the different learners.
#' 
#' @param learners [string | \code{\linkS4class{learner}} | list of the previous two] \cr
#'   Defines the learning algorithms which should be compared.
#' @param tasks [\code{\link{LearnTask}} | list of the previous] \cr
#'   Defines the tasks.
#' @param resamplings [resampling desc | resampling instance | list of the previous two] \cr
#'   Defines the resampling strategies for the tasks.
#' @param measures [see \code{\link{measures}}]
#'   Performance measures.
#' @param models [logical] \cr
#'   Should all fitted models be stored?
#'   Default is FALSE.
#' @param same.resampling.instance [logical(1)] \cr
#'   Should the same resampling instance be used for all learners (per task) to reduce variance? Default is \code{TRUE}.
#' @return \code{\linkS4class{bench.result}}.
#'
#' @note You can also get automatic, internal tuning by using \code{\link{makeTuneWrapper}} with your learner.
#'
#' @seealso \code{\link{makeTuneWrapper}}
#' @export
#' @aliases bench.exp
#' @title Benchmark experiment for multiple learners and tasks.


bench.exp <- function(learners, tasks, resamplings, measures, models=FALSE, same.resampling.instance=TRUE)  {
  
  if (!is.list(learners) && length(learners) == 1) {
    learners = list(learners)
  }
  learners = as.list(learners)
  n = length(learners)
  if (n == 0)
    stop("No learners were passed!")
  check.list.type(learners, c("character", "Learner"))
  learners = lapply(learners, function(x) if (is.character(x)) makeLearner(x) else x)
  ids = sapply(learners, function(x) x@id)
  if (any(duplicated(ids)))
    stop("Learners need unique ids!")
  
  if (!is.list(tasks)) {
    tasks = list(tasks)
  }
  if (length(tasks) == 0)
    stop("No tasks were passed!")
  check.list.type(tasks, "LearnTask")
  ids = sapply(tasks, function(x) x@desc@id)
  if (any(duplicated(ids)))
    stop("Tasks need unique ids!")
  
  if (missing(measures))
    measures = default.measures(tasks[[1]])
  if (is(measures, "Measure"))
    measures = list(measures)
  ms.names = sapply(measures, function(m) m@id)
  
  if (is(resamplings, "ResampleInstance") || is(resamplings, "ResampleDesc"))
    resamplings = replicate(length(tasks), resamplings, simplify = FALSE) 
  
  if (length(resamplings) != length(tasks))
    stop("Number of resampling strategies and number of tasks differ!")
  if (same.resampling.instance)
    resamplings = Map(function(res,tt) makeResampleInstance(res,task=tt), resamplings, tasks)
  
  learner.names = character()
  task.names = sapply(tasks, function(x) x@desc@id)   
  tds = ins = rrs = ors = list()
  
  inds = as.matrix(expand.grid(1:length(learners), 1:length(tasks)))
  inds = lapply(1:nrow(inds), function(i) inds[i,])
  results = mylapply(inds, benchmark_par, from="bench", learners=learners, tasks=tasks, resamplings=resamplings,
    measures=measures, models=models)
  
  counter = 1
  for (j in 1:length(tasks)) {
    task = tasks[[j]]
    ins[[j]] = getFeatureNames(task)
    rrs[[j]] = list()
    ors[[j]] = list()
    tds[[j]] = task@desc
    for (i in 1:length(learners)) {
      wl = learners[[i]]
      learner.names[i] = wl@id
      bm = results[[counter]]
      counter = counter+1
      
      rrs[[j]][[i]] = bm$res.result
      if(is(wl, "OptWrapper")) ors[[j]][[i]] = bm$ors else ors[[j]][i] = list(NULL)
    }
    names(rrs[[j]]) = learner.names
    names(ors[[j]]) = learner.names
  }
  names(tds) = task.names
  names(learners) = learner.names
  names(resamplings) = task.names
  names(rrs) = task.names
  names(ors) = task.names
  names(ins) = task.names
  return(new("bench.result", task.descs=tds, learners=learners, resamplings=resamplings,
      measures=measures, res.results = rrs, opt.results = ors, input.names=ins
    ))
}
