#' @include LearnTask.R
#' @include OptWrapper.R

benchmark_par = function(ind, learners, tasks, resamplings, measures, conf.mat, models) {
	i = ind[1]
	j = ind[2]
  .mlr.benchmark(learners[[i]], tasks[[j]], resamplings[[j]], measures, conf.mat, models)
}


#' Helper fucntion for bench.exp. Internal use. 
#' 
#' @seealso \code{\link{bench.exp}} 
#' @export 
#' @title Helper fucntion for bench.exp. Internal use.

.mlr.benchmark = function(learner, task, resampling, measures, conf.mat, models) {
	
	if (is.character(learner)) {
		learner = makeLearner(learner)
	}
	
	logger.info(paste("bench.exp: task=", task@desc@id, " learner=", learner@id))
	
	if (missing(measures))
		measures = default.measures(task)
  if (is(measures, "Measure"))
    measures = list(measures)   
	
	extract = function(m){}
	if (is(learner, "OptWrapper")) {
		extract = function(m) m@opt.result
	}

	
	rr = resample(learner, task, resampling, measures=measures, models=models, extract=extract)
	
	ors = NULL
  if (is(learner, "OptWrapper"))
    ors = rr$extract
  return(list(res.result=rr, ors=ors))
}



