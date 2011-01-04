#' @include task.learn.r
#' @include opt.wrapper.r

benchmark_par = function(ind, learners, tasks, resampling, measures, conf.mat, models) {
	i = ind[1]
	j = ind[2]
  .mlr.benchmark(learners[[i]], tasks[[j]], resampling, measures, conf.mat, models)
}


#' Helper fucntion for bench.exp. Internal use. 
#' 
#' @seealso \code{\link{bench.exp}} 
#' @export 
#' @title Helper fucntion for bench.exp. Internal use.

.mlr.benchmark = function(learner, task, resampling, measures, conf.mat, models) {
	
	if (is.character(learner)) {
		learner = make.learner(learner)
	}
	
	logger.info(paste("bench.exp: task=", task["id"], " learner=", learner["id"]))
	
	if (missing(measures))
		measures = default.measures(task)
  if (is(measures, "measure"))
    measures = list(measures)   
	
	extract = function(m){}
	if (is(learner, "opt.wrapper")) {
		extract = function(m) x["opt.result"]
	}

	
	rr = resample(learner, task, resampling, measures=measures, models=models, extract=extract)
	
	ors = NULL
  if (is(learner, "opt.wrapper"))
    ors = rr$extract
  return(list(res.result=rr, ors=ors))
}

