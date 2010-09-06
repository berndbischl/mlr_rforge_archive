tune.spot = function (learner, task, resampling, measures, aggr, control) {
  require(SPOT)
	path = "."
  spotMlrWriteConf(path)
	spotMlrWriteRoi(path, control)
  ee = new.env()
  ee$learner = learner
  ee$task = task
  ee$resampling = resampling
  ee$measures = measures
  ee$aggr = aggr
  ee$control = control
  assign(".spotMlr", ee, envir=.GlobalEnv)
	spot("spotMlr.conf")
  stop("done")
}

