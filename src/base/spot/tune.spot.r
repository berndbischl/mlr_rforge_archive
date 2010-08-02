tune.spot = function (learner, task, resampling, measures, aggr, control) {
	path = "."
	spotMlrWriteRoi(path=path)
	spot("spotMlr.conf")
}