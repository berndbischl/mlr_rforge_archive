
spotMlrWriteConf = function(
	path, 
	alg.seed = 1235, 
	spot.seed = 125, 
	auto.loop.steps = 100,
	init.design.func = "spotCreateDesignLhd",
	init.design.size = 10,
	init.design.repeats  = 1,
	seq.merge.func = mean,
	seq.design.size = 250,
	seq.predictionModel.func = "spotPredictRandomForest"
) {
	txt = list(
		alg.language = "sourceR",
		alg.path="bin",
		alg.func = "spotMlrAlgStart",
		alg.seed = alg.seed,
		spot.seed = spot.seed,
		auto.loop.steps = auto.loop.steps,
		init.design.func = "spotCreateDesignLhd",
		init.design.size = init.design.size,
		init.design.repeats  = init.design.repeats,
		seq.merge.func = "mean",
		seq.design.size = seq.design.size,
		seq.predictionModel.func = "spotPredictRandomForest",
		io.verbosity=3,
		io.columnSep = " "

	)
	txt = sapply(txt, function(x) ifelse(is.character(x), paste("\"", x, "\"", sep=""), as.character(x)))
	txt=paste(names(txt), txt, sep="=")
	txt=paste(txt, ";")
	f = file.path(path, "mlr.conf")
	writeLines(txt, con=f)
}

#spotMlrWriteConf(path=".")

