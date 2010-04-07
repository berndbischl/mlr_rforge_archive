



varsel.mrmr = function(learner, task, resampling, measures, aggr, method, control=varsel.control()) {
	ds.name = paste("foo", round(runif(1,max=100000000)), ".csv", sep="")
	ds.path = file.path(tempdir(), "foo.csv")
	df = na.omit(task["data"])
	df = df[,c(task["target"], task["input.names"])]
	write.table(df, file = ds.path, sep = ",", row.names=F)
	
	n.feat = control$max.vars
	n.samp = nrow(df)
	method = "MID"
	cmd = paste(mrmr.path, "-i", ds.path, "-n", n.feat, "-s", n.samp, "-m", method)
	#print(cmd)
	out = system(cmd, intern=T)
	i = grep("mRMR features", out)+2
	vars = character(0)
	while( (s = out[i]) != "") {
		v = strsplit(s, "\"")[[1]][2]
		vars = c(vars, v) 
		i = i+1
	}
	
	return(list(best=vars, path=NULL))
}





