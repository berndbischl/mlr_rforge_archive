
spotMlrAlgStart <- function(io.apdFileName, io.desFileName, io.resFileName){
	source(io.apdFileName,local=TRUE)
	## read doe/dace etc settings:
	des <- read.table(io.desFileName
			, sep=" "
			, header = TRUE
	);
	config<-nrow(des);
	attach(des)
	
	for (k in 1:config){
		if(des$REPEATS[k]>=1){
			for (i in 1:des$REPEATS[k]){
				parset = des
				cns = setdiff(colnames(des), c("CONFIG", "REPEATS", "STEP", "SEED", "repeatsLastConfig")) 
				parset= parset[, cns, drop=F]
				parset = as.list(parset[k,,drop=F])
				conf <- k
				if (exists("CONFIG")){
					conf <- des$CONFIG[k]
				}
				spotStep<-NA
				if (exists("STEP")){
					spotStep <- des$STEP[k]
				}
				seed <- des$SEED[k]+i
				ct = make.task(data=iris, target="Species")
				wl = make.learner("classif.ksvm")
				res = make.res.desc("holdout")
				p = resample.fit(wl, ct, res, parset=parset)
				perf = performance(p, measures="mmce")
				y = perf$aggr["mean", "mmce"]
			
				res = list(Y=y)
				res = c(res, parset)
				res$SEED=seed
				res$CONFIG=conf
				if (exists("STEP")){
					res=c(res,STEP=spotStep)
				} 
				res <-data.frame(res)
				colNames = TRUE
				if (file.exists(io.resFileName)){
					colNames = FALSE
				}
				
				## quote = false is required for JAVA
				write.table(res
						, file = io.resFileName
						, row.names = FALSE
						, col.names = colNames
						, sep = " "              
						, append = !colNames
						, quote = FALSE
				);		
				colNames = FALSE
			} # end for i
		} # end if(des$REPEATS[k]>=1)
	}	#end for k
	detach(des)
}


