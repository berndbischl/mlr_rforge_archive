
#
#seed    random seed (e.g. 12345)
#steps   maximum number of evolution steps (e.g. 10000)
#target  objective function threshold for priliminary evolution end (e.g. 0.0001)
#f       objective function class name (e.g. TODO)
#n       problem dimension (e.g. 12)
#sigma0  initial step size (e.g. 1.0)
#a       step size muliplier (e.g. 1.2239)
#g       history length ( e.g. 12 == n)

spotMlrAlgStart <- function(io.apdFileName, io.desFileName, io.resFileName){
  writeLines(paste("Loading design file data from::", io.desFileName), con=stderr());
  writeLines("ES run...", con=stderr());
  ## read doe/dace etc settings:
  print(io.desFileName)
  des <- read.table(io.desFileName
    , sep=" "
    , header = TRUE
  );
  print(summary(des));
  
  ##  SIGMANULL VARA VARG REPEATS SEED
  config<-nrow(des);
  print(config);
  attach(des)
  
  for (k in 1:config){
    if(des$REPEATS[k]>=1){
      for (i in 1:des$REPEATS[k]){
        
        parset = des
        cns = setdiff(colnames(des), c("CONFIG", "REPEATS", "STEP", "SEED", "repeatsLastConfig")) 
        parset= parset[, cns, drop=FALSE]
        parset = as.list(parset[k,,drop=FALSE])
        conf <- k
        if (exists("CONFIG")){
          conf <- des$CONFIG[k]
        }
        spotStep<-NA
        if (exists("STEP")){
          spotStep <- des$STEP[k]
        }
        seed <- des$SEED[k]+i
        ????????? par.vals
        wl = set.hyper.pars(.spotMlr$learner, par.vals=parset)
        p = resample(wl, .spotMlr$task, .spotMlr$resampling)
        perf = performance(p, measures=.spotMlr$measures, aggr=.spotMlr$aggr)
        # take first measure and aggr to optimize 
        y = perf$aggr[1, 1]
        res = list(Y=y)
        res = c(res, parset)
        res$SEED=seed
        res$CONFIG=conf        
        print(str(res))
        
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
  } #end for k
  detach(des)
}


