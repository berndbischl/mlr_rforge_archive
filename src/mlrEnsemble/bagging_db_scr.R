# TODO: Add comment
###############################################



dist.db.models <- function(df=df1, target=target, class=class, pars=list(), parts=parts, cpu=cpu, rep=rep, l=0)
{
	
	wrapper <- function(x, tar, learn)
	{
		tt = make.task(target=tar, data=x)
		m <- train(learn, task=tt)
		return(m)
	}
	
	size <- length(df[,1])
	teil <- floor(size/parts)
	
	teile <- c(0,teil*(1:(parts-1)),size)
	
	if(l>0 && l<=1){
		dp <- list()
		for(k in seq(0 ,rep*(parts-1) ,parts))
		{
			zufall <- sample(1:size, size)	
			for(i in 1:parts) 
			{
				dp[[i+k]] <- df[zufall[(teile[i]+1):(teile[i+1])],] 
				dp[[i+k]] <- rbind(dp[[i+k]], dp[[i+k]][1:floor(teil*l),])
			}
		}
	}
	
	else{
		dp <- list()
		for(k in seq(0 ,rep*(parts-1) ,parts))
		{
			zufall <- sample(1:size, size)	
			for(i in 1:parts) 
			{
				dp[[i+k]] <- df[zufall[(teile[i]+1):(teile[i+1])],] 				
			}
		}
	}
	
	learner <- make.learner(class, par.vals=pars)
	
	sfInit(parallel=T, cpus=cpu, type="SOCK")     
	sfLibrary(mlr)
	
	model <- list()
	model <- sfClusterApplyLB(dp, wrapper, tar=target, learn=learner)
	
	sfStop()
	
	return(model)
}



dist.db.pred <- function(model=model, df=df2, cpu=cpu, target=target)
{	
	pred <- list()
	task <- make.task(target=target, data=df)
	
	sfInit(parallel=T, cpus=cpu, type="SOCK")     
	sfLibrary(mlr)
	
	
	pred <- sfClusterApplyLB(model, predict, task=task)
	
	return(pred)
	sfStop()
}	


dist.db.bagging <- function(df1=df1, df2=df2, class=class, pars=list(), parts=parts, 
		cpu=cpu, target=target, rep=rep, l=0)
{
	model <- dist.db.models(df=df1, target=target, class=class, pars=pars, parts=parts, cpu=cpu, rep=rep, l=l)
	pred <- dist.db.pred(model=model, df=df2, cpu=cpu, target=target)
	sfStop()
	frames <- parts*rep
	
	resp <- list()
	for(i in 1:frames)
	{
		resp <- cbind(resp, pred[[i]]["response"])
	}
	
	resp.ave <- numeric()
	for(i in 1:length(resp[,1])){
		resp.ave[i] <- as.numeric(rownames(as.matrix(which.max(table(as.numeric(resp[i,]))))))
	}
	
	
	return(resp.ave)
}



