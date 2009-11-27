



g <- function(x, f, labs) {
	m = nrow(datastore)
	
	#y.noisy = mean(replicate(2, f(x)))
	y.noisy = f(x)
	z = c(x,y.noisy)
	datastore <<- rbind(datastore, z)
	colnames(datastore) <<- labs
	print(datastore[nrow(datastore),])
	
	if (nrow(datastore)>30)
		datastore <<- datastore[-1,]
	
	print(sd(datastore$y))
#	return(y.noisy)
	if (nrow(datastore) >= 25) {
		data2 = cbind(datastore, datastore[,1]^2, datastore[,2]^2, datastore[,1] * datastore[,2])
		m = lm(y~., data=data2)
		ps = summary(m)$coef[,4]
		#if (all(ps > 0.2))
		#	stop("conv")
		y.smooth = predict(m, newdata=data2[nrow(data2),])
		return(y.smooth)
	} 
	return(y.noisy)
}


tune.cmaes <- function(f, start, lower, upper, control) {
	n = length(lower)
	dd = as.data.frame(matrix(0,nrow=0, ncol=n+1))
	colnames(dd) <- c(names(start), "y")
	datastore <<- dd
	res <- cma_es(par=start, fn=g, lower=lower, upper=upper, control=control, f=f, labs=colnames(dd))
	par <- as.list(res$par)
	list(par=par, perf=res$val)
}