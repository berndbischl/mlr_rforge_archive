descriptive <- function(bench, data, plots=c("beanplot"), algorithms=dimnames(bench)[[2]], col=rainbow(length(algorithms))) {
	bench2 = bench[,algorithms,,data]
	#class(bench2) = "bench"
	#op = par(mfrow=c(length(plots),1))
	op = par(mfrow=c(2,2))
	
	for (p in plots) {
		if ("boxplot"==p)
			boxplot(bench2, col=col)
		if ("density"==p) {
			densitychart(as.bench(bench2), col=col, lwd=2)
			legend("topright", legend=algorithms, lty=1, lwd=2, col=col)
		}
		if ("beanplot"==p) {
			mm = melt(bench2)
			colnames(mm)[2] = "alg"
			beanplot(value~alg, data=mm, col=as.list(col))
		}
	}
	par(op)
}