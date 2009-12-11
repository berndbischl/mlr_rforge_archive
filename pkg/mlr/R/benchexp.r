#' @export 
bench.exp <- function(wrapped.learners, task, resampling) {
	wrapped.learners = as.list(wrapped.learners)
	bs = matrix(-1, nrow=resampling["iters"], ncol=length(wrapped.learners))
	for (i in 1:length(wrapped.learners)) {
		wl = wrapped.learners[[i]]
		tp = benchmark(wl, task, resampling)$test.perf
		bs[,i] = tp		
	}
	#bs = as.bench(list(bs))
	return(bs)
}