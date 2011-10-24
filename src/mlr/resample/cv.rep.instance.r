#' @include ResampleInstance.R
#' @include RepCVDesc.R
roxygen()



setClass(
		"repcv.instance", 
		contains = c("ResampleInstance")
)                                                     

setMethod(
		f = "initialize",
		signature = signature("repcv.instance"),
		def = function(.Object, desc, size, task) {
      folds = desc@iters/desc@reps
      d = makeResampleDesc("CV", iters=folds)
			i = replicate(desc@reps, makeResampleInstance(d, size=size), simplify=FALSE)
			train.inds = Reduce(c, lapply(i, function(j) j@train.inds))
      test.inds = Reduce(c, lapply(i, function(j) j@test.inds))
      g = as.factor(rep(1:desc@reps, each=folds))
      callNextMethod(.Object, desc=desc, size=size, train.inds=train.inds, test.inds=test.inds, group=g)
		}
)
