#' @include resample.instance.r
#' @include cv.rep.desc.r
roxygen()



setClass(
		"repcv.instance", 
		contains = c("resample.instance.nonseq")
)                                                     

setMethod(
		f = "initialize",
		signature = signature("repcv.instance"),
		def = function(.Object, desc, size, task) {
      d = make.res.desc("cv", iters=desc["iters"])
			i = replicate(desc["reps"], make.res.instance(d, size=size), simplify=FALSE)
			train.inds = Reduce(c, lapply(i, function(j) j@train.inds))
      test.inds = Reduce(c, lapply(i, function(j) j@test.inds))
      g = as.factor(rep(1:desc["reps"], each=desc["iters"]))
      callNextMethod(.Object, desc=desc, size=size, train.inds=train.inds, test.inds=test.inds, group=g)
		}
)

setMethod(
  f = "to.string",
  signature = signature("repcv.instance"),
  def = function(x) {
    return(
      paste(
        "Instance for ", x["desc"]["id"],  " with ", x["desc"]["iters"], " iterations, ", x["desc"]["reps"], " repetitions and ", x["size"], " cases",
        sep=""
      )
    )
  }
)

