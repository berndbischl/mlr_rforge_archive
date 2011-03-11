#' @include WrappedModel.R
roxygen()
#' @include opt.result.r
roxygen()

setClass(
  "opt.model",
  contains = c("WrappedModel"),
  representation = representation(
    opt.result = "opt.result"
  )
)

setMethod(
  f = "initialize",
  signature = signature("opt.model"),
  def = function(.Object, learner, model, task.desc, prep.control, subset, vars, time, opt.result) {
    .Object@opt.result = opt.result
    callNextMethod(.Object, learner, model, task.desc, prep.control, subset, vars, time)
  }
)

#' @rdname to.string

setMethod(
  f = "to.string",
  signature = signature("opt.model"),
  def = function(x) {
    s1 = callNextMethod(x)
    s2 = to.string(x@opt.result)
    paste(s1, "\nOptimzation result:\n", s2, sep="")
  }
)



