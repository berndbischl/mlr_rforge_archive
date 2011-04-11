#' @include OptResult.R
roxygen()

#' @importClassesFrom mlr WrappedModel
setClass(
  "OptModel",
  contains = c("WrappedModel"),
  representation = representation(
    opt.result = "OptResult"
  )
)

setMethod(
  f = "initialize",
  signature = signature("OptModel"),
  def = function(.Object, learner, model, task.desc, subset, vars, time, opt.result) {
    .Object@opt.result = opt.result
    callNextMethod(.Object, learner, model, task.desc, subset, vars, time)
  }
)


setMethod(f = "show",  signature = signature("OptModel"), def = function(object) {
  callNextMethod(object)
  cat("\nOptimzation result:\n")
  show(object@opt.result)
})



