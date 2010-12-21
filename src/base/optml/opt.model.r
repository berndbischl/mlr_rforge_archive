#' @include wrapped.model.r
roxygen()
#' @include opt.result.r
roxygen()

setClass(
  "opt.model",
  contains = c("wrapped.model"),
  representation = representation(
    opt.result = "opt.result"
  )
)

setMethod(
  f = "initialize",
  signature = signature("opt.model"),
  def = function(.Object, learner, model, data.desc, task.desc, prep.control, subset, vars, time, opt.result) {
    .Object@opt.result = opt.result
    callNextMethod(.Object, learner, model, data.desc, task.desc, prep.control, subset, vars, time)
  }
)

#' @rdname to.string

setMethod(
  f = "to.string",
  signature = signature("opt.model"),
  def = function(x) {
    s = callNextMethod(x)
    op = x@opt.result["par"]
    op = paste(names(op), op, sep="=", collapse=" ")
    return(
      paste(
        s, "\n",
        "Opt. pars: ", op,
        sep=""
      )
    )
  }
)



