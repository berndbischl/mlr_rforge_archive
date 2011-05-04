#' Control object for MCO variable selection. 
#' 
#' @exportClass VarselMCOControl
#' @title Control object for MCO variable selection.

setClass(
  "VarselControl",
  contains = c("OptControl"),
  representation = representation(
    control = ""    
  )
)

#' Constructor.
setMethod(
  f = "initialize",
  signature = signature("VarselMCOControl"),
  def = function(.Object, path, same.resampling.instance) {
    .Object = callNextMethod(.Object, path, same.resampling.instance)
    return(.Object)
  }
)

setMethod(f = "show",  signature = signature("VarselMCOControl"), def = function(object) {
    cat(
      "Control object for varselMCO\n",
      "Same resampling instance: ", x@same.resampling.instance, "\n",
      sep=""
    )
})

