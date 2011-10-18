
setGeneric(
  name = "makeVarCostMeasure",
  def = function(id, fun) {
    if (missing(id))
      id = "costs"
#    if (missing(minimize))
#      minimize = TRUE
    standardGeneric("makeVarCostMeasure")
  }
)

setMethod(
  f = "makeVarCostMeasure",
  signature = signature(id="character", fun="function"),
  def = function(id="costs", fun) {
    makeMeasure(id="varcosts", minimize=TRUE, classif=TRUE, regr=TRUE, 
      allowed.pred.types=c("response", "prob"), extra.args=list(fun),  
      fun=function(task, model, pred, extra.pars) {
        fun = extra.pars[[1]]
        v = model@vars 
        fun(v)
      }
    )
  }
)
