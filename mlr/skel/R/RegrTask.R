
setMethod(
		f = "initialize",
		signature = signature("RegrTask"),
		def = function(.Object, id, data, weights, blocking, target) {
				
			if (missing(data))
        return(make.empty(.Object))
      
      td = new("TaskDesc", data, target, "regr", id, 
        length(weights) > 0, length(blocking) > 0, as.character(NA))      
      
			callNextMethod(.Object, data=data, weights=weights, blocking=blocking, task.desc=td)
		}
)



setMethod("show", "RegrTask", function(object) {
  td = object@desc
  data = getData(object)
  feat = printToChar(object@desc@n.feat)
  cat(
    "Regression problem ", td@id, "\n",
    "Features:\n", feat, "\n", 
    "Observations: ", td@size , "\n",
    "Missings: ", td@has.missing, "\n", 
    "Infinites: ", td@has.inf, "\n", 
    "Target: ", td@target, "\n", 
    "Has weights: ", td@has.weights, "\n", 
    "Has blocking: ", td@has.blocking, "\n",
    sep=""
  )
})


