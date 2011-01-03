
get.data = function(task, subset, vars, with.target=TRUE, class.as.num=FALSE) {
  tn = task["target"]
  if (missing(vars) && with.target)
    d = task@data[subset, ]
  else {
    if (missing(vars))
      vars = task["input.names"]
    v = if (with.target) c(vars, tn) else vars 
    d = task@data[subset, v]
  }    
  if (task["is.classif"] && with.target && class.as.num)
    d[, tn] = as.numeric(d[, tn] == task["positive"]) 
  return(d)              
}