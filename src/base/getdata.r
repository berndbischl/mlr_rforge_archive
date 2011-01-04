# todo: mabye optimize for speed for defaault values
# todo: test
get.data = function(task, subset=1:task["size"], vars, target.extra=FALSE, 
  class.as=c("factor", "01", "-1+1")) {
  
  match.arg(class.as)
  if (missing(vars))
    vars = task["input.names"]
  tn = task["target"]
  
  # reduce to subset
  d = task@data[subset,,drop=FALSE]
    
  # maybe recode y
  if (task["is.classif"] && class.as=="01")
    d[, tn] = as.numeric(d[, tn] == task["positive"])
  else if (task["is.classif"] && class.as=="-1+1")
    d[, tn] = 2*as.numeric(d[, tn] == task["positive"])-1
    
  # reduce to vars
  if (target.extra) 
    list(data=d[,vars,drop=FALSE], target=d[, tn])
  else
    d[,vars,drop=FALSE]              
}