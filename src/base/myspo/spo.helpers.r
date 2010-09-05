
setGeneric(
  name = "sample.pardesc",
  def = function(n, pd) {
    standardGeneric("sample.pardesc")      
  }
)

setMethod(
  f = "sample.pardesc",
  signature = signature(n="integer", pd="par.desc.num"),
  def = function(n, pd) {
    x = runif(n, pd["lower"], pd["upper"])
    if (pd["data.type"] == "integer")
      x = round(x) 
  }
)

setMethod(
  f = "sample.pardesc",
  signature = signature(n="integer", pd="par.desc.disc"),
  def = function(n, pd) 
    as.factor(sample(names(pd["vals"]), n, replace=TRUE))
)

setMethod(
  f = "sample.pardesc",
  signature = signature(n="integer", pd="par.desc.log"),
  def = function(n, pd) 
    sample(c(TRUE, FALSE), n, replace=TRUE)
)

