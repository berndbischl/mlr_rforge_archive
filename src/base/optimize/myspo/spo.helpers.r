#todo probably put in mlr something like Parameter.list as a type, of par.bag.
# maybe its enough to have fnctions operation on R lists of such.....

setGeneric(
  name = "sample.pardesc",
  def = function(n, pd) {
    standardGeneric("sample.pardesc")      
  }
)

setMethod(
  f = "sample.pardesc",
  signature = signature(n="integer", pd="Parameter.double"),
  def = function(n, pd) {
    x = runif(n, pd["lower"], pd["upper"])
    if (pd["data.type"] == "integer")
      x = round(x)
    return(x)
  }
)

setMethod(
  f = "sample.pardesc",
  signature = signature(n="integer", pd="Parameter.disc"),
  def = function(n, pd) 
    as.factor(sample(names(pd["vals"]), n, replace=TRUE))
)

setMethod(
  f = "sample.pardesc",
  signature = signature(n="integer", pd="Parameter.log"),
  def = function(n, pd) 
    sample(c(TRUE, FALSE), n, replace=TRUE)
)

