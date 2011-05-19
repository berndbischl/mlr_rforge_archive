test.opt.path <- function() {
  op = new("OptPathDF", x.names=c("x1", "x2"), y.names=c("y1", "y2"), minimize=c(TRUE, FALSE))
  addPathElement(op, list(x1=1, x2="a"), c(y1=1, y2=4))
  addPathElement(op, list(x1=2, x2="a"), c(y1=3, y2=2))
  checkEquals(op@env$dob, 1:2)
  if (!use.package) {
    setEoL(op, list(x1=2, x2="a"), 8)
    checkEquals(op@env$eol[2], 8)
  }

  x = as.data.frame(op)
  checkTrue(is.data.frame(x))
  checkEquals(nrow(x), 2)
  checkEquals(ncol(x), 6)
  
  print(op)
  
#  op2 = subset(op, dob=1)
#  checkEquals(length(op@env$path), 2)
#  checkEquals(length(op2@env$path), 1)
#  checkEquals(op2@env$path[[1]], op@env$path[[1]])

#  op2 = subset(op, dob=2)
#  checkEquals(length(op@env$path), 2)
#  checkEquals(length(op2@env$path), 1)
#  checkEquals(op2@env$path[[1]], op@env$path[[2]])
  
  gbe = function(op, y.name, dob) {
    i = getBestIndex(op, y.name, dob)
    getPathElement(op, i)
  }
  
  checkEquals(gbe(op, y.name="y1", dob=1:2), getPathElement(op, 1))
  checkEquals(gbe(op, y.name="y2", dob=1:2), getPathElement(op, 1))
  checkEquals(gbe(op, y.name="y1", dob=1), getPathElement(op, 1))
  checkEquals(gbe(op, y.name="y2", dob=1), getPathElement(op, 1))
  checkEquals(gbe(op, y.name="y1", dob=2), getPathElement(op, 2))
  checkEquals(gbe(op, y.name="y2", dob=2), getPathElement(op, 2))
  
  op = new("OptPathDF", x.names=c("x", "y"), y.names="z", minimize=TRUE)
  addPathElement(op, list(c(1,1), "a"), 1)
  addPathElement(op, list(c(2,2), "a"), 3)
  df = as.data.frame(op)
  checkEquals(dim(df), c(2,3+1+2))
  checkEquals(colnames(df), c("x1", "x2", "y", "z", "dob", "eol"))
}  