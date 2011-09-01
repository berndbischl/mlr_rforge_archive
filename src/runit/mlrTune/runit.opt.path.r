test.opt.path <- function() {
  ps1 = makeParameterSet(
    makeNumericParameter("x"),
    makeDiscreteParameter("y", vals=c("a", "b"))
  )
  op = new("OptPathDF", par.set=ps1, y.names=c("z1", "z2"), minimize=c(TRUE, FALSE))
  addPathElement(op, x=list(x=1, y="a"), y=c(z1=1, z2=4))
  addPathElement(op, x=list(x=2, y="a"), y=c(z1=3, z2=2))
  checkEquals(op@env$dob, 1:2)
  if (!use.package) {
    setEoL(op, 2, 8)
    checkEquals(op@env$eol[2], 8)
  }

  x = as.data.frame(op)
  checkTrue(is.data.frame(x))
  checkEquals(nrow(x), 2)
  checkEquals(ncol(x), 6)
  
  print(op)
  
  checkEquals(getPathElement(op, 1)$x, list(x=1, y="a"))
  
  gbe = function(op, y.name, dob) {
    i = getBestIndex(op, y.name, dob)
    getPathElement(op, i)
  }
  
  checkEquals(gbe(op, y.name="z1", dob=1:2), getPathElement(op, 1))
  checkEquals(gbe(op, y.name="z2", dob=1:2), getPathElement(op, 1))
  checkEquals(gbe(op, y.name="z1", dob=1), getPathElement(op, 1))
  checkEquals(gbe(op, y.name="z2", dob=1), getPathElement(op, 1))
  checkEquals(gbe(op, y.name="z1", dob=2), getPathElement(op, 2))
  checkEquals(gbe(op, y.name="z2", dob=2), getPathElement(op, 2))
    
  ps2 = makeParameterSet(
    makeNumericVectorParameter("x", dim=2),
    makeIntegerParameter("y")
  )
  op = new("OptPathDF", par.set=ps2, y.names="z", minimize=TRUE)
  addPathElement(op, x=list(c(1,1), 7L), y=1)
  addPathElement(op, x=list(c(2,2), 8L), y=3)
  df = as.data.frame(op)
  checkEquals(dim(df), c(2,3+1+2))
  checkEquals(colnames(df), c("x1", "x2", "y", "z", "dob", "eol"))
  e = getPathElement(op, 1)
  checkEquals(e$x, list(x=c(1,1), y=7L))
}  