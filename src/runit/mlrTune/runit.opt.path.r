test.opt.path <- function() {
  ps1 = makeParamSet(
    makeNumericParam("x"),
    makeDiscreteParam("y", vals=c("a", "b"))
  )
  op = new("OptPathDF", par.set=ps1, y.names=c("z1", "z2"), minimize=c(TRUE, FALSE))
  addOptPathEl(op, x=list(x=1, y="a"), y=c(z1=1, z2=4))
  addOptPathEl(op, x=list(x=2, y="a"), y=c(z1=3, z2=2))
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
  
  checkEquals(getOptPathEl(op, 1)$x, list(x=1, y="a"))
  
  gbe = function(op, y.name, dob) {
    i = getOptPathBestIndex(op, y.name, dob)
    getOptPathEl(op, i)
  }
  
  checkEquals(gbe(op, y.name="z1", dob=1:2), getOptPathEl(op, 1))
  checkEquals(gbe(op, y.name="z2", dob=1:2), getOptPathEl(op, 1))
  checkEquals(gbe(op, y.name="z1", dob=1), getOptPathEl(op, 1))
  checkEquals(gbe(op, y.name="z2", dob=1), getOptPathEl(op, 1))
  checkEquals(gbe(op, y.name="z1", dob=2), getOptPathEl(op, 2))
  checkEquals(gbe(op, y.name="z2", dob=2), getOptPathEl(op, 2))
    
  ps2 = makeParamSet(
    makeNumericVectorParam("x", dim=2),
    makeIntegerParam("y")
  )
  op = new("OptPathDF", par.set=ps2, y.names="z", minimize=TRUE)
  addOptPathEl(op, x=list(c(1,1), 7L), y=1)
  addOptPathEl(op, x=list(c(2,2), 8L), y=3)
  df = as.data.frame(op)
  checkEquals(dim(df), c(2,3+1+2))
  checkEquals(colnames(df), c("x1", "x2", "y", "z", "dob", "eol"))
  e = getOptPathEl(op, 1)
  checkEquals(e$x, list(x=c(1,1), y=7L))
}

testOptPathDiscretePars = function() {
  ps1 = makeParamSet(
    makeDiscreteParam("x1", vals=c("a", "b")),
    makeDiscreteParam("x2", vals=1:2),
    makeDiscreteParam("x3", vals=c(1.2, 5)),
    makeDiscreteParam("x4", vals=list(foo=identity, bar=list()))
  )
  op = new("OptPathDF", par.set=ps1, y.names="y", minimize=TRUE)
  addOptPathEl(op, x=list(x1="a", x2=2L, x3=5, x4="foo"), y=0)
  d = as.data.frame(op)
  checkTrue(nrow(d) == 1 && ncol(d) == 4+1+2)
  checkTrue(is.character(d$x1))
  checkTrue(is.integer(d$x2))
  checkTrue(is.numeric(d$x3))
  checkTrue(is.character(d$x4))
  checkTrue(d[1,1] == "a" && d[1,2] == 2L && d[1,3] == 5 && d[1,4] == "foo")
}
  

