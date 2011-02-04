test.par.set <- function() {
  np1 = makeNumericParameter(id="x1", lower=-1L, upper=1)
  np2 = makeNumericParameter(id="x2", lower=0, upper=Inf)
  dp1 = makeDiscreteParameter(id="x3", vals=list(a="char", b=2L, c=2.2, "e"))
  
  b1 = makeParameterSet(np1, np2, dp1)
  
  checkTrue(is.feasible(b1, list(0,0,"a")))
  checkTrue(!is.feasible(b1, list(2,0,"a")))

  checkEquals(lower(b1), c(x1=-1, x2=0))
  checkEquals(upper(b1), c(x1= 1, x2=Inf))
    
  checkException(makeParameterSet(1))
  checkException(makeParameterSet(makeNumericParameter(name="x"), makeNumericParameter(name="x")))
  
}

