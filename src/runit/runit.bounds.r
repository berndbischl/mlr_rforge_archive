test.par.set <- function() {
  np1 = numeric.parameter(id="x1", lower=-1L, upper=1)
  np2 = numeric.parameter(id="x2", lower=0, upper=Inf)
  dp1 = discrete.parameter(id="x3", vals=list(a="char", b=2L, c=2.2, "e"))
  
  b1 = makeParameterSet(np1, np2, dp1)
  
  checkTrue(is.feasible(list(0,0,"a"), b1))
  checkTrue(!is.feasible(list(2,0,"a"), b1))

  checkEquals(lower(b1), c(x1=-1, x2=0))
  checkEquals(upper(b1), c(x1= 1, x2=Inf))
    
  checkException(makeParameterSet())
  checkException(makeParameterSet(1))
  checkException(makeParameterSet(numeric.parameter(name="x"), numeric.parameter(name="x")))
  
}

