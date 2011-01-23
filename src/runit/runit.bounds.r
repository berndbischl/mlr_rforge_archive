test.bounds <- function() {
  np1 = numeric.parameter(name="x1", lower=-1L, upper=1)
  np2 = numeric.parameter(name="x2", lower=0, upper=Inf)
  dp1 = discrete.parameter(name="x3", vals=list(a="char", b=2L, c=2.2, "e"))
  
  b1 = make.bounds(np1, np2, dp1)
  
  checkTrue(is.feasible(list(0,0,"a"), b1))
  checkTrue(!is.feasible(list(2,0,"a"), b1))

  checkEquals(lower(b1), c(x1=-1, x2=0))
  checkEquals(upper(b1), c(x1= 1, x2=Inf))
    
  checkException(make.bounds())
  checkException(make.bounds(1))
  checkException(make.bounds(numeric.parameter(name="x"), numeric.parameter(name="x")))
  
}

