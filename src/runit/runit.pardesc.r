## Test typed parameters
test.numeric_parameters <- function() {
  np <- makeNumericParameter(id="x", lower=-1L, upper=1)
  checkEquals("numeric", np["type"])
  checkEquals(-1, lower(np))
  checkEquals(1, upper(np))
  checkTrue(!is.integer(lower(np))) ## Force type conversion!
  
  checkTrue(is.feasible(np, -1))
  checkTrue(is.feasible(np, 1))
  checkTrue(is.feasible(np, 0))

  checkTrue(!is.feasible(np, 2))
  checkTrue(!is.feasible(np, Inf))
  checkTrue(!is.feasible(np, -Inf))
  checkTrue(!is.feasible(np, NA))
  checkTrue(!is.feasible(np, "bam"))
  
  np <- makeNumericParameter(id="x", lower=0, upper=Inf)
  checkTrue(is.feasible(np, 2))
  checkTrue(is.feasible(np, Inf))
  checkTrue(!is.feasible(np, -2))
  checkTrue(!is.feasible(np, -Inf))
  checkTrue(!is.feasible(np, NULL))

  ## Error conditions:
  checkException(makeNumericParameter(id="x", lower="bam", upper=1))
  checkException(makeNumericParameter(id="x", lower=NA, upper=1))
  checkException(makeNumericParameter(id="x", lower=NULL, upper=1))
  checkException(makeNumericParameter(id="x", lower=0, upper="bam"))
  checkException(makeNumericParameter(id="x", lower=0, upper=NA))
  checkException(makeNumericParameter(id="x", lower=0, upper=NULL))
  checkException(makeNumericParameter(id="x", lower=1, upper=-1))
  checkException(makeNumericParameter(id="x", lower=c(-1, 1), upper=2))
  checkException(makeNumericParameter(id="x", lower=-1, upper=c(1, 2)))
}

test.numericvector_parameters <- function() {
  np = makeNumericVectorParameter(id="x", lower=-1L, upper=1, dim=2)
  checkEquals("numericvector", np["type"])
  checkEquals(c(-1,-1), lower(np))
  checkEquals(c(1,1), upper(np))
  checkTrue(!is.integer(lower(np))) ## Force type conversion!
  
  checkTrue(is.feasible(np, c(-1,-1)))
  checkTrue(is.feasible(np, c(1,1)))
  checkTrue(is.feasible(np, c(0,1)))
  
  checkTrue(!is.feasible(np, c(2,0)))
  checkTrue(!is.feasible(np, Inf))
  checkTrue(!is.feasible(np, -Inf))
  checkTrue(!is.feasible(np, NA))
  checkTrue(!is.feasible(np, "bam"))
  
  np <- makeNumericVectorParameter(id="x", lower=0, upper=Inf, dim=3)
  checkTrue(is.feasible(np, c(2,1,1)))
  checkTrue(is.feasible(np, c(Inf, Inf, Inf)))
  checkTrue(!is.feasible(np, c(-2,1,0)))
  checkTrue(!is.feasible(np, c(-Inf, 1)))
  checkTrue(!is.feasible(np, NULL))
  
  ## Error conditions:
  checkException(makeNumericVectorParameter(id="x", lower="bam", upper=1))
  checkException(makeNumericVectorParameter(id="x", dim=2, lower=NA, upper=1))
  checkException(makeNumericVectorParameter(id="x", dim=2, lower=NULL, upper=1))
  checkException(makeNumericVectorParameter(id="x", dim=2, lower=0, upper="bam"))
  checkException(makeNumericVectorParameter(id="x", dim=2, lower=0, upper=NA))
  checkException(makeNumericVectorParameter(id="x", dim=2, lower=0, upper=NULL))
  checkException(makeNumericVectorParameter(id="x", dim=2, lower=1, upper=-1))
  checkException(makeNumericVectorParameter(id="x", dim=2, lower=c(1, 1), upper=c(0,0)))
}


test.discrete_parameters <- function() {
  f <- function(x) 2 * x
  dp <- makeDiscreteParameter(id="x",
                           vals=list(a="char", b=2L, c=2.2, d=f, "e"))
  checkEquals("discrete", dp["type"])
  checkTrue(is.feasible(dp, "char"))
  checkTrue(is.feasible(dp, 2L))
  checkTrue(is.feasible(dp, 2.2))
  checkTrue(is.feasible(dp, f))
  ff <- function(x) 2 * x
  checkTrue(is.feasible(dp, ff))
  checkTrue(is.feasible(dp, function(x) 2 * x))
  checkTrue(is.feasible(dp, "e"))

  checkTrue(!is.feasible(dp, "f"))
  checkTrue(!is.feasible(dp, sum))
  checkTrue(!is.feasible(dp, NULL))


  ## Error conditions:
  checkException(makeDiscreteParameter(id="x", vals=list(a=1, "a")))
  checkException(makeDiscreteParameter(id="x", vals=list()))  
}

test.integer_parameters <- function() {
  ip <- makeIntegerParameter(id="x", lower=-1L, upper=1)
  checkEquals("integer", ip["type"])
  checkEquals(-1L, lower(ip))
  checkEquals(1L, upper(ip))
  checkTrue(is.integer(lower(ip))) 
  
  checkTrue(is.feasible(ip, -1))
  checkTrue(is.feasible(ip, -1L))
  checkTrue(is.feasible(ip, 1L))
  checkTrue(is.feasible(ip, 0L))

  checkTrue(!is.feasible(ip, 0.5))
  checkTrue(!is.feasible(ip, Inf))
  checkTrue(!is.feasible(ip, -Inf))
  checkTrue(!is.feasible(ip, NA))
  checkTrue(!is.feasible(ip, "bam"))
  
  ip <- makeIntegerParameter(id="x", lower=0)
  checkTrue(is.feasible(ip, 2L))
  checkTrue(!is.feasible(ip, Inf))
  checkTrue(!is.feasible(ip, -2L))
  checkTrue(!is.feasible(ip, -Inf))
  checkTrue(!is.feasible(ip, NULL))

  ## Error conditions:
  checkException(makeIntegerParameter(id="x", lower="bam", upper=1L))
  checkException(makeIntegerParameter(id="x", lower=NA, upper=1L))
  checkException(makeIntegerParameter(id="x", lower=NULL, upper=1L))
  checkException(makeIntegerParameter(id="x", lower=0L, upper="bam"))
  checkException(makeIntegerParameter(id="x", lower=0L, upper=NA))
  checkException(makeIntegerParameter(id="x", lower=0L, upper=NULL))
  checkException(makeIntegerParameter(id="x", lower=1L, upper=-1L))
  checkException(makeIntegerParameter(id="x", lower=c(-1L, 1L), upper=2L))
  checkException(makeIntegerParameter(id="x", lower=-1L, upper=c(1L, 2L)))
}

test.logical_parameters <- function() {
  bp <- makeLogicalParameter(id="x")
  checkEquals("logical", bp["type"])
  checkTrue(is.feasible(bp, TRUE))
  checkTrue(is.feasible(bp, FALSE))

  checkTrue(!is.feasible(bp, "bam"))
  checkTrue(!is.feasible(bp, 1L))
  checkTrue(!is.feasible(bp, 1))
  checkTrue(!is.feasible(bp, NULL))  
}

test.function_parameters <- function() {
  fp = makeFunctionParameter(id="x")
  checkEquals("function", fp["type"])
  checkTrue(is.feasible(fp, identity))
  
  checkTrue(!is.feasible(fp, "bam"))
  checkTrue(!is.feasible(fp, 1L))
  checkTrue(!is.feasible(fp, 1))
  checkTrue(!is.feasible(fp, NULL))  
}

