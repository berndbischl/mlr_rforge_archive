## Test typed parameters
test.numeric_parameters <- function() {
  np <- makeNumericParameter(id="x", lower=-1L, upper=1)
  checkEquals("numeric", np@type)
  checkEquals(-1, lower(np))
  checkEquals(1, upper(np))
  checkTrue(!is.integer(lower(np))) ## Force type conversion!
  
  checkTrue(isFeasible(np, -1))
  checkTrue(isFeasible(np, 1))
  checkTrue(isFeasible(np, 0))

  checkTrue(!isFeasible(np, 2))
  checkTrue(!isFeasible(np, Inf))
  checkTrue(!isFeasible(np, -Inf))
  checkTrue(!isFeasible(np, NA))
  checkTrue(!isFeasible(np, "bam"))
  
  np <- makeNumericParameter(id="x", lower=0, upper=Inf)
  checkTrue(isFeasible(np, 2))
  checkTrue(isFeasible(np, Inf))
  checkTrue(!isFeasible(np, -2))
  checkTrue(!isFeasible(np, -Inf))
  checkTrue(!isFeasible(np, NULL))

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
  checkEquals("numericvector", np@type)
  checkEquals(c(-1,-1), lower(np))
  checkEquals(c(1,1), upper(np))
  checkTrue(!is.integer(lower(np))) ## Force type conversion!
  
  checkTrue(isFeasible(np, c(-1,-1)))
  checkTrue(isFeasible(np, c(1,1)))
  checkTrue(isFeasible(np, c(0,1)))
  
  checkTrue(!isFeasible(np, c(2,0)))
  checkTrue(!isFeasible(np, Inf))
  checkTrue(!isFeasible(np, -Inf))
  checkTrue(!isFeasible(np, NA))
  checkTrue(!isFeasible(np, "bam"))
  
  np <- makeNumericVectorParameter(id="x", lower=0, upper=Inf, dim=3)
  checkTrue(isFeasible(np, c(2,1,1)))
  checkTrue(isFeasible(np, c(Inf, Inf, Inf)))
  checkTrue(!isFeasible(np, c(-2,1,0)))
  checkTrue(!isFeasible(np, c(-Inf, 1)))
  checkTrue(!isFeasible(np, NULL))
  
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
  checkEquals("discrete", dp@type)
  checkTrue(isFeasible(dp, "char"))
  checkTrue(isFeasible(dp, 2L))
  checkTrue(isFeasible(dp, 2.2))
  checkTrue(isFeasible(dp, f))
  ff <- function(x) 2 * x
  checkTrue(isFeasible(dp, ff))
  checkTrue(isFeasible(dp, function(x) 2 * x))
  checkTrue(isFeasible(dp, "e"))

  checkTrue(!isFeasible(dp, "f"))
  checkTrue(!isFeasible(dp, sum))
  checkTrue(!isFeasible(dp, NULL))


  ## Error conditions:
  checkException(makeDiscreteParameter(id="x", vals=list(a=1, "a")))
  checkException(makeDiscreteParameter(id="x", vals=list()))  
}

test.integer_parameters <- function() {
  ip <- makeIntegerParameter(id="x", lower=-1L, upper=1)
  checkEquals("integer", ip@type)
  checkEquals(-1L, lower(ip))
  checkEquals(1L, upper(ip))
  checkTrue(is.integer(lower(ip))) 
  
  checkTrue(isFeasible(ip, -1))
  checkTrue(isFeasible(ip, -1L))
  checkTrue(isFeasible(ip, 1L))
  checkTrue(isFeasible(ip, 0L))

  checkTrue(!isFeasible(ip, 0.5))
  checkTrue(!isFeasible(ip, Inf))
  checkTrue(!isFeasible(ip, -Inf))
  checkTrue(!isFeasible(ip, NA))
  checkTrue(!isFeasible(ip, "bam"))
  
  ip <- makeIntegerParameter(id="x", lower=0)
  checkTrue(isFeasible(ip, 2L))
  checkTrue(!isFeasible(ip, Inf))
  checkTrue(!isFeasible(ip, -2L))
  checkTrue(!isFeasible(ip, -Inf))
  checkTrue(!isFeasible(ip, NULL))

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
  checkEquals("logical", bp@type)
  checkTrue(isFeasible(bp, TRUE))
  checkTrue(isFeasible(bp, FALSE))

  checkTrue(!isFeasible(bp, "bam"))
  checkTrue(!isFeasible(bp, 1L))
  checkTrue(!isFeasible(bp, 1))
  checkTrue(!isFeasible(bp, NULL))  
}

test.function_parameters <- function() {
  fp = makeFunctionParameter(id="x")
  checkEquals("function", fp@type)
  checkTrue(isFeasible(fp, identity))
  
  checkTrue(!isFeasible(fp, "bam"))
  checkTrue(!isFeasible(fp, 1L))
  checkTrue(!isFeasible(fp, 1))
  checkTrue(!isFeasible(fp, NULL))  
}

