## test.pardesc <- function() {
##   pd1 = new("par.desc.double", par.id="x", default=0.001, lower=0)
##   checkEquals(pd1["data.type"], "numeric")
  
##   v = make.learner("classif.mda")
##   checkTrue(all.els.named(v["par.descs"]))
##   v = make.learner("classif.rpart", predict.type="prob")
##   checkTrue(all.els.named(v["par.descs"]))
  
##   w = new("base.wrapper", learner=v, par.descs=list(pd1))
##   checkTrue(all.els.named(w["par.descs"]))
##   w = make.probth.wrapper(v, classes=multiclass.task["class.levels"])
##   checkTrue(all.els.named(w["par.descs"]))
##   w = make.multiclass.wrapper(v)
##   checkTrue(all.els.named(w["par.descs"]))
##   w = make.filter.wrapper(v, fw.threshold=0.5, fw.method="chi.squared")
##   checkTrue(all.els.named(w["par.descs"]))
##   w = make.preproc.wrapper(v, 
##     train=function(data, targetvar, args) data, 
##     predict=function(data, targetvar, args, control) data,
##     args = list(x=1, y=2)
##   )
##   checkException(
##     make.preproc.wrapper(v, 
##       train=function(data, targetvar, args) data, 
##       predict=function(data, targetvar, args, control) data,
##       args = list(minsplit=1)
##     ), silent=TRUE)
##   s = geterrmessage()
##   checkTrue(length(grep("yperparameter names in wrapper clash with base learner names: minsplit", s)) > 0 )
## }

## Test typed parameters
test.numeric_parameters <- function() {
  np <- makeNumericParameter(id="x", lower=-1L, upper=1)
  checkEquals("numeric", np["type"])
  checkEquals(-1, lower(np))
  checkEquals(1, upper(np))
  checkTrue(!is.integer(lower(np))) ## Force type conversion!
  
  checkTrue(is.feasible(-1, np))
  checkTrue(is.feasible(1, np))
  checkTrue(is.feasible(0, np))

  ## Vectorized:
  checkEquals(c(FALSE, TRUE, TRUE, TRUE, FALSE),
              is.feasible(c(-2, -1, 0, 1, 2), np))

  checkTrue(!is.feasible(2, np))
  checkTrue(!is.feasible(Inf, np))
  checkTrue(!is.feasible(-Inf, np))
  checkTrue(!is.feasible(NA, np))
  checkTrue(!is.feasible("bam", np))
  
  np <- makeNumericParameter(id="x", lower=0, upper=Inf)
  checkTrue(is.feasible(2, np))
  checkTrue(is.feasible(Inf, np))
  checkTrue(!is.feasible(-2, np))
  checkTrue(!is.feasible(-Inf, np))
  checkTrue(!is.feasible(NULL, np))

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

test.discrete_parameters <- function() {
  f <- function(x) 2 * x
  dp <- makeDiscreteParameter(id="x",
                           vals=list(a="char", b=2L, c=2.2, d=f, "e"))
  checkEquals("discrete", dp["type"])
  checkTrue(is.feasible("char", dp))
  checkTrue(is.feasible(2L, dp))
  checkTrue(is.feasible(2.2, dp))
  checkTrue(is.feasible(f, dp))
  ff <- function(x) 2 * x
  checkTrue(is.feasible(ff, dp))
  checkTrue(is.feasible(function(x) 2 * x, dp))
  checkTrue(is.feasible("e", dp))

  ## Vectorized:
  checkEquals(c(TRUE, TRUE, TRUE),
            is.feasible(list(2.2, 2L, f), dp))
  checkEquals(c(TRUE, FALSE),
              is.feasible(list(2L, NULL), dp))
  
  checkTrue(!is.feasible("f", dp))
  checkTrue(!is.feasible(sum, dp))
  checkTrue(!is.feasible(NULL, dp))


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
  
  checkTrue(is.feasible(-1, ip))
  checkTrue(is.feasible(-1L, ip))
  checkTrue(is.feasible(1L, ip))
  checkTrue(is.feasible(0L, ip))

  ## Vectorized:
  checkEquals(c(FALSE, TRUE, FALSE, TRUE, FALSE),
              is.feasible(c(-2L, -1, 0.5, 1L, 2), ip))
  
  checkTrue(!is.feasible(0.5, ip))
  checkTrue(!is.feasible(Inf, ip))
  checkTrue(!is.feasible(-Inf, ip))
  checkTrue(!is.feasible(NA, ip))
  checkTrue(!is.feasible("bam", ip))
  
  ip <- makeIntegerParameter(id="x", lower=0)
  checkTrue(is.feasible(2L, ip))
  checkTrue(!is.feasible(Inf, ip))
  checkTrue(!is.feasible(-2L, ip))
  checkTrue(!is.feasible(-Inf, ip))
  checkTrue(!is.feasible(NULL, ip))

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
  checkTrue(is.feasible(TRUE, bp))
  checkTrue(is.feasible(FALSE, bp))

  checkEquals(c(TRUE, TRUE, FALSE),
              is.feasible(c(TRUE, FALSE, NA), bp))

  checkTrue(!is.feasible("bam", bp))
  checkTrue(!is.feasible(1L, bp))
  checkTrue(!is.feasible(1, bp))
  checkTrue(!is.feasible(NULL, bp))  
}

test.function_parameters <- function() {
  fp = makeFunctionParameter(id="x")
  checkEquals("function", fp["type"])
  checkTrue(is.feasible(identity, fp))
  
  checkEquals(c(TRUE, TRUE, FALSE),
    is.feasible(list(identity, identity, TRUE), fp))
  
  checkTrue(!is.feasible("bam", fp))
  checkTrue(!is.feasible(1L, fp))
  checkTrue(!is.feasible(1, fp))
  checkTrue(!is.feasible(NULL, fp))  
}

