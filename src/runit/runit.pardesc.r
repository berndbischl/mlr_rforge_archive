## test.pardesc <- function() {
##   pd1 = new("par.desc.double", par.name="x", default=0.001, lower=0)
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
  np <- numeric.parameter(name="x", lower=-1L, upper=1)
  checkEquals("numeric", np["type"])
  checkEquals(-1, np["lower"])
  checkEquals(1, np["upper"])
  checkTrue(!is.integer(np["lower"])) ## Force type conversion!
  
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
  
  np <- numeric.parameter(name="x", lower=0, upper=Inf)
  checkTrue(is.feasible(2, np))
  checkTrue(is.feasible(Inf, np))
  checkTrue(!is.feasible(-2, np))
  checkTrue(!is.feasible(-Inf, np))
  checkTrue(!is.feasible(NULL, np))

  ## Error conditions:
  checkException(numeric.parameter(name="x", lower="bam", upper=1))
  checkException(numeric.parameter(name="x", lower=NA, upper=1))
  checkException(numeric.parameter(name="x", lower=NULL, upper=1))
  checkException(numeric.parameter(name="x", lower=0, upper="bam"))
  checkException(numeric.parameter(name="x", lower=0, upper=NA))
  checkException(numeric.parameter(name="x", lower=0, upper=NULL))
  checkException(numeric.parameter(name="x", lower=1, upper=-1))
  checkException(numeric.parameter(name="x", lower=c(-1, 1), upper=2))
  checkException(numeric.parameter(name="x", lower=-1, upper=c(1, 2)))
}

test.discrete_parameters <- function() {
  f <- function(x) 2 * x
  dp <- discrete.parameter(name="x",
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
  
  checkTrue(!is.feasible(2, dp))
  checkTrue(!is.feasible("f", dp))
  checkTrue(!is.feasible(sum, dp))
  checkTrue(!is.feasible(NULL, dp))


  ## Error conditions:
  checkException(discrete.parameter(name="x", vals=list(a=1, "a")))
  checkException(discrete.parameter(name="x", vals=list()))  
}

test.integer_parameters <- function() {
  ip <- integer.parameter(name="x", lower=-1L, upper=1)
  checkEquals("integer", ip["type"])
  checkEquals(-1L, ip["lower"])
  checkEquals(1L, ip["upper"])
  checkTrue(is.integer(ip["lower"])) 
  
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
  
  ip <- integer.parameter(name="x", lower=0)
  checkTrue(is.feasible(2L, ip))
  checkTrue(!is.feasible(Inf, ip))
  checkTrue(!is.feasible(-2L, ip))
  checkTrue(!is.feasible(-Inf, ip))
  checkTrue(!is.feasible(NULL, ip))

  ## Error conditions:
  checkException(integer.parameter(name="x", lower="bam", upper=1L))
  checkException(integer.parameter(name="x", lower=NA, upper=1L))
  checkException(integer.parameter(name="x", lower=NULL, upper=1L))
  checkException(integer.parameter(name="x", lower=0L, upper="bam"))
  checkException(integer.parameter(name="x", lower=0L, upper=NA))
  checkException(integer.parameter(name="x", lower=0L, upper=NULL))
  checkException(integer.parameter(name="x", lower=1L, upper=-1L))
  checkException(integer.parameter(name="x", lower=c(-1L, 1L), upper=2L))
  checkException(integer.parameter(name="x", lower=-1L, upper=c(1L, 2L)))
}

test.logical_parameters <- function() {
  bp <- logical.parameter(name="x")
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
