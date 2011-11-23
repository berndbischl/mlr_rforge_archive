test.spo.rf <- function() {
  f = makeSPOFunction(function(x) sum(x^2))
  
  ps = makeParamSet(
    makeNumericParam("x1", lower=-2, upper=1), 
    makeNumericParam("x2", lower=-1, upper=2) 
  )
  des = makeDesign(10, par.set=ps)
  y  = sapply(1:nrow(des), function(i) f(as.list(des[i,])))
  des$y = y
  learner = makeLearner("regr.randomForest")
  ctrl = makeSPOControl(seq.loops=5, seq.design.points=100, save.model.at=c(0,5))
  or = spo(f, ps, des, learner, ctrl)
  checkTrue(!is.na(or$y))
  checkEquals(or$y, f(or$x))
  checkEquals(length(or$path), 15)
  checkTrue(is.list(or$x))
  checkEquals(names(or$x), names(ps@pars))
  checkTrue(is(or$models[[1]]@learner, "regr.randomForest"))
  checkEquals(length(or$models[[1]]@subset), 10)
  checkTrue(is(or$models[[2]]@learner, "regr.randomForest"))
  checkEquals(length(or$models[[2]]@subset), 15)
  
  # check errors
  des$y = NULL
  checkException(spo(f, ps, des, learner, ctrl), silent=TRUE)
  s = geterrmessage()
  checkTrue(length(grep("must contain y column", s)) >0 )
  des$y = y
  
  ctrl = makeSPOControl(seq.loops=5, seq.design.points=100, propose.points.method="EI")
  checkException(spo(f, ps, des, learner, ctrl), silent=TRUE)
  s = geterrmessage()
  checkTrue(length(grep("Expected improvement can currently", s)) >0 )
  ctrl = makeSPOControl(seq.loops=5, seq.design.points=100)
  
  # check trafo
  ps = makeParamSet(
    makeNumericParam("x1", lower=-10, upper=10, trafo=function(x) abs(x)) 
  )
  des = makeDesign(10, par.set=ps)
  des$y  = sapply(1:nrow(des), function(i) f(as.list(des[i,])))
  or = spo(f, ps, des, learner, ctrl)
  checkTrue(!is.na(or$y))
  checkEquals(length(or$path), 15)
  df = as.data.frame(or$path)
  checkTrue(is.numeric(df$x1))
  
  # discrete par
  f = function(x) if(x[[3]]=="a") x[[1]]^2+x[[2]]^2 else x[[1]]^2+x[[2]]^2 + 20 
  
  ps = makeParamSet(
    makeNumericParam("x1", lower=-2, upper=1), 
    makeIntegerParam("x2", lower=-1, upper=2), 
    makeDiscreteParam("x3", vals=c("a", "b")) 
  )
  des = makeDesign(10, par.set=ps)
  y  = sapply(1:nrow(des), function(i) f(as.list(des[i,])))
  des$y = y
  learner = makeLearner("regr.randomForest")
  ctrl = makeSPOControl(seq.loops=5, seq.design.points=100)
  or = spo(f, ps, des, learner, ctrl)
  checkTrue(!is.na(or$y))
  checkEquals(length(or$path), 15)
  df = as.data.frame(or$path)
  checkTrue(is.numeric(df$x1))
  checkTrue(is.integer(df$x2))
  checkTrue(is.character(df$x3))
  checkTrue(is.numeric(df$y))
  checkTrue(is.list(or$x))
  checkEquals(names(or$x), names(ps@pars))
  
  ctrl = makeSPOControl(init.design.points=3, seq.loops=5, seq.design.points=100)
  or = spo(f, ps, des=NULL, learner, ctrl)
  checkTrue(!is.na(or$y))
  checkEquals(length(or$path), 8)
  checkEquals(names(or$x), names(ps@pars))
  
  f = function(x) sum(x[[1]]^2) + (2 - x[[2]])^2
  
  ps = makeParamSet(
    makeNumericVectorParam("v", lower=-5, upper=5, dim=2), 
    makeNumericParam("w", lower=-5, upper=5) 
  )
  learner = makeLearner("regr.randomForest")
  ctrl = makeSPOControl(init.design.points=5, seq.loops=10, propose.points.method="CMAES")
  or = spo(f, ps, des=NULL, learner, ctrl)
  checkTrue(!is.na(or$y))
  checkEquals(length(or$path), 15)
  ctrl = makeSPOControl(init.design.points=5, seq.loops=10, final.point="best.predicted")
  or = spo(f, ps, des=NULL, learner, ctrl)
  checkEquals(length(or$path), 15)
} 

test.spo.km <- function() {
  f = makeSPOFunction(function(x) sum(x^2))
  
  ps = makeParamSet(
    makeNumericParam("x1", lower=-2, upper=1), 
    makeNumericParam("x2", lower=-1, upper=2) 
  )
  des = makeDesign(10, par.set=ps)
  y  = sapply(1:nrow(des), function(i) f(as.list(des[i,])))
  des$y = y
  learner = makeLearner("regr.km", nugget.estim=TRUE)
  ctrl = makeSPOControl(seq.loops=5, seq.design.points=100)
  or = spo(f, ps, des, learner, ctrl)
  checkTrue(!is.na(or$y))
  checkEquals(length(or$path), 15)
  df = as.data.frame(or$path)
  checkTrue(is.numeric(df$x1))
  checkTrue(is.numeric(df$x2))
  checkTrue(is.list(or$x))
  checkEquals(names(or$x), names(ps@pars))
  checkTrue(is(or$models[[1]]@learner, "regr.km"))
  checkEquals(length(or$models[[1]]@subset), 15)
  
  ps = makeParamSet(
    makeNumericParam("x1", lower=-2, upper=1), 
    makeIntegerParam("x2", lower=-1, upper=2) 
  )
  des = makeDesign(10, par.set=ps)
  des$y  = sapply(1:nrow(des), function(i) f(as.list(des[i,])))
  or = spo(f, ps, des, learner, ctrl)
  checkTrue(!is.na(or$y))
  checkEquals(length(or$path), 15)
  df = as.data.frame(or$path)
  checkTrue(is.numeric(df$x1))
  checkTrue(is.integer(df$x2))
  checkTrue(is.list(or$x))
  checkEquals(names(or$x), names(ps@pars))

  
  ctrl = makeSPOControl(seq.loops=5, seq.design.points=100, propose.points.method="EI")
  or = spo(f, ps, des, learner, ctrl)
  checkTrue(!is.na(or$y))
  checkEquals(length(or$path), 15)
  df = as.data.frame(or$path)
  checkTrue(is.numeric(df$x1))
  checkTrue(is.integer(df$x2))
  checkTrue(is.list(or$x))
  checkEquals(names(or$x), names(ps@pars))
} 

testResample = function() {
  f = makeSPOFunction(function(x) sum(x^2))
  ps = makeSPOParamSet("x", lower=c(0,0), upper=c(1,1)) 
  learner = makeLearner("regr.randomForest")
  ctrl = makeSPOControl(seq.loops=5, seq.design.points=10, resample.at=c(1,3))
  or = spo(f, ps, des=NULL, learner, ctrl)
  x = or$resample
  checkTrue(is.list(x) && length(x) == 2)
  checkTrue(is.numeric(x[[1]]) && is.numeric(x[[2]]))
  checkEquals(names(x), c("1", "3"))
}


