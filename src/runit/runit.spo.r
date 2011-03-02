test.spo.rf <- function() {
  f = makeSPOFunction(function(x) sum(x^2))
  
  ps = makeParameterSet(
    makeNumericParameter("x1", lower=-2, upper=1), 
    makeNumericParameter("x2", lower=-1, upper=2) 
  )
  des = makeDesign(10, par.set=ps)
  y  = sapply(1:nrow(des), function(i) f(as.list(des[i,])))
  des$y = y
  learner = makeLearner("regr.randomForest")
  ctrl = makeSPOControl(seq.loops=5, seq.design.points=100, save.model.at=c(0,5))
  or = spo(f, ps, des, learner, ctrl)
  checkEquals(length(as.list(or$path)), 15)
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
  ps = makeParameterSet(
    makeNumericParameter("x1", lower=-10, upper=10, trafo=function(x) abs(x)) 
  )
  des = makeDesign(10, par.set=ps)
  checkTrue(all(des$x1 >= 0))
  des$y  = sapply(1:nrow(des), function(i) f(as.list(des[i,])))
  or = spo(f, ps, des, learner, ctrl)
  checkEquals(length(as.list(or$path)), 15)
  df = as.data.frame(or$path)
  checkTrue(is.numeric(df$x1))
  
  # discrete par
  f = function(x) if(x[[3]]=="a") x[[1]]^2+x[[2]]^2 else x[[1]]^2+x[[2]]^2 + 20 
  
  ps = makeParameterSet(
    makeNumericParameter("x1", lower=-2, upper=1), 
    makeIntegerParameter("x2", lower=-1, upper=2), 
    makeDiscreteParameter("x3", vals=c("a", "b")) 
  )
  des = makeDesign(10, par.set=ps)
  y  = sapply(1:nrow(des), function(i) f(as.list(des[i,])))
  des$y = y
  learner = makeLearner("regr.randomForest")
  ctrl = makeSPOControl(seq.loops=5, seq.design.points=100)
  or = spo(f, ps, des, learner, ctrl)
  checkEquals(length(as.list(or$path)), 15)
  df = as.data.frame(or$path)
  checkTrue(is.numeric(df$x1))
  checkTrue(is.integer(df$x2))
  checkTrue(is.factor(df$x3))
  checkTrue(is.numeric(df$y))
  checkTrue(is.list(or$x))
  checkEquals(names(or$x), names(ps@pars))

  ctrl = makeSPOControl(init.design.points=3, seq.loops=5, seq.design.points=100)
  or = spo(f, ps, des=NULL, learner, ctrl)
  checkEquals(length(as.list(or$path)), 8)
  checkEquals(names(or$x), names(ps@pars))
} 

test.spo.km <- function() {
  f = makeSPOFunction(function(x) sum(x^2))
  
  ps = makeParameterSet(
    makeNumericParameter("x1", lower=-2, upper=1), 
    makeNumericParameter("x2", lower=-1, upper=2) 
  )
  des = makeDesign(10, par.set=ps)
  y  = sapply(1:nrow(des), function(i) f(as.list(des[i,])))
  des$y = y
  learner = makeLearner("regr.km")
  ctrl = makeSPOControl(seq.loops=5, seq.design.points=100)
  or = spo(f, ps, des, learner, ctrl)
  checkEquals(length(as.list(or$path)), 15)
  df = as.data.frame(or$path)
  checkTrue(is.numeric(df$x1))
  checkTrue(is.numeric(df$x2))
  checkTrue(is.list(or$x))
  checkEquals(names(or$x), names(ps@pars))
  checkTrue(is(or$models[[1]]@learner, "regr.km"))
  checkEquals(length(or$models[[1]]@subset), 15)
  
  ps = makeParameterSet(
    makeNumericParameter("x1", lower=-2, upper=1), 
    makeIntegerParameter("x2", lower=-1, upper=2) 
  )
  des = makeDesign(10, par.set=ps)
  des$y  = sapply(1:nrow(des), function(i) f(as.list(des[i,])))
  or = spo(f, ps, des, learner, ctrl)
  checkEquals(length(as.list(or$path)), 15)
  df = as.data.frame(or$path)
  checkTrue(is.numeric(df$x1))
  checkTrue(is.integer(df$x2))
  checkTrue(is.list(or$x))
  checkEquals(names(or$x), names(ps@pars))

  
  ctrl = makeSPOControl(seq.loops=5, seq.design.points=100, propose.points.method="EI")
  or = spo(f, ps, des, learner, ctrl)
  checkEquals(length(as.list(or$path)), 15)
  df = as.data.frame(or$path)
  checkTrue(is.numeric(df$x1))
  checkTrue(is.integer(df$x2))
  checkTrue(is.list(or$x))
  checkEquals(names(or$x), names(ps@pars))
} 


