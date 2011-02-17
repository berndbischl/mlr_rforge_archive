test.makeDesign = function() {
  ps1 = makeParameterSet(
    makeNumericParameter("x1", lower=-2, upper=1) 
  )
  des = makeDesign(13, ps1, randomLHS)
  checkEquals(nrow(des), 13)
  checkEquals(ncol(des), 1)
  checkTrue(is.numeric(des[,1]))
  checkTrue(des[,1] >= -2 && des[,1] <= 1)
  des = makeDesign(13, ps1, maximinLHS)
  checkEquals(nrow(des), 13)
  checkEquals(ncol(des), 1)
  checkTrue(des[,1] >= -2 && des[,1] <= 1)
  
  ps2 = makeParameterSet(
    makeNumericParameter("x1", lower=-2, upper=1), 
    makeIntegerParameter("x2", lower=10, upper=20) 
  )
  des = makeDesign(13, ps2, randomLHS)
  checkEquals(nrow(des), 13)
  checkEquals(ncol(des), 2)
  checkTrue(is.numeric(des[,1]))
  checkTrue(des[,1] >= -2 && des[,1] <= 1)
  checkTrue(is.integer(des[,2]))
  checkTrue(des[,2] >= 10 && des[,2] <= 20)
  des = makeDesign(13, ps2, maximinLHS)
  checkEquals(nrow(des), 13)
  checkEquals(ncol(des), 2)
  checkTrue(is.numeric(des[,1]))
  checkTrue(des[,1] >= -2 && des[,1] <= 1)
  checkTrue(is.integer(des[,2]))
  checkTrue(des[,2] >= 10 && des[,2] <= 20)

  ps3 = makeParameterSet(
    makeNumericParameter("x1", lower=-2, upper=1), 
    makeIntegerParameter("x2", lower=10, upper=20), 
    makeDiscreteParameter("x3", vals=c("a", "b", "c")) 
  )
  des = makeDesign(13, ps3)
  checkEquals(nrow(des), 13)
  checkEquals(ncol(des), 3)
  checkTrue(is.numeric(des[,1]))
  checkTrue(des[,1] >= -2 && des[,1] <= 1)
  checkTrue(is.integer(des[,2]))
  checkTrue(des[,2] >= 10 && des[,2] <= 20)
  checkTrue(is.factor(des[,3]))
  checkEquals(levels(des[,3]), names(ps3@pars[[3]]@constraints$vals))
}