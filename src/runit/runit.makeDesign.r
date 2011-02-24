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
  
  ps4 = makeParameterSet(
    makeNumericVectorParameter("x", dim=2, lower=-2, upper=1), 
    makeIntegerVectorParameter("y", dim=3, lower=10L, upper=20L) 
  )
  des = makeDesign(13, ps4)
  checkEquals(nrow(des), 13)
  checkEquals(ncol(des), 5)
  checkEquals(colnames(des), c("x", "x", "y", "y", "y"))
  checkTrue(is.numeric(des[,1]))
  checkTrue(is.numeric(des[,2]))
  checkTrue(is.integer(des[,3]))
  checkTrue(is.integer(des[,4]))
  checkTrue(is.integer(des[,5]))
  checkTrue(des[,1] >= -2 && des[,1] <= 1)
  checkTrue(des[,2] >= -2 && des[,2] <= 1)
  checkTrue(des[,3] >= 10 && des[,3] <= 20)
  checkTrue(des[,4] >= 10 && des[,4] <= 20)
  checkTrue(des[,5] >= 10 && des[,5] <= 20)

  ps5 = makeParameterSet(
    makeNumericVectorParameter("x", dim=2, lower=-2, upper=1, trafo =  function(x) 2^x), 
    makeIntegerVectorParameter("y", dim=3, lower=10L, upper=20L, trafo=function(x) 3L*x) 
  )
  des = makeDesign(100, ps5)
  checkEquals(nrow(des), 100)
  checkEquals(ncol(des), 5)
  checkEquals(colnames(des), c("x", "x", "y", "y", "y"))
  checkTrue(is.numeric(des[,1]))
  checkTrue(is.numeric(des[,2]))
  checkTrue(is.integer(des[,3]))
  checkTrue(is.integer(des[,4]))
  checkTrue(is.integer(des[,5]))
  checkTrue(des[,1] >= 1/4 && des[,1] <= 2)
  checkTrue(des[,2] >= 1/4 && des[,2] <= 2)
  checkTrue(des[,3] >= 30 && des[,3] <= 60)
  checkTrue(des[,4] >= 30 && des[,4] <= 60)
  checkTrue(des[,5] >= 30 && des[,5] <= 60)
  
}