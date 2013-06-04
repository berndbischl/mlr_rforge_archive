context("infill crits")

test_that("infill crits", {
  f = makeMBOFunction(function(x) sum(x^2))
  ps = makeParamSet(
    makeNumericVectorParam("x", len=2, lower=-10, upper=10) 
  )
  learner = makeLearner("regr.km", predict.type="se")
  mycontrol = function(minimize, crit) {
    makeMBOControl(minimize=minimize, init.design.points=20, seq.loops=10, seq.design.points=500, 
      infill.crit=crit, infill.opt="design")
  }
  mycheck = function(or, minimize) {
    expect_equal(getOptPathLength(or$opt.path), 30)
    expect_true(!is.na(or$y))
    if (minimize)    
      expect_true(or$y < 1)
    else
      expect_true(or$y > 100)
  }
  
  mins = c(TRUE, FALSE)
  crits = c("mean", "ei")
  
  for (minimize in mins) {
    for (crit in crits) {
      ctrl = mycontrol(minimize, crit)
      or = mbo(f, ps, NULL, learner, ctrl, show.info=FALSE)
      mycheck(or, minimize)
    }
  }
})
