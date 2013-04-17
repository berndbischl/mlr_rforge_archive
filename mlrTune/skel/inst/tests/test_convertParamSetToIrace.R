context("convertParamSetToIrace")

test_that("convertParamSetToIrace", {
  hookRun = function(instance, candidate, extra.params = NULL, config = list()) {
    1
  }
  instances = 1:10
  runIrace = function(ps) {
    ip = convertParamSetToIrace(ps)
    expect_equal(getParamIds(ps, repeated=TRUE, with.nr=TRUE), as.character(ip$names))
    res = capture.output(
      irace(
        tunerConfig = list(
          hookRun = hookRun, 
          instances = instances,
          maxExperiments = 10,
          logFile = tempfile()
        ),
        parameters = ip
      )
    )
    
  }
  
  ps = makeParamSet(
    makeLogicalParam("v1"),
    makeNumericParam("x1", lower=1, upper=4),
    makeIntegerParam("y1", lower=1, upper=4),
    makeDiscreteParam("z1", values=c("a", "b", "c")),
    makeLogicalVectorParam("v2", len=2),
    makeNumericVectorParam("x2", len=2, lower=1:2, upper=pi),
    makeIntegerVectorParam("y2", len=2, lower=0:1, upper=4),
    makeDiscreteVectorParam("z2", len=2, values=c("a", "b", "c"))
  )
  runIrace(ps)
})
