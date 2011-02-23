test.ParmeterSet <- function() {
  ps1 = makeParameterSet(
    makeNumericParameter("u", lower=1),
    makeIntegerParameter("v", lower=1, upper=2),
    makeDiscreteParameter("w", vals=1:2),
    makeLogicalParameter("x")
  )
  checkEquals(lower(ps1), c(u=1, v=1L))
  checkEquals(upper(ps1), c(u=Inf, v=2L))
  vals = list(1,2); names(vals) = 1:2
  checkEquals(values(ps1), list(w=vals))
  checkEquals(values(ps1, only.names=TRUE), list(w=c("1", "2")))
  print(ps1)
  
  ps2 = makeParameterSet(
    makeNumericParameter("x", lower=1),
    makeIntegerVectorParameter("y", dim=2, lower=3L, upper=9L)
  )
  checkEquals(getRepeatedParameterIDs(ps2), c("x", "y", "y"))
  checkEquals(lower(ps2), c(x=1, y=3L, y=3L))
  checkEquals(upper(ps2), c(x=Inf, y=9L, y=9L))
  checkTrue(is.list(values(ps2)))
  checkEquals(length(values(ps2)), 0)
  print(ps2)
} 