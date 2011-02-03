#test.pardesc.randomval <- function() {
#  x = makeNumericParameter(id="x", lower=10, upper=20)
#  r = randomVal(x, 13)
#  checkTrue(is.numeric(r))
#  checkEquals(length(r), 13)
#  checkTrue(all(!is.na(r)))
#  checkTrue(all(r >= lower(x) & r <= upper(x)))
#
#  x = makeIntegerParameter(id="x", lower=10, upper=20)
#  r = randomVal(x, 13)
#  checkTrue(is.integer(r))
#  checkEquals(length(r), 13)
#  checkTrue(all(!is.na(r)))
#  checkTrue(all(r >= lower(x) & r <= upper(x)))
#  
#  x = makeLogicalParameter(id="x")
#  r = randomVal(x, 13)
#  checkTrue(is.logical(r))
#  checkEquals(length(r), 13)
#  checkTrue(all(!is.na(r)))
#
#  x = makeDiscreteParameter(id="x", vals=c("a", "b", "c"))
#  r = randomVal(x, 13)
#  checkTrue(is.character(r))
#  checkEquals(length(r), 13)
#  checkTrue(all(!is.na(r)))
#  checkTrue(all(r %in% x@constraints$vals))
#}
#  