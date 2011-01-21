test.pardesc.randomval <- function() {
  x = numeric.parameter(name="x", lower=10, upper=20)
  r = random.val(13, x)
  checkTrue(is.numeric(r))
  checkEquals(length(r), 13)
  checkTrue(all(!is.na(r)))
  checkTrue(all(r >= x["lower"] & r <= x["upper"]))

  x = integer.parameter(name="x", lower=10, upper=20)
  r = random.val(13, x)
  checkTrue(is.integer(r))
  checkEquals(length(r), 13)
  checkTrue(all(!is.na(r)))
  checkTrue(all(r >= x["lower"] & r <= x["upper"]))
  
  x = logical.parameter(name="x")
  r = random.val(13, x)
  checkTrue(is.logical(r))
  checkEquals(length(r), 13)
  checkTrue(all(!is.na(r)))

  x = discrete.parameter(name="x", vals=c("a", "b", "c"))
  r = random.val(13, x)
  checkTrue(is.factor(r))
  checkEquals(length(r), 13)
  checkTrue(all(!is.na(r)))
  checkTrue(all(r %in% x["vals"]))
}
  