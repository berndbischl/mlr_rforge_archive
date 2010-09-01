



test.pardesc <- function() {
  pd = new("par.desc.num", par.name="x", default=0.001, lower=0)
  checkEquals(pd["data.type"], "numeric")
}
