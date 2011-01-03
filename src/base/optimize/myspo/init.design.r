

init.design = function(par.descs, n.points) {
  inds.num = which(sapply(par.descs, function(x) is(x, "par.desc.double")))
  inds.rest = (1:length(par.descs))[-inds.num] 
  
  # LH sampling for numerics and ints
  lhc = randomLHS(n=n.points, k=length(inds.num))
  rescale = function(x, lower, upper) (upper-lower)*x + lower
  pds = par.descs[inds.num]
  lhc = sapply(seq(length=length(pds)), function(i) rescale(lhc[,i], pds[[i]]["lower"], pds[[i]]["upper"]))
  # round integer vars
  inds.int = which(sapply(pds, function(x) x["data.type"] == "integer"))
  lhc[, inds.int] = round(lhc[, inds.int])
  colnames(lhc) = sapply(pds, function(x) x["par.name"])

  des = as.data.frame(lhc)
  # sample other vars randomly
  for (i in inds.rest) {
    pd = par.descs[[i]]
    des[, pd["par.name"]] = sample.pardesc(n.points, pd)  
  }
  return(des)
}



