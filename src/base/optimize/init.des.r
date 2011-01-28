make.design = function(n, par.set, fun, fun.args) {
  
  lower = lower(par.set)
  upper = upper(par.set)
  vals = values(par.set)
  pars = par.set@pars
  
  inds.num = which(sapply(pars, function(x) x@type == "numeric"))
  inds.int = which(sapply(pars, function(x) x@type == "integer"))
  inds.int = which(sapply(pars, function(x) x@type == "logical"))
  inds.dis = which(sapply(pars, function(x) x@type == "discrete"))
  
  des = do.call(fun, c(list(n=n, k=length(pars)), fun.args))
  
  des = as.data.frame(des)
  
  for (i in 1:length(pars)) {
    p = pars[[i]]
    if (p@type == "numeric")
      des[,i] = p@trafo((upper(p)-lower(p))*des[,i] + lower(p))
    else if (p@type == "integer")
      des[,i] = p@trafo(floor((upper(p)-lower(p)+1)*des[,i] + lower(p)))
    else if (p@type == "logical")
      des[,i] = ifelse(des[,i] <= 0.5, FALSE, TRUE)
    else if (p@type == "discrete") {
      v = values(p)
      des[,i] = factor(names(v[ceiling(des[,i] * length(v))]), levels=v)
    }
  }
  colnames(des) = sapply(pars, function(x) x@id)
  return(des)
}


p1 = makeNumericParameter(id="x1", lower=1.1, upper=5.0, trafo=function(x) round(x,2))
p2 = makeIntegerParameter(id="x2", lower=9, upper=11)
p3 = makeDiscreteParameter(id="x3", vals=list(1,2,3))
p4 = makeLogicalParameter(id="x4")
p5 = makeDiscreteParameter(id="x5", vals=list("a", "b", "c", "d"))

b = makeParameterSet(p1, p2, p3, p4, p5)

d = init.design(10, b, randomLHS, list())
#print(d)

