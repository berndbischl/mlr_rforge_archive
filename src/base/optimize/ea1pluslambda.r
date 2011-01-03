ea1pluslambda = function(f, par.descs, control) {
  m = length(par.descs)
  curx = list()
  cury = f(curx)
  step.sizes = list()
  for (i in 1:m) {
    pd = par.descs[[i]]
    if (is(pd, "par.desc.double")) 
      step.sizes[[pd["par.name"]]] = (pd["upper"] - pd["lower"]) / 100
  }
  step.sizes = as.numeric(step.sizes)
  print(step.sizes)
  
  offspring = list()
  for (i in 1:m) {
    pd = par.descs[[i]]
    if (is(pd, "par.desc.double")) 
      step.sizes[[pd["par.name"]]] = (pd["upper"] - pd["lower"]) / 100
  }
  #ys = sapply(1:nrow(offspring), function(i) do.call(f, data.frame.row.to.list(offspring, i)))
  ys = f(offspring)
  j = which.min(ys)
  if (ys[j] < cury) {
    cury = ys[j]
    curx = data.frame.row.to.list(offspring, i)
  }
}