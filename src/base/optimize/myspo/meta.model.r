
train.meta.model = function(meta.learner, constr.learner, des, y, control) {
  good = !is.na(y)
  des.good = des[good, ]
  des.good[, control$y.name] = y[good]
  rt = make.task(target=control$y.name, data=des.good)
  meta.model = train(meta.learner, task=rt)
  if (sum(!good) > 0) {
    des[, control$y.name] = as.factor(good)
    ct = make.task(target=control$y.name, data=des)
    constr.model = train(constr.learner, ct)
  } else {
    constr.model = NULL
  }
  list(meta.model=meta.model, constr.model=constr.model)  
} 

pred.meta.model = function(meta.model, des) {
  predict(meta.model, newdata=des)["response"]
} 

sigma.meta.model = function(meta.model, des) {
  m = meta.model["learner.model"]
  p = predict(m, newdata=des, predict.all=TRUE)
  pmean = p$aggregate
  pind = p$individual
  sigma = colSum(apply(pind, 2, function(x) (x-pmean)^2))
  sigma = sigma / (ncol(pind) - 1)
  return(sigma)
} 


