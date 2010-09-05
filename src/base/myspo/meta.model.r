
train.meta.model = function(meta.learner, constr.learner, des, y) {
  good = !is.na(y)
  des.good = des[good, ]
  des.good$y = y[good]
  rt = make.task(target="y", data=des.good)
  meta.model = train(meta.learner, rt)
  
  if (sum(!good) > 0) {
    des$y = as.factor(good)
    ct = make.task(target="y", data=des)
    constr.model = train(constr.learner, ct)
  } else {
    constr.model = NULL
  }
  list(meta.model=meta.model, constr.model=constr.model)  
} 

pred.meta.model = function(meta.model, des) {
  predict(meta.model, newdata=des)["response"]
} 

