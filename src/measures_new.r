

ct = make.task(data=iris, target=Species)
res = make.res.desc("cv", iters=3)
p = resample.fit("classif.lda", task=ct, res)
perf = performance(p)


m <- function(train.pred, train.x, train.y, test.pred, test.x, test.y,  model)
  
  
  resample.fit(..., models=T)
performance(..., task=ct)



performanceE:_measure <- function(model=FALSE, train=FALSE, test=TRUE, body) {
  m <- function(train.pred, train.x, train.y, train.id, test.pred, test.x, test.y, test.id, task, model) {}
  body(m) <- body
  attrib(m) <- list(requiires_train....)
  m
}

mse <- performance_measure(FALSE, FALSE, TRUE, {
    
  })
m: requires.



p  = resample.fit(wl, ct, bs)

b632_performance <- function(pm) {
  force(pm)
  f <- train.pred, train.x, train.y, train.id, test.pred, test.x, test.y, test.id, task, model) {
  trainerr <- pm(...)
  testerr <- pm(...)
  w*trainerr + (1-w)*testwerr
}
f
}

ooob_performance <- function(pm) {
  pm
}

ib_	

performance(p, list(b632_performance(pm)

measures = list(b632(mmce), oob(mmce), mmce)

