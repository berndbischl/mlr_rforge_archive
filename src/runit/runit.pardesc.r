test.pardesc <- function() {
  pd1 = new("par.desc.double", par.name="x", default=0.001, lower=0)
  checkEquals(pd1["data.type"], "numeric")
  
  v = make.learner("classif.rpart", predict.type="prob")
  checkTrue(all.names(v["par.descs"]))
 
  w = new("base.wrapper", learner=v, par.descs=list(pd1))
  checkTrue(all.names(w["par.descs"]))
  w = make.probth.wrapper(v, classes=multiclass.task["class.levels"])
  checkTrue(all.names(w["par.descs"]))
  w = make.multiclass.wrapper(v)
  checkTrue(all.names(w["par.descs"]))
  w = make.filter.wrapper(v, fw.threshold=0.5, fw.method="chi.squared")
  checkTrue(all.names(w["par.descs"]))
  w = make.preproc.wrapper(v, 
    train=function(data, targetvar, args) data, 
    predict=function(data, targetvar, args, control) data,
    args = list(x=1, y=2)
  )
  checkException(
    make.preproc.wrapper(v, 
      train=function(data, targetvar, args) data, 
      predict=function(data, targetvar, args, control) data,
      args = list(minsplit=1)
    ), silent=TRUE)
  s = geterrmessage()
  checkTrue(length(grep("yperparameter names in wrapper clash with base learner names: minsplit", s)) >0 )
}
