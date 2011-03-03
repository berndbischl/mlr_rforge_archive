test.varsel <- function() {
  inner = makeResampleDesc("CV", iter=2)
  
  # check all methods
  ctrl = exhvarsel.control(max.vars=2)
  vr = varsel("classif.lda", task=multiclass.task, resampling=inner, control=ctrl)
  checkEquals(length(as.list(vr@path)), 10) 
  checkEquals(nrow(as.data.frame(vr@path)), 10) 
  checkEquals(ncol(as.data.frame(vr@path)), 8) 
  # test printing
  print(vr)
  
  # check maxit
  ctrl = randomvarsel.control(maxit=4, path=TRUE)
  vr = varsel("classif.lda", task=multiclass.task, resampling=inner, control=ctrl)
  checkEquals(length(as.list(vr@path)), 4) 
  
  ctrl = sequential.control(method="sfs", alpha=0.01, path=TRUE)
  vr = varsel("classif.lda", task=multiclass.task, resampling=inner, control=ctrl)
  checkTrue(length(as.list(vr@path)) > 1) 
  
  ctrl = sequential.control(method="sbs", beta=0.01, path=TRUE)
  vr = varsel("classif.lda", task=multiclass.task, resampling=inner, control=ctrl)
  checkTrue(length(as.list(vr@path)) > 1) 
  
  ctrl = sequential.control(method="sffs", alpha=0.01, path=TRUE)
  vr = varsel("classif.lda", task=multiclass.task, resampling=inner, control=ctrl)
  # we must at least try to select a 2nd feature
  checkTrue(length(as.list(vr@path)) >= 1 + 4 + 1 + 3) 
  
  ctrl = sequential.control(method="sfbs", beta=0.01, path=TRUE)
  vr = varsel("classif.lda", task=multiclass.task, resampling=inner, control=ctrl)
  checkTrue(length(as.list(vr@path)) > 1) 
  
  
  
  # check max.vars
  ctrl = sequential.control(alpha=0, max.vars=1, method="sfs", path=TRUE)
  vr = varsel("classif.lda", task=binaryclass.task, resampling=inner, control=ctrl)
  checkEquals(length(vr@x), 1) 
  
  ctrl = sequential.control(beta=1, max.vars=58, method="sbs", path=TRUE)
  vr = varsel("classif.lda", task=binaryclass.task, resampling=inner, control=ctrl)
  checkEquals(length(vr@x), 58) 
  
  # check empty model
  ctrl = sequential.control(method="sfs", alpha=10)
  vr = varsel("classif.lda", task=multiclass.task, resampling=inner, control=ctrl)
  checkEquals(vr@x, character(0)) 
  
  wl = makeVarselWrapper("classif.lda", resampling=inner, control=ctrl)
  outer = makeResampleDesc("CV", iter=2)
  be = bench.exp(wl, task=multiclass.task, resampling=outer)
  
  # check bits
  ctrl = sequential.control(method="sfs", alpha=0.3)
  bn = c("b1", "b2")
  btf = function(b, task) {
    fns = getFeatureNames(task)
    Reduce(c, list(fns[1:2], fns[3:4])[as.logical(b)], init=character(0))
  } 
  vr = varsel("classif.lda", task=multiclass.task, resampling=inner, 
    bit.names=bns, bits.to.features=btf, control=ctrl)
  checkEquals(length(vr@x), 58) 
  
}

