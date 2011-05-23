test.varsel <- function() {
  inner = makeResampleDesc("CV", iter=2)
  
  # check all methods
  ctrl = makeVarselControlExhaustive(max.vars=2)
  vr = varsel("classif.lda", task=multiclass.task, resampling=inner, control=ctrl)
  checkEquals(getLength(vr@path), 11) 
  checkEquals(nrow(as.data.frame(vr@path)), 11) 
  checkEquals(ncol(as.data.frame(vr@path)), 8) 
  # test printing
  print(vr)
  
  # check maxit
  ctrl = makeVarselControlRandom(maxit=4, path=TRUE)
  vr = varsel("classif.lda", task=multiclass.task, resampling=inner, control=ctrl)
  checkEquals(getLength(vr@path), 4) 
  
  ctrl = makeVarselControlSequential(method="sfs", alpha=0.01, path=TRUE)
  vr = varsel("classif.lda", task=multiclass.task, resampling=inner, control=ctrl)
  checkTrue(getLength(vr@path) > 1) 
  
  ctrl = makeVarselControlSequential(method="sbs", beta=0.01, path=TRUE)
  vr = varsel("classif.lda", task=multiclass.task, resampling=inner, control=ctrl)
  checkTrue(getLength(vr@path) > 1) 
  
  ctrl = makeVarselControlSequential(method="sffs", alpha=0.01, path=TRUE)
  vr = varsel("classif.lda", task=multiclass.task, resampling=inner, control=ctrl)
  # we must at least try to select a 2nd feature
  checkTrue(getLength(vr@path) >= 1 + 4 + 1 + 3) 
  
  ctrl = makeVarselControlSequential(method="sfbs", beta=0.01, path=TRUE)
  vr = varsel("classif.lda", task=multiclass.task, resampling=inner, control=ctrl)
  checkTrue(getLength(vr@path) > 1) 
  
  
  
  # check max.vars
  ctrl = makeVarselControlSequential(alpha=0, max.vars=1, method="sfs", path=TRUE)
  vr = varsel("classif.lda", task=binaryclass.task, resampling=inner, control=ctrl)
  checkEquals(length(vr@x), 1) 
  
  ctrl = makeVarselControlSequential(beta=1, max.vars=58, method="sbs", path=TRUE)
  vr = varsel("classif.lda", task=binaryclass.task, resampling=inner, control=ctrl)
  checkEquals(length(vr@x), 58) 
  
  # check empty model
  ctrl = makeVarselControlSequential(method="sfs", alpha=10)
  vr = varsel("classif.lda", task=multiclass.task, resampling=inner, control=ctrl)
  checkEquals(vr@x, character(0)) 
  
  wl = makeVarselWrapper("classif.lda", resampling=inner, control=ctrl)
  outer = makeResampleDesc("CV", iter=2)
  r = resample(wl, task=multiclass.task, resampling=outer)
  
  # check bits
  bns = c("b1", "b2")
  btf = function(b, task) {
    fns = getFeatureNames(task)
    Reduce(c, list(fns[1:2], fns[3:4])[as.logical(b)], init=character(0))
  } 
  
  ctrl = makeVarselControlRandom(maxit=3)
  vr = varsel("classif.lda", task=multiclass.task, resampling=inner, bit.names=bns, bits.to.features=btf, control=ctrl)
  df = as.data.frame(vr@path) 
  checkEquals(colnames(df), c("b1", "b2", "mmce.test.mean", "mmce.test.sd", "dob", "eol"))
  checkEquals(nrow(df), 3)
  
  ctrl = makeVarselControlExhaustive()
  vr = varsel("classif.lda", task=multiclass.task, resampling=inner, bit.names=bns, bits.to.features=btf, control=ctrl)
  df = as.data.frame(vr@path) 
  checkEquals(colnames(df), c("b1", "b2", "mmce.test.mean", "mmce.test.sd", "dob", "eol"))
  checkEquals(nrow(df), 4)
  
  ctrl = makeVarselControlSequential(method="sfs", alpha=0)
  vr = varsel("classif.lda", task=multiclass.task, resampling=inner, bit.names=bns, bits.to.features=btf, control=ctrl)
  df = as.data.frame(vr@path) 
  checkEquals(colnames(df), c("b1", "b2", "mmce.test.mean", "mmce.test.sd", "dob", "eol"))
  checkEquals(nrow(df), 4)
}

