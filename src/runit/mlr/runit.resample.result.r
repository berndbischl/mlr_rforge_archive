

test.ResamplePrediction = function() {
  rin1 = makeResampleInstance(makeResampleDesc("BS", iters=4), task=multiclass.task)  
  rin2 = makeResampleInstance(makeResampleDesc("CV", iters=7), task=multiclass.task)  
  rin3 = makeResampleInstance(makeResampleDesc("Subsample", iters=2), task=multiclass.task)  
  
  lrn = makeLearner("classif.lda")
	p1 = resample(lrn, multiclass.task, rin1)$pred       
	p2 = resample(lrn, multiclass.task, rin2)$pred       
	p3 = resample(lrn, multiclass.task, rin3)$pred       
	
	inds = Reduce(c, rin1@test.inds)
	y = getTargets(multiclass.task)[inds]
	checkEquals(p1@df$id, inds)
	checkEquals(p1@df$truth, y)
  inds = Reduce(c, rin2@test.inds)
  y = getTargets(multiclass.task)[inds]
	checkEquals(p2@df$id, inds)
	checkEquals(p2@df$truth, y)
  inds = Reduce(c, rin3@test.inds)
  y = getTargets(multiclass.task)[inds]
	checkEquals(p3@df$id, inds)
	checkEquals(p3@df$truth, y)
}



