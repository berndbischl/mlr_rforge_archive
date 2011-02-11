

test.ResamplePrediction = function() {
  rin1 = make.res.instance(make.res.desc("bs", iters=4), task=multiclass.task)  
  rin2 = make.res.instance(make.res.desc("cv", iters=7), task=multiclass.task)  
  rin3 = make.res.instance(make.res.desc("subsample", iters=2), task=multiclass.task)  
  
	p1 = resample("classif.lda", multiclass.task, rin1)$pred       
	p2 = resample("classif.lda", multiclass.task, rin2)$pred       
	p3 = resample("classif.lda", multiclass.task, rin3)$pred       
	
	inds = Reduce(c, rin1@test.inds)
	y = multiclass.task["targets"][inds]
	checkEquals(p1@df$id, inds)
	checkEquals(p1@df$truth, y)
  inds = Reduce(c, rin2@test.inds)
  y = multiclass.task["targets"][inds]
	checkEquals(p2@df$id, inds)
	checkEquals(p2@df$truth, y)
  inds = Reduce(c, rin3@test.inds)
  y = multiclass.task["targets"][inds]
	checkEquals(p3@df$id, inds)
	checkEquals(p3@df$truth, y)
}



