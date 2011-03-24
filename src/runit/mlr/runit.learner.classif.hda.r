#test.lda <- function() {
#	
#	set.seed(debug.seed)
#	m <- lda(formula=multiclass.formula, data=multiclass.train)
#	set.seed(debug.seed)
#	p <- predict(m, newdata=multiclass.test)
#	
#	simple.test("classif.hda", multiclass.df, multiclass.target, multiclass.train.inds, p$class)
#		
#	tt <- "hda"
#	tp <- function(model, newdata) predict(model, newdata)$class
#	
#	cv.test("classif.hda", multiclass.df, multiclass.target, tune.train=tt, tune.predict=tp )
#	
#	
#}



