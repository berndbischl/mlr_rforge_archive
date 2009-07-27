
test.restrict.learn.task = function() {
	
	ct <- new("classif.task", new("lda"), data=testsuite.df, formula=testsuite.formula)
	
	inds2 = c(1,7,10)
	inds3 = c(1,1,7,11,11)
	
	ct2 <- restrict.learn.task(ct, inds2)
	ct3 <- restrict.learn.task(ct, inds3)
	
	checkEquals(testsuite.df[inds2,], ct2@data)
	checkEquals(testsuite.df[inds3,], ct3@data)
}
