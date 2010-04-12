

remove.patterns <<- c(
		"\\\\item\\{\\\\code\\{\\\\link\\[=initialize,.*Constructor.\\}",
		"\\\\alias\\{\\[\\}"
				
#		"\\\\alias\\{initialize\\}",
#		"\\\\alias\\{as.character\\}",
#		"\\\\alias\\{show\\}",
#		"\\\\alias\\{predict\\}",
#		"\\\\alias\\{set.train.par\\}",
#		"\\\\alias\\{set.predict.par\\}",
#		"\\\\alias\\{train.learner\\}",
#		
#		"\\\\item\\{\\\\code\\{\\\\link\\[=make.default.measure.*make.default.measure\\}\\}\\}\\{\\}",
#		"\\\\item\\{\\\\code\\{\\\\link\\[=train,classif.task.*train\\}\\}\\}\\{\\}",
#		"\\\\item\\{\\\\code\\{\\\\link\\[=train,regr.task.*train\\}\\}\\}\\{\\}",
#
#		"\\\\item\\{\\\\code\\{\\\\link\\[=train.learner,.*train.learner\\}\\}\\}",
#		"\\\\item\\{\\\\code\\{\\\\link\\[=train.learner,.*train.learner\\}\\}\\}",
#		"\\\\item\\{\\\\code\\{\\\\link\\[=predict.learner,.*predict.learner\\}\\}\\}",
#		"\\\\item\\{\\\\code\\{\\\\link\\[=predict.learner,.*predict.learner\\}\\}\\}",
#		
#		"\\\\item\\{\\\\code\\{\\\\link\\[=initialize,lda-method.*Constructor.\\}",
#		"\\\\item\\{\\\\code\\{\\\\link\\[=initialize,qda-method.*Constructor.\\}",
#		"\\\\item\\{\\\\code\\{\\\\link\\[=initialize,rda-method.*Constructor.\\}",
#		"\\\\item\\{\\\\code\\{\\\\link\\[=initialize,mda-method.*Constructor.\\}",
#		"\\\\item\\{\\\\code\\{\\\\link\\[=initialize,loclda-method.*Constructor.\\}",
#		"\\\\item\\{\\\\code\\{\\\\link\\[=initialize,logreg-method.*Constructor.\\}",
#		"\\\\item\\{\\\\code\\{\\\\link\\[=initialize,multinom-method.*Constructor.\\}",
#		"\\\\item\\{\\\\code\\{\\\\link\\[=initialize,kknn.classif-method.*Constructor.\\}",
#		"\\\\item\\{\\\\code\\{\\\\link\\[=initialize,naiveBayes-method.*Constructor.\\}",
#		"\\\\item\\{\\\\code\\{\\\\link\\[=initialize,rpart.classif-method.*Constructor.\\}",
#		"\\\\item\\{\\\\code\\{\\\\link\\[=initialize,randomForest.classif-method.*Constructor.\\}",
#		"\\\\item\\{\\\\code\\{\\\\link\\[=initialize,adaboost-method.*Constructor.\\}",
#		"\\\\item\\{\\\\code\\{\\\\link\\[=initialize,kernlab.svm.classif-method.*Constructor.\\}",
#		
#		"\\\\item\\{\\\\code\\{\\\\link\\[=initialize,stats.lm-method.*Constructor.\\}",
#		"\\\\item\\{\\\\code\\{\\\\link\\[=initialize,penalized.ridge-method.*Constructor.\\}",
#		"\\\\item\\{\\\\code\\{\\\\link\\[=initialize,penalized.lasso-method.*Constructor.\\}",
#		"\\\\item\\{\\\\code\\{\\\\link\\[=initialize,kknn.regr-method.*Constructor.\\}",
#		"\\\\item\\{\\\\code\\{\\\\link\\[=initialize,kknn.regr-method.*Constructor.\\}",
#		"\\\\item\\{\\\\code\\{\\\\link\\[=initialize,blackboost.regr-method.*Constructor.\\}",
#		"\\\\item\\{\\\\code\\{\\\\link\\[=initialize,gbm.regr-method.*Constructor.\\}",
#		
#		"\\\\item\\{\\\\code\\{\\\\link\\[=to.string,.*\\}",
#		"\\\\item\\{\\\\code\\{\\\\link\\[=show,.*Shows the object by calling as.character.\\}",
#		"\\\\item\\{\\\\code\\{\\\\link\\[=print,.*Prints the object by calling as.character.\\}"

#initialize,adaboost-method"                                                                                       
#[15] "       missing link(s):  initialize,blackboost.regr-method"                                                                                
#[16] "       missing link(s):  predict,classif.task-method initialize,classif.task-method [,classif.task-method as.character,classif.task-method"
#[17] "       missing link(s):  initialize,gbm.regr-method"                                                                                       
#[18] "       missing link(s):  initialize,kernlab.svm.classif-method train.learner,kernlab.svm.classif,formula,data.frame,numeric,list-method"   
#[20] "       missing link(s):  initialize,kknn.regr-method"                                                                                      
#[22] "       missing link(s):  [,learn.task-method"                                                                                              
#[24] "       missing link(s):  penalized.ridge-class"                                                                                            
#[27] "       missing link(s):  initialize,nnet.multinom-method"                                                                                  
#[28] "       missing link(s):  initialize,penalized.lasso-method train.learner,penalized.lasso,formula,data.frame,numeric,list-method"           
#[29] "       missing link(s):  initialize,penalized.ridge-method train.learner,penalized.ridge,formula,data.frame,numeric,list-method"           
#[31] "       missing link(s):  initialize,randomForest.classif-method"                                                                           
)

remove.exp <-function(file) {
	x <- paste(readLines(file), collapse="###")
	for (pattern in remove.patterns) {
		x <- sub(pattern, "" ,x)
	}
	x <- strsplit(x, "###")[[1]]
	writeLines(x, file)
	#writeLines(x, "c:/test.txt")
}

#remove.exp("d:/sync/projekte/mlr/pkg/mlr/man/lda-class.Rd")

