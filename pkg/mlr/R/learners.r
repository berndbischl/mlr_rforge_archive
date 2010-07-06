#' List of supported learning algorithms. 
#' 
#' \itemize{ 
#' 		\item{\bold{classif.ada}}{\cr Boosting from ada package: \code{\link[ada]{ada}}}
#' 		\item{\bold{classif.adaboost.M1}}{\cr Boosting from adabag package: \code{\link[adabag]{adaboost.M1}}}
#' 		\item{\bold{classif.blackboost}}{\cr Gradient boosting with regression trees from mboost package: \code{\link[mboost]{blackboost}}}
#' 		\item{\bold{classif.gbm}}{\cr Gradient boosting machine from gbm package: \code{\link[gbm]{gbm}}}
#' 		\item{\bold{classif.glmboost}}{\cr Boosting for GLMs from mbboost package: \code{\link[mboost]{glmboost}}}
#' 		\item{\bold{classif.grplasso}}{\cr Logistic Regression with Group Lasso from grplasso package: \code{\link[grplasso]{grplasso}}}
#' 		\item{\bold{classif.j48}}{\cr J48 Decision Trees from RWeka package: \code{\link[RWeka]{J48}}}
#' 		\item{\bold{classif.kknn}}{\cr k-Nearest Neighbor from kknn package: \code{\link[kknn]{kknn}}}
#' 		\item{\bold{classif.ksvm}}{\cr Support Vector Machines from kernlab package: \code{\link[kernlab]{ksvm}}\cr
#'      Note that kernel parameters have to be passed directly and not by using the kpar list in ksvm.}  
#' 		\item{\bold{classif.lda}}{\cr Linear Discriminant Analysis from MASS package: \code{\link[MASS]{lda}}}
#' 		\item{\bold{classif.loclda}}{\cr Local LDA from klaR package: \code{\link[klaR]{loclda}}}
#' 		\item{\bold{classif.logreg}}{\cr Logistic Regression from stats package: \code{\link[stats]{glm}}}
#' 		\item{\bold{classif.lvq1}}{\cr Learning Vector Quantization from class package: \code{\link[class]{lvq1}}}
#' 		\item{\bold{classif.mda}}{\cr Mixture Discriminant Analysis from mda package: \code{\link[mda]{mda}}}
#' 		\item{\bold{classif.multinom}}{\cr Multinomial Regression from nnet package: \code{\link[nnet]{multinom}}}
#' 		\item{\bold{classif.naiveBayes}}{\cr Naive Bayes from e1071 package: \code{\link[e1071]{naiveBayes}}}
#' 		\item{\bold{classif.nnet}}{\cr Neural Network from nnet package: \code{\link[nnet]{nnet}}}  
#' 		\item{\bold{classif.penalizedSVM}}{\cr Support Vector Machines with L1 penalty from penalizedSVM package: \code{\link[penalizedSVM]{penalizedSVM}}}
#' 		\item{\bold{classif.qda}}{\cr Quadratic Discriminant Analysis from MASS package: \code{\link[MASS]{qda}}}
#' 		\item{\bold{classif.randomForest}}{\cr Random Forest from randomForest package: \code{\link[randomForest]{randomForest}}}
#' 		\item{\bold{classif.rda}}{\cr Regularized Discriminant Analysis from klaR package: \code{\link[klaR]{rda}}}
#' 		\item{\bold{classif.rpart}}{\cr Decision Tree from rpart package: \code{\link[rpart]{rpart}}}
#' 		\item{\bold{classif.svm}}{\cr Support Vector Machines (libsvm) from e1071 package: \code{\link[e1071]{svm}}}
#' }
#' 
#' \itemize{ 
#' 		\item{\bold{regr.blackboost}}{\cr Gradient boosting with regression trees from mboost package: \code{\link[mboost]{blackboost}}}
#' 		\item{\bold{regr.gbm}}{\cr Gradient boosting machine from gbm package: \code{\link[gbm]{gbm}}}
#' 		\item{\bold{regr.kknn}}{\cr K-Nearest-Neighbor regression from kknn package: \code{\link[kknn]{kknn}}}
#' 		\item{\bold{regr.ksvm}}{\cr Support Vector Machines from kernlab package: \code{\link[kernlab]{ksvm}} \cr
#'      Note that kernel parameters have to be passed directly and not by using the kpar list in ksvm.}  
#' 		\item{\bold{regr.lasso}}{\cr Lasso regression from penalized package: \code{\link[penalized]{penalized}}}
#' 		\item{\bold{regr.lm}}{\cr Simple linear regression from stats package: \code{\link[stats]{lm}}}
#' 		\item{\bold{regr.randomForest}}{\cr Random Forest from randomForest package: \code{\link[randomForest]{randomForest}}}
#' 		\item{\bold{regr.ridge}}{\cr Ridge regression from penalized package: \code{\link[penalized]{penalized}}}
#' 		\item{\bold{regr.rpart}}{\cr Decision Tree from rpart package: \code{\link[rpart]{rpart}}}
#' }
#'  
#' @title Implemented inducers.

learners = function() {}
