#' List of supported learning algorithms. 
#' 
#' \itemize{ 
#' 		\item{classif.kknn}{ k-Nearest Neighbor from kknn package: \code{\link[kknn]{kknn}}}
#' 		\item{classif.naiveBayes}{ Naive Bayes from e1071 package: \code{\link[e1071]{naiveBayes}}}
#' 		\item{classif.lda}{ Linear Discriminant Analysis from MASS package: \code{\link[MASS]{lda}}}
#' 		\item{classif.qda}{ Quadratic Discriminant Analysis from MASS package: \code{\link[MASS]{qda}}}
#' 		\item{classif.rda}{ Regularized Discriminant Analysis from klaR package: \code{\link[klaR]{rda}}}
#' 		\item{classif.mda}{ Mixture Discriminant Analysis from mda package: \code{\link[mda]{mda}}}
#' 		\item{classif.logreg}{ Logistic Regression from stats package: \code{\link[stats]{lm}}}
#' 		\item{classif.rpart}{ Decision Tree from rpart package: \code{\link[rpart]{rpart}}}
#' 		\item{classif.randomForest}{ Random Forest from randomForest package: \code{\link[randomForest]{randomForest}}}
#' 		\item{classif.adabag}{ Boosting from adabag package: \code{\link[adabag]{adabag}}}
#' 		\item{classif.gbm}{ Gradient boosting machine from gbm package: \code{\link[gbm]{gbm}}}
#' 		\item{classif.ksvm}{ Support Vector Machines from kernlab package: \code{\link[kernlab]{ksvm}}}  
#' 		\item{classif.nnet}{ Neural Network from nnet package: \code{\link[nnet]{nnet}}}  
#' }
#' 
#' \itemize{ 
#' 		\item{regr.lm}{ Simple linear regression from stats package: \code{\link[stats]{errormatrix}}}
#' 		\item{regr.ridge}{ Ridge regression from penalized package: \code{\link[penalized]{penalized}}}
#' 		\item{regr.lasso}{ Lasso regression from penalized package: \code{\link[penalized]{penalized}}}
#' 		\item{regr.kknn}{ K-Nearest-Neigbor regression from kknn package: \code{\link[kknn]{kknn}}}
#' 		\item{regr.gbm}{ Gradient boosting machine from gbm package: \code{\link[gbm]{gbm}}}
#' 		\item{regr.blackboost}{ Gradient boosting with regression trees from mboost package: \code{\link[mboost]{blackboost}}}
#' 		\item{classif.ksvm}{ Support Vector Machines from kernlab package: \code{\link[kernlab]{ksvm}}}  
#' 		\item{regr.nnet}{ Neural Network from nnet package: \code{\link[nnet]{nnet}}}  
#' }
#'  
#' @title Implemented inducers.

learners = function() {}
