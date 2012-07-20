#' List of supported learning algorithms. 
#' 
#' \itemize{ 
#'   	\item{\bold{classif.ada}}{\cr Boosting from ada package: \code{\link[ada]{ada}}}
#' 		\item{\bold{classif.adaboost.M1}}{\cr Boosting from adabag package: \code{\link[adabag]{adaboost.M1}}}
#' 		\item{\bold{classif.blackboost}}{\cr Gradient boosting with regression trees from mboost package: \code{\link[mboost]{blackboost}}}
#' 		\item{\bold{classif.ctree}}{\cr Conditional Inference Trees from party package: \code{\link[party]{ctree}}}
#' 		\item{\bold{classif.fnn}}{\cr Fast k-Nearest Neighbor from FNN package: \code{\link[FNN]{knn}}}
#' 		\item{\bold{classif.gbm}}{\cr Gradient boosting machine from gbm package: \code{\link[gbm]{gbm}}}
#' 		\item{\bold{classif.glmboost}}{\cr Boosting for GLMs from mbboost package: \code{\link[mboost]{glmboost}}}
#' 		\item{\bold{classif.grplasso}}{\cr Logistic Regression with Group Lasso from grplasso package: \code{\link[grplasso]{grplasso}}}
#' 		\item{\bold{classif.j48}}{\cr J48 Decision Trees from RWeka package: \code{\link[RWeka]{J48}}}
#' 		\item{\bold{classif.JRip}}{\cr Propositional Rule Learner from RWeka package: \code{\link[RWeka]{JRip}}}
#' 		\item{\bold{classif.kknn}}{\cr k-Nearest Neighbor from kknn package: \code{\link[kknn]{kknn}}}
#' 		\item{\bold{classif.ksvm}}{\cr Support Vector Machines from kernlab package: \code{\link[kernlab]{ksvm}}\cr
#'      Note that kernel parameters have to be passed directly and not by using the kpar list in ksvm.}  
#' 		\item{\bold{classif.lda}}{\cr Linear Discriminant Analysis from MASS package: \code{\link[MASS]{lda}}}
#' 		\item{\bold{classif.loclda}}{\cr Local LDA from klaR package: \code{\link[klaR]{loclda}}}
#' 		\item{\bold{classif.logreg}}{\cr Logistic Regression from stats package: \code{\link[stats]{glm}}}
#' 		\item{\bold{classif.lssvm}}{\cr Least Squares Support Vector Machine from kernlab package: \code{\link[kernlab]{lssvm}}}
#' 		\item{\bold{classif.lvq1}}{\cr Learning Vector Quantization from class package: \code{\link[class]{lvq1}}}
#' 		\item{\bold{classif.mda}}{\cr Mixture Discriminant Analysis from mda package: \code{\link[mda]{mda}}}
#' 		\item{\bold{classif.multinom}}{\cr Multinomial Regression from nnet package: \code{\link[nnet]{multinom}}}
#' 		\item{\bold{classif.naiveBayes}}{\cr Naive Bayes from e1071 package: \code{\link[e1071]{naiveBayes}}}
#' 		\item{\bold{classif.nnet}}{\cr Neural Network from nnet package: \code{\link[nnet]{nnet}}}  
#' 		\item{\bold{classif.OneR}}{\cr 1-R classifier from RWeka package: \code{\link[RWeka]{OneR}}}
#' 		\item{\bold{classif.PART}}{\cr PART decision lists from RWeka package: \code{\link[RWeka]{PART}}}
#' 		\item{\bold{classif.penalizedSVM}}{\cr Support Vector Machines with L1 penalty from penalizedSVM package: \code{\link[penalizedSVM]{penalizedSVM}}}
#' 		\item{\bold{classif.qda}}{\cr Quadratic Discriminant Analysis from MASS package: \code{\link[MASS]{qda}}}
#' 		\item{\bold{classif.randomForest}}{\cr Random Forest from randomForest package: \code{\link[randomForest]{randomForest}}}
#' 		\item{\bold{classif.rda}}{\cr Regularized Discriminant Analysis from klaR package: \code{\link[klaR]{rda}}}
#' 		\item{\bold{classif.rpart}}{\cr Decision Tree from rpart package: \code{\link[rpart]{rpart}}}
#'   	\item{\bold{classif.sda}}{\cr Support Vector Machines (libsvm) from e1071 package: \code{\link[e1071]{svm}}}
#' 		\item{\bold{classif.svm}}{\cr Support Vector Machines (libsvm) from e1071 package: \code{\link[e1071]{svm}}}
#' }
#' 
#' \itemize{ 
#' 		\item{\bold{regr.blackboost}}{\cr Gradient boosting with regression trees from mboost package: \code{\link[mboost]{blackboost}}}
#' 		\item{\bold{regr.earth}}{\cr Multivariate Adaptive Regression Splines from earth package: \code{\link[earth]{earth}}}
#' 		\item{\bold{regr.fnn}}{\cr Fast k-Nearest Neighbor from FNN package: \code{\link[FNN]{knn}}}
#' 		\item{\bold{regr.gbm}}{\cr Gradient boosting machine from gbm package: \code{\link[gbm]{gbm}}}
#' 		\item{\bold{regr.kknn}}{\cr K-Nearest-Neighbor regression from kknn package: \code{\link[kknn]{kknn}}}
#'    \item{\bold{regr.km}}{\cr Kriging from DiceKriging package: \code{\link[DiceKriging]{km}}}
#' 		\item{\bold{regr.ksvm}}{\cr Support Vector Machines from kernlab package: \code{\link[kernlab]{ksvm}} \cr
#'      Note that kernel parameters have to be passed directly and not by using the kpar list in ksvm.}  
#' 		\item{\bold{regr.lasso}}{\cr Lasso regression from penalized package: \code{\link[penalized]{penalized}}}
#' 		\item{\bold{regr.lm}}{\cr Simple linear regression from stats package: \code{\link[stats]{lm}}}
#'    \item{\bold{regr.logicreg}}{\cr Logic regression from LogicReg package: \code{\link[LogicReg]{logreg}}}
#'    \item{\bold{regr.nnet}}{\cr Neural Network from nnet package: \code{\link[nnet]{nnet}}}
#' 		\item{\bold{regr.randomForest}}{\cr Random Forest from randomForest package: \code{\link[randomForest]{randomForest}}}
#' 		\item{\bold{regr.ridge}}{\cr Ridge regression from penalized package: \code{\link[penalized]{penalized}}}
#' 		\item{\bold{regr.rpart}}{\cr Decision Tree from rpart package: \code{\link[rpart]{rpart}}}
#' 		\item{\bold{regr.rsm}}{\cr Response surface regression from rsm package: \code{\link[rsm]{rsm}} \cr
#'      Note that you select the order of the regression by using modelfun="FO" (first order), "TWI" (two-way interactions, this is with 1st oder terms!) and "SO" (full second order)}.  
#'    \item{\bold{regr.rvm}}{\cr Relevance Vector Machine from rpart kernlab: \code{\link[kernlab]{rvm}} \cr
#'      Note that kernel parameters have to be passed directly and not by using the kpar list in rvm.}  
#' }
#' @name learners
#' @rdname learners
NULL

#' Find matching learning algorithms.
#' 
#' Returns the class names of learning algorithms which have specific characteristics, e.g.
#' whether they supports missing values, case weights, etc. 
#' 
#' The default of all boolean parameters is NA, meaning: property is not required, don't care.
#' 
#' @param x [string | \code{\linkS4class{LearnTask}}] \cr
#'   Type of the learning algorithm, either "classif" or "regr" or task to solve
#' @param doubles [\code{logical(1)}] \cr
#'   Supports real-valued inputs? Pass only when x is a string.
#' @param factors [\code{logical(1)}] \cr
#'   Supports factor inputs? Pass only when x is a string.
#' @param missings [\code{logical(1)}] \cr
#'   Supports missing values? Pass only when x is a string.
#' @param multiclass [\code{logical(1)}] \cr
#'   Supports multiclass problems? Pass only when x is a string.
#' @param weights [\code{logical(1)}] \cr
#'   Supports case weights? Pass only when x is a string.
#' @param probs [\code{logical(1)}] \cr
#'   Can predict probabilities?
#' @export 
getLearners = function(x = NA, doubles = NA, factors = NA, missings = NA, weights = NA, 
  multiclass = NA, probs = NA) {
  
  type = x
  meths = methods("makeRLearner")
  str_
  top.cl = switch(type, classif="rlearner.classif", regr="rlearner.regr", na="rlearner")
  ls = Filter(function(x) extends(x, top.cl) && x != top.cl , mlr.classes)
  
  f = function(x) {
    wl = try(makeLearner(x))
    if(is (wl, "try-error")) 
      return(NULL)
    else
      return(wl)
  }
  
  ls = lapply(ls, f)
  ls = Filter(function(x) !is.null(x), ls)
  
  
  f = function(x) {
    ( is.na(doubles) || doubles == x["numerics"] ) &&
      ( is.na(factors) || factors == x["factors"] ) &&
      ( is.na(characters) || characters == x["characters"] ) &&
      ( is.na(missings) || missings == x["missings"] ) &&
      ( is.na(multiclass) || multiclass == x["multiclass"] ) &&
      ( is.na(weights) || weights == x["weights"]  ) &&
      ( is.na(probs) || probs == x["prob"] )
  }
  
  ls = Filter(f, ls)
  ls = sapply(ls, function(x) as.character(class(x)))
  
  return(ls)
}
		
 
listLearners = function(type=as.logical(NA), numerics=as.logical(NA), factors=as.logical(NA),
                        missings=as.logical(NA), weights=as.logical(NA), 
                        oneclass=as.logical(NA), twoclass=as.logical(NA), multiclass=as.logical(NA), 
                        prob=as.logical(NA), se=as.logical(NA), warn.missing.packages=TRUE) {
  
  checkArg(type, choices=list("classif", "regr", as.logical(NA)), as.logical(NA))
  checkArg(numerics, "logical", len=1L, na.ok=TRUE)
  checkArg(factors, "logical", len=1L, na.ok=TRUE)
  checkArg(missings, "logical", len=1L, na.ok=TRUE)
  checkArg(weights, "logical", len=1L, na.ok=TRUE)
  checkArg(oneclass, "logical", len=1L, na.ok=TRUE)
  checkArg(twoclass, "logical", len=1L, na.ok=TRUE)
  checkArg(multiclass, "logical", len=1L, na.ok=TRUE)
  checkArg(prob, "logical", len=1L, na.ok=TRUE)
  checkArg(se, "logical", len=1L, na.ok=TRUE)
  checkArg(warn.missing.packages, "logical", len=1L, na.ok=FALSE)
  
  meths = as.character(methods("makeRLearner"))
  res = list()
  for (m in meths) {
    lrn = do.call(m, list())
     if (
          ( is.na(type) || type == lrn$type ) &&
          ( is.na(numerics) || doubles == lrn$numerics ) &&
          ( is.na(factors) || factors == lrn$factors ) &&
          ( is.na(missings) || missings == lrn$missings ) &&
          ( is.na(weights) || weights == lrn$weights  ) &&
          ( is.na(oneclass) || oneclass == lrn$oneclass ) &&
          ( is.na(twoclass) || twoclass == lrn$twoclass ) &&
          ( is.na(multiclass) || multiclass == lrn$multiclass ) &&
          ( is.na(prob) || prob == lrn$prob ) &&
          ( is.na(se) || se == lrn$se ) 
    ) {
        res[[length(res)+1]] = lrn
      }
  }
  sapply(res, function(lrn) class(lrn)[1])
}

listLearnersForTask = function(task) {
  checkArg(task, "SupervisedTask")
  td = task$task.desc
  listLearners(type=td$type, numerics=td$n.feat["numeric"] > 0, factors=td$n.feat["numeric"] > 0,
    missings=td$has.misisngs, weights=td$has.weights
    multiclass=length(td$class.levels) > 2, warn.missing.packages=TRUE)
  )
}
