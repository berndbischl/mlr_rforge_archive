#' List of supported learning algorithms. 
#' 
#' \itemize{ 
#'   	\item{\bold{classif.ada}}{\cr Boosting from ada package: \code{\link[ada]{ada}}}
#' 		\item{\bold{classif.boosting}}{\cr Boosting from adabag package: \code{\link[adabag]{boosting}}}
#'      Note that \code{xval} has been set to 0 by default for speed.}  
#' 		\item{\bold{classif.blackboost}}{\cr Gradient boosting with regression trees from mboost package: \code{\link[mboost]{blackboost}}}
#' 		\item{\bold{classif.ctree}}{\cr Conditional Inference Trees from party package: \code{\link[party]{ctree}}}
#' 		\item{\bold{classif.fnn}}{\cr Fast k-Nearest Neighbor from FNN package: \code{\link[FNN]{knn}}}
#' 		\item{\bold{classif.gbm}}{\cr Gradient boosting machine from gbm package: \code{\link[gbm]{gbm}}}
#' 		\item{\bold{classif.glmboost}}{\cr Boosting for GLMs from mbboost package: \code{\link[mboost]{glmboost}}}
#'      Note that \code{family} has been set to \code{Binomial()} by default.}  
#' 		\item{\bold{classif.J48}}{\cr J48 Decision Trees from RWeka package: \code{\link[RWeka]{J48}}}
#' 		\item{\bold{classif.JRip}}{\cr Propositional Rule Learner from RWeka package: \code{\link[RWeka]{JRip}}}
#' 		\item{\bold{classif.kknn}}{\cr k-Nearest Neighbor from kknn package: \code{\link[kknn]{kknn}}}
#' 		\item{\bold{classif.ksvm}}{\cr Support Vector Machines from kernlab package: \code{\link[kernlab]{ksvm}}\cr
#'      Note that kernel parameters have to be passed directly and not by using the kpar list in ksvm.}  
#'      Note that \code{fit} has been set to \code{FALSE} by default for speed.}  
#' 		\item{\bold{classif.lda}}{\cr Linear Discriminant Analysis from MASS package: \code{\link[MASS]{lda}}}
#' 		\item{\bold{classif.logreg}}{\cr Logistic Regression from stats package: \code{\link[stats]{glm}}}
#' 		\item{\bold{classif.lssvm}}{\cr Least Squares Support Vector Machine from kernlab package: \code{\link[kernlab]{lssvm}}}
#'      Note that \code{fitted} has been set to \code{FALSE} by default for speed.}  
#' 		\item{\bold{classif.lvq1}}{\cr Learning Vector Quantization from class package: \code{\link[class]{lvq1}}}
#' 		\item{\bold{classif.mda}}{\cr Mixture Discriminant Analysis from mda package: \code{\link[mda]{mda}}}
#'      Note that \code{keep.fitted} has been set to \code{FALSE} by default for speed.}  
#' 		\item{\bold{classif.multinom}}{\cr Multinomial Regression from nnet package: \code{\link[nnet]{multinom}}}
#' 		\item{\bold{classif.naiveBayes}}{\cr Naive Bayes from e1071 package: \code{\link[e1071]{naiveBayes}}}
#' 		\item{\bold{classif.nnet}}{\cr Neural Network from nnet package: \code{\link[nnet]{nnet}}}  
#'      Note that \code{size} has been set to 3 by default.}  
#' 		\item{\bold{classif.OneR}}{\cr 1-R classifier from RWeka package: \code{\link[RWeka]{OneR}}}
#' 		\item{\bold{classif.PART}}{\cr PART decision lists from RWeka package: \code{\link[RWeka]{PART}}}
#' 		\item{\bold{classif.qda}}{\cr Quadratic Discriminant Analysis from MASS package: \code{\link[MASS]{qda}}}
#' 		\item{\bold{classif.randomForest}}{\cr Random Forest from randomForest package: \code{\link[randomForest]{randomForest}}}
#' 		\item{\bold{classif.rda}}{\cr Regularized Discriminant Analysis from klaR package: \code{\link[klaR]{rda}}}
#'      Note that \code{estimate.error} has been set to \code{FALSE} by default for speed.}  
#' 		\item{\bold{classif.rpart}}{\cr Decision Tree from rpart package: \code{\link[rpart]{rpart}}}
#'      Note that \code{xval} has been set to 0 by default for speed.}  
#' 		\item{\bold{classif.svm}}{\cr Support Vector Machines (libsvm) from e1071 package: \code{\link[e1071]{svm}}}
#' }
#' 
#' \itemize{ 
#' 		\item{\bold{regr.blackboost}}{\cr Gradient boosting with regression trees from mboost package: \code{\link[mboost]{blackboost}}}
#' 		\item{\bold{regr.earth}}{\cr Multivariate Adaptive Regression Splines from earth package: \code{\link[earth]{earth}}}
#' 		\item{\bold{regr.fnn}}{\cr Fast k-Nearest Neighbor from FNN package: \code{\link[FNN]{knn}}}
#' 		\item{\bold{regr.gbm}}{\cr Gradient boosting machine from gbm package: \code{\link[gbm]{gbm}}}
#'      Note that \code{distribution} has been set to \dQuote{gaussian} by default.}  
#' 		\item{\bold{regr.kknn}}{\cr K-Nearest-Neighbor regression from kknn package: \code{\link[kknn]{kknn}}}
#'    \item{\bold{regr.km}}{\cr Kriging from DiceKriging package: \code{\link[DiceKriging]{km}}}
#' 		\item{\bold{regr.ksvm}}{\cr Support Vector Machines from kernlab package: \code{\link[kernlab]{ksvm}} \cr
#'      Note that kernel parameters have to be passed directly and not by using the kpar list in ksvm.}  
#'      Note that \code{fit} has been set to \code{FALSE} by default for speed.}  
#' 		\item{\bold{regr.lasso}}{\cr Lasso regression from penalized package: \code{\link[penalized]{penalized}}}
#' 		\item{\bold{regr.lm}}{\cr Simple linear regression from stats package: \code{\link[stats]{lm}}}
#'    \item{\bold{regr.mars}}{\cr Multivariate Adaptive Regression Splines from mda package: \code{\link[mda]{mars}}}
#'    \item{\bold{regr.nnet}}{\cr Neural Network from nnet package: \code{\link[nnet]{nnet}}}
#'      Note that \code{size} has been set to 3 by default.}  
#'   	\item{\bold{regr.pcr}}{\cr Principal component regression from pls package: \code{\link[pls]{pcr}}}
#'      Note that \code{model} has been set to \code{FALSE} by default for speed.}  
#' 		\item{\bold{regr.randomForest}}{\cr Random Forest from randomForest package: \code{\link[randomForest]{randomForest}}}
#' 		\item{\bold{regr.ridge}}{\cr Ridge regression from penalized package: \code{\link[penalized]{penalized}}}
#' 		\item{\bold{regr.rpart}}{\cr Decision Tree from rpart package: \code{\link[rpart]{rpart}}}
#'      Note that \code{xval} has been set to 0 by default for speed.}  
#' 		\item{\bold{regr.rsm}}{\cr Response surface regression from rsm package: \code{\link[rsm]{rsm}} \cr
#'      Note that you select the order of the regression by using modelfun="FO" (first order), "TWI" (two-way interactions, this is with 1st oder terms!) and "SO" (full second order)}.  
#'    \item{\bold{regr.rvm}}{\cr Relevance Vector Machine from rpart kernlab: \code{\link[kernlab]{rvm}} \cr
#'      Note that kernel parameters have to be passed directly and not by using the kpar list in rvm.}  
#'      Note that \code{fit} has been set to \code{FALSE} by default for speed.}  
#' }
#' @name learners
#' @rdname learners
NULL

#' Find matching learning algorithms.
#' 
#' Returns the class names of learning algorithms which have specific characteristics, e.g.
#' whether they supports missing values, case weights, etc. 
#'  
#' The default for all search parameters is \code{NA}, meaning: property is not required, do not care.
#' 
#' Note that the packages of all learners are loaded during the search.
#' 
#' @param type [\code{character(1)}]\cr
#'   Type of the learning algorithm, either \dQuote{classif} or \dQuote{regr}.
#' @param numerics [\code{logical(1)}]\cr
#'   Supports real-valued features?
#' @param factors [\code{logical(1)}]\cr
#'   Supports factor features?
#' @param missings [\code{logical(1)}]\cr
#'   Supports missing values in features?
#' @param weights [\code{logical(1)}]\cr
#'   Supports case weights?
#' @param oneclass [\code{logical(1)}]\cr
#'   Supports oneclass problems?
#' @param twoclass [\code{logical(1)}]\cr
#'   Supports twoclass problems?
#' @param multiclass [\code{logical(1)}]\cr
#'   Supports multiclass problems?
#' @param prob [\code{logical(1)}]\cr
#'   Can predict probabilities (classification)?
#' @param se [\code{logical(1)}]\cr
#'   Can predict standard errors (regression)?
#' @param quiet [\code{logical(1)}]\cr
#'   Construct learners quietly to check their properties, shows no package startup messages.
#'   Turn off if you suspect errors.
#'   Default is code{TRUE}.
#' @param warn.missing.packages [\code{logical(1)}]\cr
#'   If some learner cannot be constructed because its package is missing, 
#'   should a warning be shown?
#'   Default is code{TRUE}.
#' @return [\code{character}]. Class names of matching learners.
#' @export 
listLearners = function(type=as.logical(NA), numerics=as.logical(NA), factors=as.logical(NA),
                        missings=as.logical(NA), weights=as.logical(NA), 
                        oneclass=as.logical(NA), twoclass=as.logical(NA), multiclass=as.logical(NA), 
                        prob=as.logical(NA), se=as.logical(NA), quiet=TRUE,
                        warn.missing.packages=TRUE) {
  
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
  errs = list()
  for (m in meths) {
    if (quiet)
      suppressAll(lrn <- try(do.call(m, list()), silent=TRUE))
    else
      lrn = try(do.call(m, list()))
    if (is.error(lrn)) {
      errs = c(errs, strsplit(as.character(m), "makeLearner.")[[1]][1])
    } else {
      if (
        ( is.na(type) || type == lrn$type ) &&
        ( is.na(numerics) || numerics == lrn$numerics ) &&
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
  }
  if (warn.missing.packages && length(errs) > 0)
    warningf("The following learners could not be constructed, probably because their packages are not installed:\n%s\nCheck ?learners to see which packages you need or install mlr with all suggestions.", collapse(errs))
  sapply(res, function(lrn) class(lrn)[1])
}

#' @param task [\code{\link{SupervisedTask}}]\cr 
#'   The task. Learners are returned that are applicable.
#' @export
#' @rdname listLearners
listLearnersForTask = function(task, prob=as.logical(NA), se=as.logical(NA), warn.missing.packages=TRUE) {
  checkArg(task, "SupervisedTask")
  td = task$task.desc

  numerics = ifelse(td$n.feat["numerics"] > 0, TRUE, NA)
  factors = ifelse(td$n.feat["factors"] > 0, TRUE, NA)
  missings = ifelse(td$has.missings, TRUE, NA)
  weights = ifelse(td$has.weights, TRUE, NA)
  oneclass = ifelse(td$type=="classif" && length(td$class.levels) == 1L, TRUE, NA)
  twoclass = ifelse(td$type=="classif" && length(td$class.levels) == 2L, TRUE, NA)
  multiclass = ifelse(td$type=="classif" && length(td$class.levels) > 2L, TRUE, NA)
  
  listLearners(type=td$type, numerics=numerics, factors=factors,
    missings=missings, weights=weights,
    oneclass=oneclass, twoclass=twoclass, multiclass=multiclass,
    prob=prob, se=se, warn.missing.packages=warn.missing.packages)
}
