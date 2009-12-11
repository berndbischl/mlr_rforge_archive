\name{make.regr.task}
\alias{make.regr.task,character,missing,data.frame,numeric-method}
\alias{make.regr.task}
\title{make.regr.task}
\usage{make.regr.task(target, formula, data, weights)}
\description{\code{make.regr.task} defines a regression task for a learner and a data set and is the starting point 
for further steps like training, predicting new data, resampling and tuning.}
\details{\code{make.regr.task} already performs quite a few tasks: It tries to load required package for the 
learner, sets up the learner to deal with a regression problem, gathers information about the features 
of the data set and the method, and compares whether they are compatible 
(e.g. some methods might not handle NAs or factors). It also might perform some data conversions 
in the data.frame, like coverting integer features to numerics, but will generally  
warn about this.

List of supported learning algorithms. The naming conventions are to add the package name as a prefix if
a learner is implemented in different packages and the suffix ".regr" if it can handle more than a
regression task.  

\itemize{ 
\item{\code{\linkS4class{stats.lm}}}{Simple linear regression from stats package}
\item{\code{\linkS4class{penalized.ridge}}}{Ridge regression from penalized package}
\item{\code{\linkS4class{penalized.lasso}}}{Lasso regression from penalized package}
\item{\code{\linkS4class{kknn.regr}}}{K-Nearest-Neigbor regression from kknn package}
\item{\code{\linkS4class{gbm.regr}}}{Gradient boosting machine from gbm package}
\item{\code{\linkS4class{blackboost.regr}}}{Gradient boosting with regression trees from mboost package}
}}
\value{An object of class \code{\linkS4class{regr.task}}.}
\seealso{\code{\linkS4class{regr.task}}, \code{\link{train}}, \code{\link{predict}}}
\arguments{\item{target}{[\code{\link{character}}] \cr
Name of the target variable.}
\item{data}{[\code{\link{data.frame}}] \cr
A data frame containing the variables in the model.}
\item{weights}{[\code{\link{numeric}}] \cr
An optional vector of weights to be used in the fitting process. Default is a weight of 1 for every case.}
}
\examples{library(mlbench)
data(BostonHousing)
# define a regression task for a Gradient Boosting Machine for regression for the data set BostonHousing
rt <- make.regr.task("gbm.regr", data = BostonHousing, target = "medv")}
