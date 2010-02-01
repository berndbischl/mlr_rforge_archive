\name{make.regr.task}
\alias{make.regr.task-methods}
\alias{make.regr.task}
\title{Contruct regression task}
\usage{make.regr.task(name, data, target, formula, excluded, weights)}
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
\item{\code{\linkS4class{stats.lm}}}{ Simple linear regression from stats package}
\item{\code{\linkS4class{penalized.ridge}}}{ Ridge regression from penalized package}
\item{\code{\linkS4class{penalized.lasso}}}{ Lasso regression from penalized package}
\item{\code{\linkS4class{kknn.regr}}}{ K-Nearest-Neigbor regression from kknn package}
\item{\code{\linkS4class{gbm.regr}}}{ Gradient boosting machine from gbm package}
\item{\code{\linkS4class{blackboost.regr}}}{ Gradient boosting with regression trees from mboost package}
}}
\value{An object of class \code{\linkS4class{regr.task}}.}
\seealso{\code{\linkS4class{regr.task}}}
\arguments{\item{name}{[\code{\link{character}}] \cr
Name of task / data set to be used string representations later on. Default is empty string.}
\item{target}{[\code{\link{character}}] \cr
Name of the target variable.}
\item{formula}{[\code{\link{formula}}] \cr
Instead of specifying the target, you can use the formula interface. 
If you are using just a subset of the variables of transformations of the variables, this will built a new internal 
data frame by calling \code{\link{model.frame}}.}
\item{data}{[\code{\link{data.frame}}] \cr
A data frame containing the variables in the model.}
\item{excluded}{[\code{\link{character}}]
Names of inputs, which should be generally disregarded, e.g. IDs, etc. Default is zero-length vector.}
\item{weights}{[\code{\link{numeric}}] \cr
An optional vector of weights to be used in the fitting process. Default is a weight of 1 for every case.}
}
\examples{library(mlbench)
data(BostonHousing)
# define a regression for the data set BostonHousing
rt <- make.regr.task(data = BostonHousing, target = "medv")}
