\name{benchmark}
\alias{benchmark,learn.task,resample.instance,resample.desc,list,list,logical-method}
\alias{benchmark}
\title{benchmark}
\usage{benchmark(learn.task, outer.resampling, inner.resampling, ranges, measure, all.tune.results)}
\description{\code{benchmark} conducts a benchmark experiment for a single classifier on a single
data set. This consists of an inner stage and outer stage. At the outer stage a 
tuning set and a test set are repeatedly formed from the data through resampling 
(usually cross-validation or bootstrapping). The respective hyperparameters of the 
classifier are tuned on the tuning set again through an inner resampling process,
the classifier is trained on the complete tuning set with the best found 
hyperparameters and the performance is measured on the test set.}
\value{If \code{all.tune.results} is FALSE (default) benchmark returns a list 
containing the best parameter combinations, their inner run mean performance, 
the standard deviation of their inner run performance and their test performance.
If \code{all.tune.results} is TRUE the output contains additional information about all
tested parameters by the inner runs.}
\arguments{\item{learn.task}{[\code{\linkS4class{learn.task}}] \cr
Learning task.}
\item{ranges}{[\code{\link{list}}] \cr 
List of named range vectors/list for hyperparameters used in tuning 
(see \code{\link{tune}}).}
\item{measure}{[\code{\link{character}}/\code{\link{list}}] \cr 
Name of performance measure to optimize or a list describing your own performance measure. 
The default is mean misclassification error.}
\item{outer.resampling}{[\code{\link{resample.instance}}] \cr 
Specifies the training and test indices of the resampled data used in outer stage.}
\item{inner.resampling}{[\code{\link{resample.desc}}] \cr 
Describes resampling method to be used in inner stage.}
\item{all.tune.results}{[\code{\link{logical}}] \cr 
Should complete results for all inner tunings be returned? (default is FALSE)}
}
\examples{# set up the learning task and parameter grid
ct <- make.classif.task("kernlab.svm.classif", data=iris, formula=Species~.)
ranges <- list(kernel="polydot", degree=1:3, C=2^seq(-2,2))
# create the outer cross-validation
or <- make.cv.instance(iters=5, size=nrow(iris))					
# describe the inner cross-validation
ir <- make.cv.desc(iters=3)
benchmark(learn.task=ct, ranges=ranges, outer.resampling=or, inner.resampling=ir)}
