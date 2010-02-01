\name{resample.instance-class}
\alias{resample.instance-class}
\alias{resample.instance}
\title{resample.instance}
\description{Base class for specific resampling draws like cross-validation or bootstrapping.
This class encapsulates training and test sets generated from the data set for a number of iterations. 
It mainly stores a set of integer vectors indicating the training examples for each iteration.
Don't create objects from this class directly but use the corresponding subclasses.
For construction simply use the factory method \code{\link{make.res.instance}} 
to get a \code{\linkS4class{cv.instance}}.}
\note{If you want to add another resampling strategy, have a look at the web documentation.}
\seealso{\code{\linkS4class{resample.desc}}, \code{\link{make.res.instance}}, \code{\link{resample.fit}}}
\section{Slots}{\describe{\item{\code{desc}:}{(\code{\link{resample.desc}}) Description object for resampling strategy}\item{\code{size}:}{(\code{\link{integer}}) Number of observations in data}\item{\code{inds}:}{(\code{\link{list}}) List of integer vectors specifying the training cases for each iteration. Each vector might contain duplicated indices and the order matters for some classifiers.}}}
\section{Methods}{\describe{\item{\code{\link[=benchmark,wrapped.learner,learn.task,resample.instance-method]{benchmark}}}{}\item{\code{\link[=resample.fit,wrapped.learner,learn.task,resample.instance,list,character,character,function-method]{resample.fit}}}{}\item{\code{\link[=initialize,resample.instance-method]{initialize}}}{\code{signature( = "resample.instance")}: This is mainly for internal use, you only need to use this, when you extend resample.instance to add another resampling strategy!}
