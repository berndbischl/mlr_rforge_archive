\name{resample.desc-class}
\alias{resample.desc-class}
\alias{resample.desc}
\title{resample.desc}
\description{Base class for description of resampling algorithms.
A description of a resampling algorithm contains all necessary information to provide a resampling.instance, 
when given the size of the data set.}
\section{Slots}{\describe{\item{\code{instance.class}:}{(\code{\link{character}}) S4 class name of the corresponding resample.instance}\item{\code{name}:}{(\code{\link{character}}) Name of this resampling algorithm}\item{\code{iters}:}{(\code{\link{numeric}}) Number of iterations}}}
\section{Methods}{\describe{\item{\code{\link[=benchmark,learn.task,resample.instance,resample.desc,list,list,logical-method]{benchmark}}}{\code{signature(learn.task = "learn.task", outer.resampling = "resample.instance", inner.resampling = "resample.desc", ranges = "list", measure = "list", all.tune.results = "logical")}: \code{benchmark} conducts a benchmark experiment for a single classifier on a single
data set. This consists of an inner stage and outer stage. At the outer stage a 
tuning set and a test set are repeatedly formed from the data through resampling 
(usually cross-validation or bootstrapping). The respective hyperparameters of the 
classifier are tuned on the tuning set again through an inner resampling process,
the classifier is trained on the complete tuning set with the best found 
hyperparameters and the performance is measured on the test set.}\item{\code{\link[=[,resample.desc-method]{[}}}{\code{signature( = "resample.desc")}: Getter.}\item{\code{\link[=make.resample.instance,resample.desc,numeric-method]{make.resample.instance}}}{\code{signature(desc = "resample.desc", size = "numeric")}: Mainly for internal use. Construct a \code{\linkS4class{resample.instance}} from a \code{\linkS4class{resample.desc}}. 
Convenience method, so one doesn't have to call the specific constructors of classes inheriting from resample.desc.}}}
