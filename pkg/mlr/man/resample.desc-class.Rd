\name{resample.desc-class}
\alias{resample.desc-class}
\alias{resample.desc}
\title{resample.desc}
\description{Base class for description of resampling algorithms.
A description of a resampling algorithm contains all necessary information to provide a resampling.instance, 
when given the size of the data set.}
\section{Slots}{\describe{\item{\code{instance.class}:}{(\code{\link{character}}) S4 class name of the corresponding resample.instance}\item{\code{name}:}{(\code{\link{character}}) Name of this resampling algorithm}\item{\code{iters}:}{(\code{\link{numeric}}) Number of iterations}}}
\section{Methods}{\describe{\item{\code{\link[=[,resample.desc-method]{[}}}{\code{signature( = "resample.desc")}: Getter.}\item{\code{\link[=resample.fit,wrapped.learner,learn.task,resample.desc,list,character,character,function-method]{resample.fit}}}{}\item{\code{\link[=make.resample.instance,resample.desc,numeric-method]{make.resample.instance}}}{\code{signature(desc = "resample.desc", size = "numeric")}: Mainly for internal use. Construct a \code{\linkS4class{resample.instance}} from a \code{\linkS4class{resample.desc}}. 
Convenience method, so one doesn't have to call the specific constructors of classes inheriting from resample.desc.}}}
