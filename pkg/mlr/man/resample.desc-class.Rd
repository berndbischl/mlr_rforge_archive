\name{resample.desc-class}
\alias{resample.desc-class}
\alias{resample.desc}
\title{resample.desc}
\description{Base class for description of resampling algorithms.
A description of a resampling algorithm contains all necessary information to provide a resampling.instance, 
when given the size of the data set.}
\details{\code{resample.desc-class}: Getter.

\describe{
\item{instance.class [character]}{S4 class name of the corresponding resample.instance}
\item{name [character]}{Name of this resampling algorithm}
\item{iters [numeric]}{Number of iterations. Note that this the complete number of generated train/test sets, so for a 10 times repeated 5fold cross-validation it would be 50.}
}

}
\section{Extends}{\code{\linkS4class{object}}}
\section{Methods}{\describe{\item{\code{\link[=[,resample.desc-method]{[}}}{}\item{\code{\link[=to.string,resample.desc-method]{to.string}}}{}\item{\code{\link[=make.res.instance,resample.desc,missing,numeric-method]{make.res.instance}}}{}\item{\code{\link[=make.res.instance,resample.desc,learn.task,missing-method]{make.res.instance}}}{}}}
\alias{[,resample.desc-method}

