\name{make.resample.instance}
\alias{make.resample.instance,resample.desc,numeric-method}
\alias{make.resample.instance}
\title{Mainly for internal use.}
\usage{make.resample.instance(desc, size)}
\description{Mainly for internal use. Construct a \code{\linkS4class{resample.instance}} from a \code{\linkS4class{resample.desc}}. 
Convenience method, so one doesn't have to call the specific constructors of classes inheriting from resample.desc.}
\value{Object of corresponding subclass of \code{\linkS4class{resample.instance}}.}
\arguments{\item{desc}{[\code{\linkS4class{resample.desc}}] \cr Describes the resampling strategy.}
\item{size}{[integer] \cr Size of the data set to resample from.}
}
\examples{cv.d <- new("cv.desc", iters = 10)
rin <- make.resample.instance(desc = cv.d, size = nrow(iris))}
