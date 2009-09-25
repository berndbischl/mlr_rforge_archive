\name{make.cv.instance}
\alias{make.cv.instance,numeric,numeric-method}
\alias{make.cv.instance}
\title{make.cv.instance}
\usage{make.cv.instance(size, iters)}
\description{\code{make.cv.instance} generates a \code{\linkS4class{cv.instance}} object, which encapsulates the generated indices of training and test sets.}
\value{A \code{\linkS4class{cv.instance}} object, which encapsulates the generated indices of training and test sets.}
\seealso{\code{\link{resample.fit}}, \code{\linkS4class{cv.instance}}}
\arguments{\item{size}{[integer] \cr Size of the data set to resample.}
\item{iters}{[integer] \cr Number of generated subsets / resampling iterations.}
}
\examples{data(iris)
rin <- make.cv.instance(size=nrow(iris), iters=10)}
