\name{make.bs.instance}
\alias{make.bs.instance,numeric,numeric-method}
\alias{make.bs.instance}
\title{make.bs.instance}
\usage{make.bs.instance(size, iters)}
\description{\code{make.bs.instance} generates a \code{\linkS4class{bs.instance}} object, which encapsulates the generated indices of training and test sets.}
\value{A \code{\linkS4class{bs.instance}} object, which encapsulates the generated indices of training and test sets.}
\seealso{\code{\link{resample.fit}}, \code{\linkS4class{bs.instance}}}
\arguments{\item{size}{[integer] \cr Size of the data set to resample.}
\item{iters}{[integer] \cr Number of generated subsets / resampling iterations.}
}
\examples{data(iris)
rin <- make.bs.instance(size=nrow(iris), iters=10)}
