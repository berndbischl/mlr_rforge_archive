\name{make.subsample.instance}
\alias{make.subsample.instance,numeric,numeric,numeric-method}
\alias{make.subsample.instance}
\title{make.subsample.instance}
\usage{make.subsample.instance(size, split, iters)}
\description{\code{make.subsample.instance} generates a \code{\linkS4class{subsample.instance}} object, which encapsulates the generated indices of training and test sets.}
\value{A \code{\linkS4class{subsample.instance}} object, which encapsulates the generated indices of training and test sets.}
\seealso{\code{\link{resample.fit}}, \code{\linkS4class{subsample.instance}}}
\arguments{\item{size}{[integer] \cr Size of the data set to resample.}
\item{split}{[numeric] \cr Proportion of data used for training set.}
\item{iters}{[integer] \cr Number of generated subsets / resampling iterations.}
}
\examples{data(iris)
# split is the training set percentage
rin <- make.subsample.instance(size=nrow(iris), split=2/3, iters=10)
# holdout
rin <- make.subsample.instance(size=nrow(iris), split=2/3, iters=1)}
