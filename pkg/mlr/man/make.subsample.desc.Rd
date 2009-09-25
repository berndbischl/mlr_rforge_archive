\name{make.subsample.desc}
\alias{make.subsample.desc}
\title{make.subsample.desc}
\usage{make.subsample.desc(size, split=2/3, iters)
}
\description{Generates a description object for subsampling. Usually only needed in \code{\link{benchmark}} 
to describe the inner resampling.}
\value{A \code{\linkS4class{subsample.desc}} object.}
\seealso{\code{\linkS4class{subsample.desc}}, \code{\link{benchmark}}}
\arguments{\item{size}{[\code{\link{integer}}] \cr
Size of the data set to resample.}
\item{split}{[\code{\link{numeric}}] \cr 
Proportion of data used for training set. Default is 2/3.}
\item{iters}{[\code{\link{integer}}] \cr 
Number of generated subsets / resampling iterations.}
}
