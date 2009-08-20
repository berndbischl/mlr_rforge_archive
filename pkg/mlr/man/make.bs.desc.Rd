\name{make.bs.desc}
\alias{make.bs.desc}
\title{make.bs.desc}
\usage{make.bs.desc(size, iters)
}
\description{Generates a description object for a bootstrap. Usually only needed in \code{\link{benchmark}} 
to describe the inner resampling.}
\value{A \code{\linkS4class{bs.desc}} object.}
\seealso{\code{\linkS4class{bs.desc}}, \code{\link{benchmark}}}
\arguments{\item{size}{[\code{\link{integer}}] \cr 
Size of the data set to resample.}
\item{iters}{[\code{\link{integer}}] \cr 
Number of generated subsets / resampling iterations.}
}

