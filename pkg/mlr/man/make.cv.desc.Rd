\name{make.cv.desc}
\alias{make.cv.desc}
\title{make.cv.desc}
\usage{make.cv.desc(size, iters)
}
\description{Generates a description object for a cross-validation. Usually only needed in \code{\link{benchmark}} 
to describe the inner resampling - e.g. for a double cross-validation.}
\value{A \code{\linkS4class{cv.desc}} object.}
\seealso{\code{\linkS4class{cv.desc}}, \code{\link{benchmark}}}
\arguments{\item{size}{[\code{\link{integer}}] \cr 
Size of the data set to resample.}
\item{iters}{[\code{\link{integer}}] \cr 
Number of generated subsets / resampling iterations.}
}
