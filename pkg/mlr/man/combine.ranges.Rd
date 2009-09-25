\name{combine.ranges}
\alias{combine.ranges}
\title{combine.ranges}
\usage{combine.ranges(...)
}
\description{Convenience function for \code{\link{tune}} to combine different \code{ranges} lists (i.e. lists of named vectors/lists 
of possible hyperparameter values).}
\value{A list of \code{ranges}}
\seealso{\code{\link{tune}}}
\arguments{\item{...}{Lists of ranges.}
}
\examples{library(mlbench)
data(Sonar)
cv.i <- make.cv.instance(size = nrow(Sonar), iters = 2)
ct <- make.classif.task("kernlab.svm.classif", data = Sonar, formula = Class~.)
r1 <- list(kernel = "polydot", C = c(1,2), degree = c(2,3))
r2 <- list(kernel = "rbfdot", C = c(1,2), sigma = c(4,6))
tr <- tune(ct, cv.i, ranges = combine.ranges(r1,r2))}
