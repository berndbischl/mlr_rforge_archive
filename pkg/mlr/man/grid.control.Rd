\name{grid.control}
\alias{grid.control}
\title{Control for grid search tuning}
\usage{grid.control(ranges)
}
\description{Control structure for grid search tuning.}
\value{Control structure for tuning.}
\arguments{\item{ranges}{[\code{\link{list}}] \cr 
A list of named vectors/lists of possible values for each hyperparameter. 
You can also pass a list of such ranges by using [\code{\link{combine.ranges}}] 
in the rare case when it does not make sense to search a complete cross-product of range values.}
}
