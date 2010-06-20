\name{cmaes.control}
\alias{cmaes.control}
\title{Control for CMA-ES tuning.}
\usage{cmaes.control(start, minimize=T, scale=identity, ...)
}
\description{Control structure for CMA-ES tuning.}
\details{See \code{\link[cmaes]{cma_es}} for details of CMA-ES settings.}
\value{Control structure for tuning.}
\arguments{\item{start}{[numeric] \cr
Named vector of initial values.}
\item{minimize}{[logical] \cr
Minimize performance measure? Default is TRUE.}
\item{scale}{[\code{\link{function}}] \cr
A function to scale the hyperparameters. E.g. maybe you want to optimize in some log-space.
Has to take a single, numerical vector and return a scaled one. Default is identity function.}
\item{...}{Passed down to cmaes.}
}
