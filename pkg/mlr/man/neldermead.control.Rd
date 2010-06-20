\name{neldermead.control}
\alias{neldermead.control}
\title{Control for Nelder-Mead tuning.}
\usage{neldermead.control(start, minimize=T, scale=identity, ...)
}
\description{Control structure for Nelder-Mead tuning.}
\details{See \code{\link[stats]{optim}} for details of Nelder-Mead settings.}
\value{Control structure for tuning.}
\arguments{\item{start}{[numeric] \cr
Named vector of initial values.}
\item{minimize}{[logical] \cr
Minimize performance measure? Default is TRUE.}
\item{scale}{[\code{\link{function}}] \cr
A function to scale the hyperparameters. E.g. maybe you want to optimize in some log-space.
Has to take a single, numerical vector and return a scaled one. Default is identity function.}
\item{...}{Passed down to optim.}
}
