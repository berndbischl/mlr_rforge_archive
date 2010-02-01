\name{ps.control}
\alias{ps.control}
\title{Control for pattern search tuning}
\usage{ps.control(start, lower=rep(-Inf, length(start)), upper=rep(Inf, length(start)),
    delta=1, delta.min=10^(-3), maxit=100)
}
\description{Control structure for pattern search tuning.}
\value{Control structure for tuning.}
\arguments{\item{start}{[\code{\link{numeric}}] \cr
Named vector for initial values.}
\item{lower}{[\code{\link{numeric}}] \cr
Named vector for lower boundary constraints. Default is -Inf.}
\item{upper}{[\code{\link{numeric}}] \cr
Named vector for upper boundary constraints. Default is -Inf.}
\item{delta}{[\code{\link{numeric}}] \cr
Initial step size.}
\item{delta.min}{[\code{\link{numeric}}] \cr
Minimal step size. Default is 10^-3.}
\item{maxit}{[\code{\link{integer}}] \cr
Maximal number of iterations. Default is 100}
}
