\name{resample.desc-class}
\alias{resample.desc-class}
\alias{resample.desc}
\title{resample.desc}
\description{Base class for description of resampling algorithms.
A description of a resampling algorithm contains all necessary information to provide a resampling.instance, 
when given the size of the data set.}
\section{Slots}{\describe{\item{\code{instance.class}:}{(\code{\link{character}}) S4 class name of the corresponding resample.instance}\item{\code{name}:}{(\code{\link{character}}) Name of this resampling algorithm}\item{\code{iters}:}{(\code{\link{numeric}}) Number of iterations}}}
\section{Methods}{\describe{\item{\code{\link[=[,resample.desc-method]{[}}}{\code{signature( = "resample.desc")}: Getter.}
