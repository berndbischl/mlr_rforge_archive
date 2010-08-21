\name{par.desc-class}
\alias{par.desc-class}
\alias{par.desc}
\title{Description class for a hyperparameter.}
\description{Description class for a hyperparameter.}
\details{Getter.\cr

\describe{
\item{par.name [string]}{Name of parameter in learning algorithm.}
\item{default [any]}{Default value.}
\item{data.type [string]}{'numeric', 'factor', 'integer', 'unknown'.}
\item{when [string]}{Specifies when a cetrain hyperparameter is used. Possible entries are 'train', 'predict' or 'both'.}
\item{requires [list]}{Requirements for a parameter to be effective.}
}}
\section{Methods}{\describe{}}
