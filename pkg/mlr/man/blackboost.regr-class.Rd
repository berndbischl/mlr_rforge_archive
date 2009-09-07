\name{blackboost.regr-class}
\alias{blackboost.regr-class}
\alias{blackboost.regr}
\title{blackboost.regr}
\description{Wrapped learner for Boosting with Regression Trees from package \code{mboost} for regression problems.}
\details{\emph{Common hyperparameters:}
\describe{
\item{\code{mstop}}{Integer, giving the number of initial boosting iterations.}			
\item{\code{nu}}{Double (between 0 and 1), defining the step size or shrinkage parameter.}
\item{\code{constraint}}{Logical, indicating whether the working responses should be restricted to (-1, +1).}
\item{\code{risk}}{Character, indicating how the empirical risk should be computed for each boosting iteration.}			
\item{\code{center}}{Logical, indicating if the numerical covariates should be mean centered before fitting.}
}}
\seealso{\code{\link[mboost]{blackboost}}, \code{\link[mboost]{boost_control}}}
\section{Extends}{\code{\linkS4class{wrapped.learner.regr}}}
\section{Methods}{\describe{\item{\code{\link[=initialize,blackboost.regr-method]{initialize}}}{\code{signature( = "blackboost.regr")}: Constructor.}}}

