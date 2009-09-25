\name{penalized.lasso-class}
\alias{penalized.lasso-class}
\alias{penalized.lasso}
\title{penalized.lasso}
\description{Wrapped learner for Lasso Regression from package \code{penalized} for regression problems.}
\details{\emph{Common hyperparameters:}
\describe{
\item{\code{lambda1}}{Tuning parameter for L1 penalization.}			
\item{\code{steps}}{If greater than 1, the algorithm will fit the model for a range of steps lambda1-values, starting from the maximal value down to the value of lambda1 specified.}
\item{\code{epsilon}}{The convergence criterion.}
}}
\seealso{\code{\link[penalized]{penalized}}}
\section{Extends}{\code{\linkS4class{wrapped.learner.regr}}}
\section{Methods}{\describe{{\code{signature(wrapped.learner = "penalized.lasso", formula = "formula", data = "data.frame", weights = "numeric", parset = "list")}: Overwritten, to allow "lambda" instead of "lambda1" as parameter name.
Besides that, simply delegates to super method.}}}
