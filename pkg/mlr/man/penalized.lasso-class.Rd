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
\section{Methods}{\describe{\item{\code{\link[=initialize,penalized.lasso-method]{initialize}}}{\code{signature( = "penalized.lasso")}: Constructor.}\item{\code{\link[=train.learner,penalized.lasso,formula,data.frame,numeric,list-method]{train.learner}}}{}}}

