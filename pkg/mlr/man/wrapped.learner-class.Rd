\name{wrapped.learner-class}
\alias{wrapped.learner-class}
\alias{wrapped.learner}
\title{Wrapped...}
\description{Wrapped.learner ---text!----}
\section{Slots}{\describe{\item{\code{learner.name}:}{(\code{\link{character}}) Name of the learning method}\item{\code{learner.pack}:}{(\code{\link{character}}) R package where learner is defined}\item{\code{train.fct}:}{(\code{\link{function}}) Function used in above package to train a regular model in the package (see \code{\link{train}})}\item{\code{train.fct.pars}:}{(\code{\link{list}}) Named list of parameters which are fixed in the above train.fct. NB: These are _NOT_ 
hyperparamters of the classifier but rather parameters which are fixed for the whole 
experiment and should be fixed at the beginning for convenience (Example would be the 
tolerance parameter in lda.)}\item{\code{predict.fct}:}{(\code{\link{function}}) Function used in above package to predict new data with a trained model 
(see \code{\link{predict}})}\item{\code{predict.newdata.arg}:}{(\code{\link{character}}) Name of argument for the new data frame in the underlying predict method.}\item{\code{predict.fct.pars}:}{(\code{\link{list}}) Named list of parameters which are fixed in the above predict.fct. See train.fct.pars 
(example would be the method parameter in predict.lda.)}\item{\code{learner.props}:}{(\code{\link{learner.props}}) Properties of the learner}}}
\section{Methods}{\describe{\item{\code{\link[=train.learner,wrapped.learner,formula,data.frame,numeric,list-method]{train.learner}}}{}\item{\code{\link[=initialize,wrapped.learner-method]{initialize}}}{\code{signature( = "wrapped.learner")}: Constructor.}\item{\code{\link[=as.character,wrapped.learner-method]{as.character}}}{\code{signature( = "wrapped.learner")}: Conversion to string.}\item{\code{\link[=print,wrapped.learner-method]{print}}}{\code{signature( = "wrapped.learner")}: Prints the object by calling as.character.}\item{\code{\link[=show,wrapped.learner-method]{show}}}{\code{signature( = "wrapped.learner")}: Shows the object by calling as.character.}\item{\code{\link[=set.train.par,wrapped.learner-method]{set.train.par}}}{\code{signature( = "wrapped.learner")}: Set a parameter for the underlying train function of a wrapped learner. 
This is not meant for hyperparamters, pass these through the usual parset argument, but rather to
fix (somewhat techical) arguments which stay the same for the whole experiment. You should not have to use this too often.}\item{\code{\link[=set.predict.par,wrapped.learner-method]{set.predict.par}}}{\code{signature( = "wrapped.learner")}: Set a parameter for the underlying predict function of a wrapped learner. 
Used to fix (somewhat techical) arguments which stay the same for the whole experiment. Y
You should not have to use this too often.}}}

