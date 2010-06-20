\name{learner-class}
\alias{learner-class}
\alias{learner}
\title{Base class for inducers.}
\description{Abstract base class for learning algorithms.}
\details{\code{learner-class}: Getter.\cr

\describe{
\item{is.classif [boolean]}{Is this learner for classification tasks?}
\item{is.regr [boolean]}{Is this learner for regression tasks?}
\item{id [string]}{Id string of learner.}
\item{label [string]}{Label string of learner.}
\item{pack [string]}{Package were underlying learner is implemented.}
\item{props [{\linkS4class{learner.props}}]}{Properties object to describe functionality of the learner.}
\item{hyper.pars [list]}{List of fixed hyperparameters and respective values for this learner.}
\item{hyper.names [character]}{Names of currently used hyperparameters.}
\item{hyper.types [character]}{For which step in the model building process are the respective hyperparameters used? Named character vector.}
\item{supports.numerics [boolean]}{Can numeric inputs be processed?}
\item{supports.factors [boolean]}{Can factor inputs be processed?}
\item{supports.characters [boolean]}{Can character inputs be processed?}
\item{supports.missings [boolean]}{Can missing values be processed?}
\item{supports.multiclass [boolean]}{Can probabilities be predicted?}
\item{supports.costs [boolean]}{Can misclassification costs be directly used during training?}
\item{supports.probs [boolean]}{Can probabilities be predicted?}
\item{supports.decision [boolean]}{Can probabilities be predicted?}
\item{supports.weights [boolean]}{Can case weights be used?}
}

}
\section{Extends}{\code{\linkS4class{object}}}
\section{Methods}{\describe{\item{\code{\link[=[,learner-method]{[}}}{\code{signature( = "learner")}: Getter.}\item{\code{\link[=to.string,learner-method]{to.string}}}{}\item{\code{\link[=resample.fit,learner,learn.task,resample.instance,list,character,function-method]{resample.fit}}}{}\item{\code{\link[=train,learner,learn.task,numeric,list,character,character-method]{train}}}{}}}
\alias{[,learner-method}

