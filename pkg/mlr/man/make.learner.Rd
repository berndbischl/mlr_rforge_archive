\name{make.learner}
\alias{make.learner}
\title{Create learner object.}
\usage{make.learner(class, id, label, predict.type="response",
    predict.threshold=numeric(0), hyper.types="train", ...)
}
\description{Create learner object.}
\value{\code{\linkS4class{learner}}.}
\arguments{\item{class}{[string] \cr
Class of learner to create.}
\item{id}{[string]\cr 
Id string for object. Used to select the object from a named list, etc.}
\item{label}{[string]\cr 
Label string for object. Used in plots, etc.}
\item{predict.type}{[string] \cr
Classification: "response" | "prob" | "decision", specifying the type to predict.
Default is "response". "decision" is experimental.
Ignored for regression.}
\item{hyper.types}{[character] \cr
Character vector. If named, names have to correspond to the hyper pars in the ... arguments. If a single string is passed it is used as the type for all hyper parameters.
Specifies in which stage of the model fitting the hyper parameters are used. Currently supported at user level are
"train" and "predict". Default is "train".}
\item{predict.threshold}{[numeric] \cr
Threshold to produce class labels if type is not "response". 
Currently only supported for binary classification and type="prob", where it represents the required predicted probability
for the positive class, so that a positive class is predicted as "response".
Default is 0.5 for type="prob".
Ignored for regression.}
\item{...}{[any] \cr
Optional named (hyper)parameters.}
}
