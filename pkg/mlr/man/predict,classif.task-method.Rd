\name{predict,classif.task-method}
\alias{predict,classif.task-method}

\title{predict}
\usage{\S4method{predict}{classif.task}(object, model, newdata, type="default")}
\description{Predicts the target classes of a new data set based on 
an already fitted wrapped.model of a classifcation task.}
\details{See documentation super method.}
\value{Either a factor of predicted classes or a matrix of probabilities. The rows correspond to the 
predicted observations and the columns to the classes. Each column has the name of its respective class.}
\seealso{\code{\link{predict,learn.task-method}}}
\arguments{\item{object}{[\code{\linkS4class{learn.task}}] \cr 
Learning task}
\item{model}{[\code{\linkS4class{wrapped.model}}] \cr 
Wrapped model, trained from learn task}
\item{newdata}{[\code{\link{data.frame}}] \cr 
Contains new observations which should be predicted (by default the train data of the wrapped model).}
\item{type}{[\code{\link{character}}] \cr 
Specifies the type of predictions -	either probability ("prob") or class ("class"). 
Default is "default", which uses the type specified in the classification task.}
}
\examples{inds <- 2*(1:75)
test <- iris[-inds,]

ct <- make.classif.task("lda", data=iris, formula=Species~.)
model <- train(ct, subset=inds)
predict(ct, model, newdata = test)}
