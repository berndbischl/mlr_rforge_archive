#' Description object for learner, encapsulates basic capabilities.
#' 
#' @slot id Id of learner, used in printing and list indexing.
#' @slot type Can be 'classif' or 'regr'.
#' @slot feat Which types of features can be used? Has 2 named elements 'numerics' and 'factors'.
#' @slot weights Length 1. Can case weights be used? 
#' @slot missings Length 1. Can missing values be handled? na.omit does not count. 
#' @slot classes Which types of classes can be handled? 
#'   Has 3 named elements 'oneclass', 'twoclass' and 'multiclass'.
#'   For regression this is \code{c(oneclass=FALSE, twoclass=FALSE, multiclass=FALSE)}.  
#' @slot predict Which types of predictions (other than 'response') can be made? 
#'   Has 2 named elements 'prob' and 'decision'.  
#'   For regression this is \code{c(prob=FALSE, twoclass=FALSE, decision=FALSE)}.  
#' @slot costs Length 1. Can misclassification costs be handled? 
#'   For regression this is \code{FALSE}. 
#' 
#' @exportClass LearnerDesc
#' @seealso \code{\linkS4class{Learner}}
#' @title Description object for Learner. 

setClass(
  "LearnerDesc",
  representation = representation(
    id = "character",
    type = "character",
    feat = "logical",
    weights = "logical",
    missings = "logical",
    classes = "logical",
    predict = "logical",
    costs = "logical" 
  )
)



