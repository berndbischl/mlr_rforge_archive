#' @include object.r
roxygen()

#' Description object for learner.
#' 
#' Getter.\cr
#' 
#' \describe{
#'  \item{doubles [boolean]}{Can real-valued inputs be processed?}
#'  \item{factors [boolean]}{Can factor inputs be processed?}
#'  \item{missings [boolean]}{Can missing values be processed?}
#'  \item{weights [boolean]}{Can case weights be used?}
#'  \item{multiclass [boolean]}{Can probabilities be predicted?}
#'  \item{costs [boolean]}{Can misclassification costs be directly used during training?}
#'  \item{probs [boolean]}{Can probabilities be predicted?}
#'  \item{decision [boolean]}{Can probabilities be predicted?}
#' }
#' @exportClass learner.desc
#' @title Description object for learner. 





