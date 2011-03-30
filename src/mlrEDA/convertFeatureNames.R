#' Removes specials characters in column names.
#'
#' Currently the following 
#' c("\\[", "]", "\\(", ")", "\\{", "}", ",", "\\+", "-", "\\*", "/", "=", "\\$", "~")
#' 
#' @param data [\code{data.frame}]\cr 
#'   Data to convert.
#' @param replace [named \code{character}]
#'   For default see details.  
#' @return [\code{data.frame}]
#' 
#' @export
#' @title Summarize factors of a data.frame.

convertColumnNames = function(data, start.date) {
  
  #todo: these are all candidates for bad chars
  #! " # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~
  bad.chars = c("\\[", "]", "\\(", ")", "\\{", "}", ",", "\\+", "-", "\\*", "/", "=", "\\$", "~")
  bcs.collapsed = paste(sapply(bad.chars, function(x) substr(x, nchar(x), nchar(x))), collapse=" ")
 
  for (bc in bad.chars) {
    # take last int code when escaping regexp
    cns = gsub(pattern=bc, replacement=rev(utf8ToInt(bc))[1], cns)
  }
  return(data)
}
  
·