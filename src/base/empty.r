# internal function to create empty S4 object deriving from object
make.empty = function(x) {
  attr(x, "empty") = TRUE
  return(x)
}

# internal function to check if an object is empty, like a NULL object 
is.empty = function(x) {
  !is.null(attr(x, "empty"))
}

