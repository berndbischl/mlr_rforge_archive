evalDesign = function(des, fun) {
  sapply(1:nrow(des), function(i) fun(as.list(des[i,])))
} 