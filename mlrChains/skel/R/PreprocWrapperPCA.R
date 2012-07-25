#FIXME: read this

#' @export
makePreprocWrapperPCA = function(learner) {
  checkArg(learner, "Learner")

  trainfun = function(data, target, args) {
    print("train: PCA")
    cns = colnames(data)
    nums = cns[sapply(data, is.numeric)]
    x = data[, nums]
    pca = prcomp(x, scale=TRUE)
    data2 = data[, setdiff(cns, nums), drop=FALSE]
    data2 = cbind(data2, as.data.frame(pca$x))
    ctrl = list(center=pca$center, scale=pca$scale, rotation=pca$rotation)
    list(data=data2, control=ctrl)
  }

  predictfun = function(data, target, args, control) {
    print("predict: PCA")
    cns = colnames(data)
    nums = cns[sapply(data, is.numeric)]
    x = as.matrix(data[, nums, drop=FALSE])
    x = scale(x, center=control$center, scale=control$scale)
    x = x %*% control$rotation
    data2 = data[, setdiff(cns, nums), drop=FALSE]
    data2 = cbind(data2, as.data.frame(x))
    data2
  }
  makePreprocWrapper(learner, trainfun, predictfun)
}  


