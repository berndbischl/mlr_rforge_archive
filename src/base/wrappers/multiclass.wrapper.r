#' @include base.wrapper.r
roxygen()


#' Wrapper class for learners to handle multi-class problems. 
#' 
#' @exportClass multiclass.wrapper
#' @title Wrapper class for learners to handle multi-class problems.
setClass(
        "multiclass.wrapper",   
        contains = c("base.wrapper"),
        representation = representation(
          cm.fun = "function"
        )       
)

#' Constructor.

setMethod(
  f = "initialize",
  signature = signature("multiclass.wrapper"),
  def = function(.Object, learner) {
    pds = list(
      new("par.desc.disc", par.name="method", vals=c("onevsone", "onevsrest"), default="onevsrest")
    )
    callNextMethod(.Object, learner, par.descs=pds, par.vals=list())
  }
)

#' @rdname multiclass.wrapper-class

setMethod(
        f = "[",
        signature = signature("multiclass.wrapper"),
        def = function(x,i,j,...,drop) {
            if (i == "multiclass")
                return(TRUE)
            if (i == "prob")
                return(FALSE)
            if (i == "decision")
                return(FALSE)
            if (i == "codematrix")
                return(x@codematrix)
            callNextMethod()
        }
)

#' Fuses a base learner with a multi-class method. Creates a learner object, which can be
#' used like any other learner object. This way learners which can only handle binary classification 
#' will be able to handle multi-class problems too.
#'
#' @param learner [\code{\linkS4class{learner}} or string]\cr 
#'   Learning algorithm. See \code{\link{learners}}.  
#' @param method [string] \cr
#'   "onevsone" or "onevsrest". Default is "onevsrest".
#' 
#' @return \code{\linkS4class{learner}}.
#' 
#' @title Fuse learner with multiclass method.
#' @export

# param codematrix [matrix] \cr
#  ECOC codematrix with entries +1,-1,0. Columns define new binary problems, rows correspond to classes.

make.multiclass.wrapper = function(learner, method="onevsrest") {
  if (is.character(learner))
    learner = make.learner(learner)
  w = new("multiclass.wrapper", learner=learner)
  set.hyper.pars(w, method=method)
}


#' @rdname train.learner

setMethod(
  f = "train.learner",
  signature = signature(
    .learner="multiclass.wrapper", 
    .task="classif.task", .subset="integer" 
  ),
  
  def = function(.learner, .task, .subset,  ...) {
    .task = subset(.task, .subset)
    tn = .task["target"]
    d = .task["data"]
    y = .task["targets"]
    args = list(...)
    # remove hyperpar of wrapper
    args$method = NULL
    myargs = .learner["par.vals", par.top.wrapper.only=TRUE]
    
    # build codematrix
    cmfun = switch(myargs$method,
      onevsrest = cm.onevsrest,
      onevsone = cm.onevsone
    )
    cm = cmfun(.task["desc"])
    x = multi.to.binary(y, cm)
    
    # now fit models
    k = length(x$row.inds) 
    levs = .task["class.levels"]
    models = list()
    base = set.hyper.pars(.learner["learner"], par.vals=args)
    for (i in 1:k) {
      data2 = d[x$row.inds[[i]], ]
      data2[, tn] = x$targets[[i]] 
      ct = change.data(.task, data2)
      m = train(base, task=ct)
      models[[i]] = m 
    }
    # store cm as last el.
    models[[i+1]] = cm 
    return(models)
  }
)

#' @rdname pred.learner

setMethod(
  f = "pred.learner",
  signature = signature(
    .learner = "multiclass.wrapper", 
    .model = "wrapped.model", 
    .newdata = "data.frame", 
    .type = "character" 
  ),
  
  def = function(.learner, .model, .newdata, .type, ...) {
    models = .model["learner.model"]
    cm = models[[length(models)]]
    k = length(models)-1
    p = matrix(0, nrow(.newdata), ncol=k)
    # we use hamming decoding here
    for (i in 1:k) {
      m = models[[i]]
      p[,i] = as.integer(as.character(predict(m, newdata=.newdata, ...)["response"]))
    }
    rns = rownames(cm)
    y = apply(p, 1, function(v) {
        # todo: break ties
        #j = which.min(apply(cm, 1, function(z) sum(abs(z - v))))
        d <- apply(cm, 1, function(z) sum(abs(z - v)))
        j <- which(d == min(d))
        j <- sample(rep(j,2), size = 1)
        rns[j]
      })
    as.factor(y)
  }
)   




# Function for Multi to Binary Problem Conversion
multi.to.binary = function(target, codematrix){
    
    if (any(is.na(codematrix)) ) {
        stop("Code matrix contains missing values!")
    }
    levs <- levels(target)
    no.class <- length(levs)
    rns = rownames(codematrix)
    if (is.null(rns) || !setequal(rns, levs)) {
        stop("Rownames of code matrix have to be the class levels!")
    }
    
    binary.targets = as.data.frame(codematrix[target,])
    row.inds = lapply(binary.targets, function(v) which(v != 0))
    names(row.inds) = NULL
    targets = Map(function(y, i) factor(y[i]),
            binary.targets, row.inds)
    
    return(list(row.inds=row.inds, targets=targets))
}

cm.onevsrest = function(task.desc) {
    n = task.desc["class.nr"]
    cm = matrix(-1, n, n)
    diag(cm) = 1
    rownames(cm) = task.desc["class.levels"]
    return(cm)
} 

cm.onevsone = function(task.desc) {
    n = task.desc["class.nr"]
    cm = matrix(0, n, choose(n, 2))
    combs = combn(n, 2)
    for (i in 1:ncol(combs)) {
        j = combs[,i]
        cm[j, i] = c(1, -1) 
    }
    rownames(cm) = task.desc["class.levels"]
    return(cm)
} 

