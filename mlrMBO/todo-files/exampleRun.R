#   ctrl = makeMBOControl(noisy = FALSE, n.init.design.points = 4, iters = iters, 
#                         infill.crit = "ei", infill.opt = "design", seq.design.points = 1000, 
#                         save.model.at = 0:iters)

exampleRun = function(fun, lower, upper, name.x = "x", name.y = "y",
  surrogate, control)) {
  
  checkArg(fun, "function")
  checkArg(name.x, "character", len = 1L, na.ok = FALSE)
  checkArg(name.y, "character", len = 1L, na.ok = FALSE)
  checkArg(lower, "numeric", len = 1L, na.ok = FALSE)
  checkArg(upper, "numeric", len = 1L, na.ok = FALSE)
  
  if (missing(surrogate)) 
    surrogate = makeLearner("regr.km", predict.type = "se", nugget.estim = TRUE)
   
  par.set = makeParamSet(makeNumericParam(name.x, lower = lower, upper = upper))
  iters = 5
  
  #res = mbo(objfun, par.set, learner = surrogate, control = ctrl)
  df = as.data.frame(res$opt.path)
  
  #FIXME move to mlrmbo, nice plot function in general
  xseq = seq(sigma.range[1], sigma.range[2], length.out = 15)
  # yseq = sapply(xseq, function(sigma) {
  #   print(sigma)
  #   mean(replicate(5, objfun(list(sigma=sigma))))
  # })
  list(xseq = xseq, yseq=yseq)
}


plot.ExampleRun = function(x) {
  for (i in 1:iters) {
    #mar = mar = c(4, 4, 2, 4)
    #par(mfrow=c(2, 1), mar = mar)
    par(mfrow=c(2, 1))
    
    xpoints = data.frame(sigma = xseq)
    mod = res$models[[i]]
    yhat = infillCritMeanResponse(xpoints, mod, ctrl, par.set, rbind(initdes, seqdes))
    se = infillCritStandardError(xpoints, mod, ctrl, par.set, rbind(initdes, seqdes))
    ei = -infillCritEI(xpoints, mod, ctrl, par.set, rbind(initdes, seqdes))
    proposed.x = xseq[which.max(ei)]
    initdes = subset(df, dob == 0)
    seqdes = subset(df, dob > 0 & dob < i)
    propdes = subset(df, dob == i)
    yhat.low1 = yhat - 1 * se 
    yhat.upp1 = yhat + 1 * se 
    
    plot(c(), xlim = range(xseq), ylim = range(df$y), 
         xlab = "sigma", ylab = "mse", main = sprintf("Iter = %i", i))
    
    dr1 = seq(min(yhat - 1.5 * se), max(yhat + 1.5 * se), length = 200)
    dr2 = matrix(nrow = length(xseq), ncol = length(dr1))
    for(i in seq_along(xseq)) dr2[i,] <- dnorm(dr1, yhat[i], se[i])
    densregion(xseq, dr1, dr2, pointwise = TRUE, colmax = "pink", )
    lines(xseq, yhat.low1, lty = "dotted", lwd = 2, col = rgb(0, 0, 0, alpha = 0.5))
    lines(xseq, yhat.upp2, lty = "dotted", lwd = 2, col = rgb(0, 0, 0, alpha = 0.5))
    #col1 = rgb(0.7, 0, 0.3, alpha=0.2)
    #col2 = rgb(0.7, 0, 0.3, alpha=0.1)
    #polygon(c(xseq, rev(xseq)), c(yhat.low1, rev(yhat.upp1)), col = col, border = FALSE)  
    #polygon(c(xseq, rev(xseq)), c(yhat.low2, rev(yhat.upp2)), col = col, border = FALSE)  
    lines(xseq, yseq, lwd = 3)
    points(initdes$sigma, initdes$y, pch = 19, col = "black")
    points(seqdes$sigma, seqdes$y, pch = 19, col = "green")
    points(propdes$sigma, propdes$y, pch = 19, col = "red")
    lines(xseq, yhat, lty = "dotted", lwd = 3)
    abline(v = proposed.x)
    #p = predict(mod, newdata = )
    #lines(xseq, p$data$response)
    
    #par(new = TRUE, mar = mar)
    #plot(xseq, ei, type = "l", lty = "dashed", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
    # legend(x = "topright", legend = c("y", "y_hat", "ei"),  lty = c("solid", "dotted", "dashed"))
    #axis(side=4); mtext(side = 4, line = 3, "EI")
    
    plot(xseq, ei, type = "l", lty = "dashed", xlab = "sigma", ylab = "EI", lwd = 3)
    #FIXME show design points
    abline(v = proposed.x)
    legend(x = "topright", legend = c("y", "y_hat", "ei"),  lty = c("solid", "dotted", "dashed"))
  }
}
