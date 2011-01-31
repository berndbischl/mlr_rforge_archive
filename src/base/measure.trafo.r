#todo
# props durcvhreichen


# Evaluates measure only on training set

inbag = function(pm) {
  id = paste("inbag", pm@id, sep=".")
  make.measure(id=id, minimize=pm["minimize"], pars=list(pm@fun), 
    fun=function(pred.test, pred.train, model, task, pars) {
      pars[[1]](pred.test=pred.train)
    }
  )
}

# Evaluates measure only on test set. oob simply returns its argument. 

oob = function(pm) {
  pm  
}

# B632: Evaluates measure on training and test set and combines with the
# Should probably only be used with bootstrap resampling. 

b632 = function(pm) {
  id = paste("b632", pm@id, sep=".")
  make.measure(id=id, minimize=pm["minimize"], pars=list(pm@fun),
    fun=function(pred.test, pred.train, model, task, pars) {
      w = 0.632
      ptest = pars[[1]](pred.test=pred.test)
      ptrain = pars[[1]](pred.test=pred.train)
      w*ptest + (1-w)*ptrain
    }
  )
}

# B632+: Evaluates measure on training and test set and combines with the
# Should probably only be used with bootstrap resampling. 

b632plus = function(pm) {
  id = paste("b632", pm@id, sep=".")
  make.measure(id=id, minimize=pm["minimize"], pars=list(pm@fun), 
    fun=function(pred.test, pred.train, model, task, pars) {
      w = 0.632
      ptest = pars[[1]](pred.test=pred.test)
      ptrain = pars[[1]](pred.test=pred.train)
      w*ptest + (1-w)*ptrain
    }
  )
}
  
