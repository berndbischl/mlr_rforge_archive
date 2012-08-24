

ensemble.selector = function(learners, ensemble.select){
  if(ensemble.select=="random") {
    sel.learner = sample(length(learner),1)
  }
  return(sel.learner)
}



#list(learner, sel.learner=sel.learner)