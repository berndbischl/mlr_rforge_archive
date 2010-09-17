choose.new.points = function(n, meta.model, constr.model, curdes, cury, control) {
  #opt.meta.model.seq.des(n, meta.model, constr.model, par.descs, control)
  #opt.meta.model.bfgs(n, meta.model, constr.model, par.descs, curdes, cury, control) 
  
  if (control["seq.method"] == "DiceOptim.CL") {
    opt.meta.model.CL(n, meta.model, constr.model, curdes, cury, control) 
  }  
}