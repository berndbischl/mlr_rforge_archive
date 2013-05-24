# load extra packages for some options / user choices
loadPackages = function(control) {
  if (control$infill.opt == "CMAES") 
    requirePackages("cmaes", "proposePoints")
  #if (control$infill.opt == "EI")
  #  requirePackages("DiceOptim")
}    