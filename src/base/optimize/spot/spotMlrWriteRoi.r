

spotMlrWriteRoi = function(path, control) {
  pds = control@par.descs
	n = length(pds)
  tab = data.frame(name=rep("a", n), low=0, high=1, type="x", stringsAsFactors = FALSE)
	for (i in 1:n) {
		pd = pds[[i]]
    if (!is(pd, "par.desc.double"))
      stop("wraong type!")
		type = switch(pd["data.type"], numeric="FLOAT", integer="INT")
		#lower = ifelse(is(pd, "par.desc.double"), pd["lower"], 1)
		#upper = ifelse(is(pd, "par.desc.double"), pd["upper"], length(pd["vals"]))
    lower = pd["lower"]
    upper = pd["upper"]
    tab[i, "name"] = pd["par.name"]
		tab[i, "low"] = lower
		tab[i, "high"] = upper
		tab[i, "type"] = type
	}
  print(str(tab))
	f = file.path(path, "spotMlr.roi")
	write.table(tab, file=f, row.names=FALSE)
}


