

spotMlrWriteRoi = function(path, par.descs) {
	n = length(par.descs)
	for (i in 1:n) {
		pd = par.descs[[i]]
		type = ifelse(is(pd, "par.desc.num"), "DOUBLE", "INT")
		lower = ifelse(is(pd, "par.desc.num"), pd["lower"], 1)
		upper = ifelse(is(pd, "par.desc.num"), pd["upper"], length(pd["vals"]))
		tab[i, "name"] = pd["name"]
		tab[i, "low"] = lower
		tab[i, "high"] = upper
		tab[i, "type"] = type
	}
	f = file.path(path, "mlr.conf")
	write.table(tab, file=f)
}


