setClass(
		"prediction",
		representation = representation(
				data.desc = "data.desc",
				task.desc = "task.desc",
				id = "integer",
				response = "ANY",
				target = "ANY",
				weights = "numeric",
				prob = "ANY",
				decision = "ANY"
		)
)


setMethod(
		f = "[",
		signature = signature("prediction"),
		def = function(x,i,j,...,drop) {
			#if nothing special return slot
			return(
					eval(substitute("@"(x, slot), list(slot=i)))
			)
		}
)


setMethod(
		f = "as.data.frame",
		signature = signature("prediction"),
		def = function(x, row.names = NULL, optional = FALSE,...) {
			ii = x["id"]
			rr = x["response"]
			tt = x["target"]
			pp = x["prob"]
			dd = x["decision"]
			mm = data.frame(id=ii, response=rr)
			if (length(tt) > 1 || !is.na(tt))
				mm = cbind(mm, target=tt)
			if (length(pp) > 1 || !is.na(pp))
				mm = cbind(mm, prob=pp)
			if (length(dd) > 1 || !is.na(dd))
				mm = cbind(mm, decision=dd)
			return(mm)
		}
)


