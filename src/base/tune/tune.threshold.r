
remove.threshold = function(control) {
	if (is(control, "grid.control")) {
		control$ranges$predict.threshold = NULL
	} else {
		control$start$predict.threshold = NULL
		control$upper$predict.threshold = NULL
		control$lower$predict.threshold = NULL
	}
}


tune.threshold = function(pred, ranges, upper, lower) {
	f = function(x) {
			
	}
	
}
