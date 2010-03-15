setClass(
		"prediction",
		representation = representation(
				response = "ANY",
				trues = "ANY",
				weights = "numeric",
				prob = "matrix",
				decision = "matrix"
		)
)
