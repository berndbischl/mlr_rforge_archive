test.helpers = function() {
	
	xs = list("a", "b")
	check.list.type(xs, "character")
	
	xs = list("a", "b", 1, 2)
	check.list.type(xs, c("character", "numeric"))
	
}