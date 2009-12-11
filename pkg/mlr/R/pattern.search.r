#' @export
pattern.control <- function(
		start,
		lower=rep(-Inf, length(start)),	
		upper=rep(Inf, length(start)), 
		delta=1, 
		delta.min=10^(-3), 
		maxit=100) {
	list(start=unlist(start), lower=unlist(lower), upper=unlist(upper), delta=delta, delta.min=delta.min, maxit=maxit)	
}

# f: function to be optimized (depending on "center"!)
# delta: factor which denotes the expansion of the pattern; step size
# stop: delta at which the algorithm stops
# Nmax: Maximum number of iterations

pattern.search <- function(f, control) {
	start = control$start
	lower=control$lower
	upper=control$upper
	
	stop <- control$delta.min
	Nmax <- control$maxit
	delta <- control$delta
	
	# n: number of parameters
	n <- length(start)
	center <- matrix(0, ncol=n, nrow=(Nmax+1))
	colnames(center) <- names(start)
	f_center <- numeric(Nmax+1)
	# store evaluated points in search space + value
	visited <- as.data.frame(matrix(0, nrow=4*Nmax+1, ncol=n+1))
	colnames(visited) <- c(names(start), "val") 
	
	# Pattern matrix
	pattern <- cbind(diag(1,n), diag(-1,n))
	
	# Step 1: Initialization
	center[1,] <- start
	f_center[1] <- f(start)
	visited[1,] <- c(start, f_center[1])  
	vis.n <- 1
	# Step 2: Optimization
	for (k in 1:Nmax){
#		print(k)
#		print(center[k,])
		logger.debug("center", center[k,])
		
		# one step in every direction
		corners <- matrix(rep(center[k,], 2*n), nrow=2*n, byrow=TRUE) + delta[k]*t(pattern)
		# evaluate there
		f_corners <- numeric(2*n)
		# loop thru all corners
		for(i in 1:(2*n)) {
			# if we hit the boundary, go middle distance to bounday instead
			corner <- corners[i,]
#			print(corner)
			
			infeasible <- corner <= lower
			feasible.steps <- (center[k,] - lower)/2
			corner[infeasible] <- feasible.steps[infeasible]   			
			infeasible <- corner >= upper
			feasible.steps <- (upper - center[k,])/2
			corner[infeasible] <- feasible.steps[infeasible]
#			# only consider relevant rows/cols
#			v <- visited[1:vis.n, 1:n]
#			j <- which(apply(v, 1, function(y) all(y == corner)))
#			# corner allready vistited?
#			if (length(j) == 0) {
#				# found new one
				f_corners[i] <- f(corner)
#				vis.n <- vis.n+1
#				visited[vis.n, ] <- c(corner, f_corners[i])
#			} else {
#				f_corners[i] <- visited[j,"val"]
#			}
			# maybe we changed the corner
			corners[i,] <- corner
		}
		# Is the minimum value of the function of all corners smaller than the value of the function in the center,
		# choose this corner as new center
#		print(f_corners)
		
		if(min(f_corners) < f_center[k]){
			center[k+1,] <- corners[which.min(f_corners),]
			f_center[k+1] <- min(f_corners)
			delta[k+1] <- delta[k]
			# Otherwise keep the current center and minimize the step size (delta)
		}else{
			center[k+1,] <- center[k,]
			f_center[k+1] <- f_center[k]
			delta[k+1] <- 0.5*delta[k]
		}
		# Stop if delta gets too small, consider this as a local optimum
		if(delta[k+1] < stop){
			break;
		}
	}
	if(k == Nmax){
		warning("Optimization stopped: More than Nmax iterations!")
	}
	result <- as.data.frame(center[1:(k+1),])
	result$delta <- delta
	result$val <- f_center[1:(k+1)]
	par <- as.numeric(result[k+1,1:n, drop=TRUE])
	names(par) <- names(start)
	val <- result[k+1, "val"]
	return(list(par=par, val=val, path=result, n.eval=vis.n))
}


#Rosenbrock <- function(center) {
#	100 * (center[2] - center[1]^2)^2 + (1 - center[2])^2 # Minimum in (1,1)
#}
#p <- pattern.search(f = Rosenbrock, start = c(0,0), delta = 1, stop = 10^(-6), Nmax = 10000)
#print(p)