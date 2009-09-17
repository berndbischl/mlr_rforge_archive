# f: function to be optimized (depending on "center"!)
# start: start pattern center
# expansion: factor which denotes the expansion of the pattern; step size
# stop: expansion at which the algorithm stops
# Nmax: Maximum number of iterations

pattern.search <- function(f, start, expansion=1, stop=10^(-3), Nmax=100){

    # k: running index
    k <- 1

    # n: number of parameters
    n <- length(start)
    center <- matrix(0, ncol=n, nrow=Nmax+1)
    f_center <- numeric(0)

	
    # Pattern matrix
    pattern <- cbind(diag(1,n), diag(-1,n))

    # Step 1: Initialization
    center[1,] <- start
    f_center[1] <- f(start)

    # Step 2: Optimization
    repeat{
		print("center")
		print(str(center[k,]))
		
        # one step in every direction
        corners <- matrix(rep(center[k,], 2*n), nrow=2*n, byrow=TRUE) + expansion[k]*t(pattern)
        # evaluate there
        f_corners <- apply(corners, 1, f)

        # Is the minimum value of the function of all corners smaller than the value of the function in the center,
        # choose this corner as new center
        if(min(f_corners) < f_center[k]){
            center[k+1,] <- corners[which.min(f_corners),]
            f_center[k+1] <- min(f_corners)
            expansion[k+1] <- expansion[k]
        # Otherwise keep the current center and minimize the step size (expansion)
        }else{
            center[k+1,] <- center[k,]
            f_center[k+1] <- f_center[k]
            expansion[k+1] <- 0.5*expansion[k]
        }
        # Stopping rule
        if(expansion[k+1] < stop) return(center[k+1,])
        
		#Increase the running index
        k <- k+1

        # Stop after more than Nmax iterations
        if(k > Nmax){
            warning("Optimization stopped: More than Nmax iterations!")
            break
        }
    }
}


#Rosenbrock <- function(center) {
#	100 * (center[2] - center[1]^2)^2 + (1 - center[2])^2 # Minimum in (1,1)
#}
#p <- pattern.search(f = Rosenbrock, start = c(0,0), expansion = 1, stop = 10^(-6), Nmax = 10000)
#print(p)