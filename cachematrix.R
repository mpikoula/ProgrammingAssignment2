## The following set of functions allow the user to calculate the inverse of a matrix and cache the result in order to avoid unnecessary computation if the value of the matrix has not changed.

# makeCacheMatrix creates a special "matrix" object, which is really a list containing a function to set the value of a matrix, get the value of a matrix, set the value of the inverse and get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}

	get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = get inverse)
}


# The following function calculates the inverse of the "matrix" created with the above function. However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        # First check to see if the inverse has already been calculated
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	#If not, calculate using solve, and cache the value
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
