# This function creates a special matrix that is a list containing a function to:
#  1) set the value of the matrix
#  2) get the value of the matrix
#  3) set the value of the inverse
#  4) get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL

	set <- function(y) {
		x <<- y
		i <<- NULL
	}

	get <- function() x

	setinv <- function(solve) i <<- solve

	getinv <- function() i
	
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# This function check if the inverse of the matrix has been computed and returns the previously
#  computed value if it has and otherwise computes the matrix inverse
cacheSolve <- function(x, ...) {
	i <- x$getinv()

	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}

	data <- x$get()

	i <- solve(data, ...)

	x$setinv(i)

	i
}
