## makeCacheMatrix and cacheSolve can be used together to cache 
## the inverse of a square matrix.

## makeCacheMatrix encodes a matrix and its inverse in a list of
## functions, effectively creating a "matrix" whose inverse can be
## cached.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set = set, get = get,
		setinv = setinv, getinv = getinv)
}

## cacheSolve takes a "matrix" created by makeCacheMatrix and finds
## its inverse, or retrieves it from the cache if the calculation 
## has already been done.

cacheSolve <- function(x, ...) {
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}