## A pair of functions that cache the inverse of a matrix.
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	  #initialize the inver
        inver <- NULL
        set <- function(y) {
                x <<- y
                inver <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) {inver <<- inverse}
        getInverse <- function() {inver }
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}
## A pair of functions that cache the inverse of a matrix.
## This function creates a special "matrix" object that can cache its inverse.
cacheSolve <- function(x, ...) {
        inver <- x$getInverse()
	  #check whether there is a cache
        if (!is.null(inver )) {
		    # cache hit
                message("getting cached data")
                return(inver )
        }
	  # cache miss
        mat <- x$get()
        inver <- solve(mat, ...)
        x$setInverse(inver )
        inver 
}
