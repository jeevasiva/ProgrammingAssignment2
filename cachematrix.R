## This function creates a special matrix object than can cache its inverse.
## Saves the matrix to variable x and its inverse to variable m in scope
## Four functions: 1) get - Function that returns the matrix
##                 2) set - Function that sets matrix 
##                 3) setInverse - Function that sets the value of inverse of the matrix
##                 4) getInverse - Function that returns the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        ## Function that changes the vector stored in main function
        set <- function(y) {
            x <<- y
            m <<- NULL            
        }
        ## Function that returns the vector x stored in main function
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



##This function computes the inverse of the "matrix" returned by makeCacheMatrix. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesSolve retrieves the inverse from the cache through SetInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
	     if(!is.null(m)) {
	         message("Getting Cached Data")
	         return(m)
	     }
	     data <- x$get()
	     m <- solve(data, ...)
	     x$setInverse(m)
             m
}
