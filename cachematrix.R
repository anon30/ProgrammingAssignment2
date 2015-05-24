## These functions take a provided matrix, construct an
## object that can compute the inverse of the matrix,
## and cache the result to avoid recalculating the inverse.

## Creates a matrix object that can cache its inverse.
## Returns a list of methods that operate on the matrix:
## get, set, getInverse, and setInverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    get <- function() m
    set <- function(newMatrix) {
        m <<- newMatrix
        inv <<- NULL
    }
    getInverse <- function() inv
    setInverse <- function(i) {
        inv <<- i
    }
    list(get = get,
         set = set,
         getInverse = getInverse,
         setInverse = setInverse)
}


## Calculates the inverse of the provided matrix,
## caches the value, and returns it.

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        return(inverse)
    }
    inverse <- solve(x$get())
    x$setInverse(inverse)
    inverse
}
