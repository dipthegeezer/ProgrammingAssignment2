## These functions cache the inverse of a matrix
## a potentially expensive operation

makeCacheMatrix <- function(x = matrix()) {
    # This function creates a special "matrix" object that can cache its inverse.
    #
    # Args:
    #   x: A matrix object containing a square matrix that is invertible.
    #
    # Returns:
    #   A list object containing functions to:
    #       set the value of the matrix: set
    #       get the value of the matrix: get
    #       set the value of the inverse of the matrix: setInverse
    #       get the value of the inverse of the matrix: getInverse
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
    # This function computes the inverse of the special "matrix" returned by 
    # makeCacheMatrix above. If the inverse has already been calculated (and 
    # the matrix has not changed), then the cachesolve should retrieve the
    # inverse from the cache.
    #
    # Args:
    #   x: A cacheMatrix object as returned from makeCacheMatrix function.
    #
    # Returns:
    #   A matrix object which is the inverse of the matrix passed into the 
    #   function.
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
