
## This function creates a special “matrix” object 
## that can cache its inverse
# First take a matrix as the input
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    # The value of the matrix is set in different environment
    setMatrix <- function(y) {
        x <<- y
        inv <<- NULL
    }
    # Get value of the matrix
    getMatrix <- function() x
    # Set value of inverse matrix
    setInverse <- function(inverse) inv <<- inverse
    # Get value of inverse matrix
    getInverse <- function() inv
    list(setMatrix = setMatrix, 
         getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}


# This function computes the inverse of the special “matrix” 
# returned by makeCacheMatrix above.
# If the inverse has already been previously calculated 
# (and the there has been no change to the matrix),
# then the cacheSolve retrieves the inverse from the cache.
# Otherwise it uses the solve function to set the inverse matrix itself.

cacheSolve <- function(x, ...) {
    ## From the previous makeCacheMatrix function
    ## return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    # The inverse matrix is returned from cache if is not null
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # Otherwise if the inverse matrix was null from previous function
    # Retrieve matrix input data and... 
    # use solve function to create inverse matrix.
    data <- x$getMatrix()
    inv <- solve(data, ...)
    x$setInverse(inv)
    return(inv)
}
