## This script creates functions that can cache the inverse of a matrix in
## order to avoid the need to recalculate it.

## Function: makeCacheMatrix
## Description: Creates a special list object for the matrix that can be used set 
## and retrieve the matrix and inverse for caching purposes

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL

    ## Set the matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    ## Get the matrix
    get <- function() x
    ## Set the inverse
    setinverse <- function(k) inverse <<- k
    ## Get the inverse
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Function: cacheSolve
## Description: Uses the special object to 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## First retrieve the cached matrix and inverse
    inverse <- x$getinverse()
  
    ## If cached version exists then we return it
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    ## If cached version does not exist then we find the inverse, set the cache and return
    message("Calculating inverse")
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
