##################################################################
## When used together, makeCacheMatrix and cacheSolve           ##
## allow the inverse of a matrix to be stored and retrieved     ##
## later instead of always having to be computed.               ##
##                                                              ##
## This code was adapted from the example code for Programming  ##
## Assignment 2 in the R Programming Coursera course taught by  ##
## Roger D. Peng, PhD, Jeff Leek, PhD, and Brian Caffo, PhD.    ##
## Their example code allowed for the mean of a vector to be    ##
## cached and retrieved instead of computed every time.         ##
##################################################################

## makeCacheMatrix returns a matrix that is capable of
## storing its own inverse. When used with cacheSolve,
## the returned matrix can save computation time
## by allowing the cached inverse to be retrieved
## instead of always having to be computed.

makeCacheMatrix <- function(x = matrix()) {
        # set the inverse of the matrix to NULL
        inv <- NULL
        
        # set or change the matrix value
        set <- function(y) {
                # set the outer function 'x'
                # to the inner function 'y'
                x <<- y
                
                # set inverse to NULL so that
                # the cached inverse is not returned
                # if the matrix is changed
                inv <<- NULL
        }
        
        # return the special matrix 'x'
        get <- function() x
        
        # cache the inverse of the matrix 'x'
        setinverse <- function(inverse) inv <<- inverse
        
        # retrieve the cached inverse
        getinverse <- function() inv
        
        # return a list of the available functions
        # to set/get the matrix and its inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve returns the inverse of a special matrix 'x'
## 'x' is created using the makeCacheMatrix function
## If the inverse of 'x' has already been computed and cached,
## then the cached inverse is retrieved and returned.
## Otherwise, the inverse and computed, cached in 'x', and returned.

cacheSolve <- function(x, ...) {
        # try to retrieve the inverse of 'x'
        inv <- x$getinverse()
        
        # check if 'x' has an inverse (if it is not null)
        if(!is.null(inv)) {
                message("getting cached data")
                
                # return cached inverse
                return(inv)
        }
        
        # if 'x' does not have a cached inverse,
        # then retrieve the matrix 'x'
        data <- x$get()
        
        # solve 'x' to compute its inverse
        inv <- solve(data, ...)
        
        # store the computed inverse in 'x'
        x$setinv(inv)
        
        # return the inverse of 'x'
        inv
}

## Test using diag(5000) matrix
