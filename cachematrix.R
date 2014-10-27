## The following are a pair of functions designed to work together to 
## store a matrix and its inverse by storing both as part of a special 
## "matrix" object.

## makeCacheMatrix() 
    ## Takes a matrix and returns a "matrix" object that can be 
    ## manipulated by cacheSolve(). Specifically, it returns a list of 
    ## functions for interacting with the "matrix."
## cacheSolve() 
    ## Takes the special "matrix" object created by makeCacheMatrix()
    ## and computes the inverse of the matrix, caches it, and returns it.  
    ## If the original matrix is unchanged and the inverse has already been
    ## calculated and cached, cacheSolve() will return the cached inverse 
    ## rather than recalculate it.

#*****------------*****

## makeCacheMatrix()
    ## Create a special "matrix" object to allow caching the inverse 
    ## of a supplied matrix.
## inputs:
    ## x: a matrix
## outputs:
    ## a list containing:
        ## get/set functions for the input x
        ## getInverse/setInverse functions for the input x
        ## (note that setInverse should not be called by the user;
        ## it should only be called by cacheSolve().)

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL # set the inverse to null. This is done to initialize
                # the variable in the environment so it can be set and
                # retrieved by the functions below.
    
    ## Define the get/set functions for the input matrix x
    set <- function(y){
        x <<- y
        inv <<- NULL # we must null the inverse if we change the matrix
    }
    get <- function(){
        x
    }
    
    ## Define the getInverse/setInverse functions for x (setInverse should
    ## only be called by the cacheSolve() function, not the user)
    setInverse <- function(inverse){
        inv <<- inverse
    }
    getInverse <- function(){
        inv
    }
    
    ## Create and output a list with the functions defined above
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

#*****------------*****

## cacheSolve()
    ## Compute the inverse of the "matrix" returned by makeCacheMatrix().
    ## If the inverse has already been calculated and the original matrix
    ## has not changed, the previously computed inverse is retrieved (and 
    ## the inverse is not computed again). The supplied matrix is assumed
    ## to be square and invertible.
## inputs: 
    ## x: a matrix created by makeCacheMatrix()
## outputs:
    ## the inverse of matrix x

cacheSolve <- function(x, ...) {
    inv <- x$getInverse() # attempt to get the inverse from the 
                          # makeCacheMatrix() environment
    
    ## If an inverse was cached, great! We're done. Return it.
    if(!is.null(inv)){
        message("getting cached inverse")
        return(inv)
    }
    
    ## If we got here, we didn't have a cached inverse. So, calculate it,
    ## then cache it.
    matrix <- x$get() # get the matrix
    inv <- solve(matrix, ...) # calculate the inverse
    x$setInverse(inv) # cache the inverse
    
    ## Return the inverse
    inv 
}