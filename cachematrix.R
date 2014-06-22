## Put comments here that give an overall description of what your
## functions do

# This module introduces special type of matrix with cached inverse.
# makeCachedMatrix creates and initialize new object
# cacheSolve computes inverse using cached result if possible.

## Write a short comment describing this function

# This is function that creates "object" matrix with cachable inverse
# parameters: x - of type matrix, if not specified creates empty matrix
# 
# methods: 
#   set: sets "raw" matrix in object
#   get: gets "raw" matrix from object
#   setinverse:  sets inverse of matrix to cache
#   getinverse:  gets cached inverse of matrix (or NULL)
     

makeCacheMatrix <- function(x = matrix()) {
    #cache for inverted matrix
    inverse <- NULL
    
    #sets "raw" matrix and initialize cache to NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }

    #gets "raw" matrix - R built in matrix type
    get <- function() x

    #sets matrix 'inv' to cache 
    setinverse <- function(inv) inverse <<- inv

    #gets inverted matrix from cache (or NULL)
    getinverse <- function() inverse

    #create object
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
# This is function to calculate inverse of matrix (special object created by
#"makeCacheMatrix" function), if object has cached inverse, cache is used and 
#calculation is skiped. Otherwise it calculates inverse using function "solve"
#and result is cached.


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    #retrieve what is stored in cached
    inv <- x$getinverse()

    #if inverse is already calculated, use it
    if(!is.null(inv)) {
        message("getting cached inverted matrix")
        return inv
    }

    #nothig was cached, so get matrix to inverte it
    toinvert <- x$get()

    #inversion happens here
    inv <- solve(toinvert)

    #store result for future use
    x$setinverse(inv)
    inv
}
