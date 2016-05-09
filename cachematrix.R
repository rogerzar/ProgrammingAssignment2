## Functions to explore caching and retrieving a matrix inverse by making use of the <<- operator.
## Part of the Coursera course in R Programming, Week 3 assignment.
## REZ, May 2016

## The makeCacheMatrix function inputs a matrix (assumed invertible) and returns a list of four 
## functions to (a) set the value of the matrix, (b) get the value of the matrix, (c) set the 
## matrix inverse, and (d) get the matrix inverse.  

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(mxinverse) inv <<- mxinverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## The cacheSolve function inputs a matrix, checks to see if the inverse has already been computed 
## and cached, retrieves the inverse if it has been cached, otherwise, computes the inverse and 
## saves it in the cache.  

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    data <- x$get()
    message("computing inverse and caching it")
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
