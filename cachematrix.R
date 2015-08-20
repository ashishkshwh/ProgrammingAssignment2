## This script calculates the inverse of a matrix by caching it
## The inverse is calculated by cacheSolve(M), where M is a matrix

## First function, makeCacheMatrix is a function that gives a
##list of functions:
## getMatrix returns the matrix itself
## getInverse returns the the value stored in m
## setInverse set the value of m equal to the inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    getMatrix <- function() x
    getInverse <- function() m
    setInverse <- function(inv) m <<- inv
    
    list(getMatrix = getMatrix,getInverse=getInverse,
         setInverse=setInverse)
}

## cacheSolve checks if the inverse is calculated,
## or not (m is NULL or not). If yes, it fetches the stored 
## value of inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    mat <- makeCacheMatrix(x)
    m <- mat$getInverse()
    if (!is.null(m)) {
        message("Getting cache inverse")
        return(m)
    }
    matrx <- mat$getMatrix()
    m <- solve(matrx)
    mat$setInverse(m)
    m
}