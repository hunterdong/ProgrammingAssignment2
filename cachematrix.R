## cachematrix.R
## Author: Hunter Dong
## Course: R Programming (Coursera)
## Description: Matrix inversion is usually a costly computation and there may
## be some benefit to caching the inverse of a matrix rather than compute it 
## repeatedly. The following two functions create a matrix and cache its inverse
## so time is not wasted recalculating inverses.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    ## sets the matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    ## gets the matrix
    get <- function() x
    ## sets the inverse of the matrix
    setinverse <- function(inversematrix) inverse <<- inversematrix
    ## gets the inverse of the matrix
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## cacheSolve returns the inverse of a matrix
## it checks if the inverse has already been calculated, if so it uses the
## cached results and skips computation. If the inverse is not found,
## cacheSolve calculates the inverse and caches it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    ## if inverse matrix is found in cache, return
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    ## if inverse is not found, get original matrix and calculate inverse
    data <- x$get()
    inverse <- solve(data,...)
    ## store inverse in cache
    x$setinverse(inverse)
    ## return inverse
    inverse
}
