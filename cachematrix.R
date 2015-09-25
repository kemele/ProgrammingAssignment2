## Author: Kemele M. Endris
## Date: 26/09/2015

## This R Program contains a function that computes an inverse of a matrix. 
## Matrix inversion is usually a costly computation and there may be some benefits
## to caching the inverse of a matrix rather than computing it repeatedly. 
## This program contains two function:
##   * makeCacheMatrix, and
##   * cacheSolve

## Example run:
## > x <- makeCacheMatrix(matrix(c(2,2,3,2), nrow=2, ncol=2))
## > cacheSolve(x)
## [,1] [,2]
## [1,]   -1  1.5
## [2,]    1 -1.0
## > cacheSolve(x)
## Getting cached data ... 
## [,1] [,2]
## [1,]   -1  1.5
## [2,]    1 -1.0



## makeCacheMatrix: creates a special "matrix" object that can cache its inverse.
## Input 'x' = invertible matrix (assumed)

makeCacheMatrix <- function(x = matrix()) {
  minverse <- NULL
  set <- function(y) {
    x <<- y
    minverse <<- NULL
  }
  get <- function() x
  setinverse <- function(minv) minverse <<- minv
  getinverse <- function() minverse
  list(set = set, get = get, 
       setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix,
## If the inverse has been already calculated, then cacheSolve return the inverse from cache
## Input 'x' - makeCacheMatrix object
## returns 'minverse' - a matrix that is the inverse of the input (i.e, 'x')

cacheSolve <- function(x, ...) {
  minverse <- x$getinverse()
  if(!is.null(minverse)){
    message("Getting cached data ... ")
    return(minverse)
  }
  data <- x$get()
  minverse <- solve(data, ...)
  x$setinverse(minverse)
  minverse
}
