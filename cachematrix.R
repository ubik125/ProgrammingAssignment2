## This script in going to compute the inverse of a given matrix
## but since matrix inversion is computationally intensive there is a
## benefit to caching the inverse of a 
## matrix rather than compute it repeatedly.
## Below you will find a couple of functions that are used to create
## a "special" matrix that stores a matrix and caches its inverse.

## This function creates a special matrix, actually a list,
## that cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list (set = set, get = get, 
        setinv = setinv,
        getinv = getinv)
}

## This function computes the inverse of the special matrix created by 
## makeCacheMatrix. If the inverse has already been calculated
## then it gets the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
## Return a matrix that is the inverse of 'x'

