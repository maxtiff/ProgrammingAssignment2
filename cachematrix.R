## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix and a cache of its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # Declare variable to strore inverse
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}


## This functions returns the inverse of a matrix if it exists.
## Calculates and returns the inverse of a matrix if it does not exist.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
  
}
