## "makeCacheMatrix" creates a matrix object and can store a cache of said matrix's inverse. "cacheSolve" calls the inverse 
## of the matrix object from "makeCacheMatrix" if it exists; if the inverse does not exists, "cacheSolve" 
## will compute the inverse.


## This function creates a matrix object with the ability to store a cache of its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inv) inverse <<- inv
  
  getinverse <- function() inverse
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## This functions returns the inverse of a matrix if it exists.
## Calculates and returns the inverse of a matrix if it does not exist.

cacheSolve <- function(x, ...) {
  
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
