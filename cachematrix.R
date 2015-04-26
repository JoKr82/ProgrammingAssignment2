## Implementation of two functions to cache the calculated inverse of a matrix. 

## makeCacheMatrix creates a list of get and set functions for a provided matrix
## (get/set) and its inverse (getinverse/setinverse).
## Input: x = Matrix
## Output: List

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve takes a list created by the makeCacheMatrix function as input.
## In case the inverse matrix was not calculated yet (getinverse = NULL), or
## the matrix changed cacheSolve calculates and assigns it via setinverse.
## Input: x = Vector with get and set methods
## Output: m = Inverse of the matrix

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
   if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
