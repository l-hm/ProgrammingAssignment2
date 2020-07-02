## Computes the inverse of a square matrix with the solve function.


## markeCacheMatrix caches the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) { ## assume matrix is invertible
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL## capable of modifying functions outside parent function
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve computes inverse of matrix

cacheSolve <- function(x, ...) { ## compute inverse of matrix
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get() 
  inv <- solve(mat, ...) ## standard function to compute matrix
  x$setInverse(inv)
  inv
}
