## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## Create a function which takes a matrix named J as an argument
## This function can create a special matrix object that can cache its inverse.

makeCacheMatrix <- function(J = matrix()) {
  inv <- NULL
  set <- function(y) {
    J <<- y
    inv <<- NULL
  }
  get <- function() J
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## Create a function getting matrix J as argument along with other prossible arguments 

cacheSolve <- function(J, ...) {
  ## Return a matrix that is the inverse of 'J'
  inv <- J$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- J$get()
  inv <- solve(mat, ...)
  J$setInverse(inv)
  inv
}