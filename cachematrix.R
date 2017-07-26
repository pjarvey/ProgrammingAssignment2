## The two functions below create a cache of the inverse of matrix objects,
## and use cached values to calculate matrix inverses with less total computation time.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  # Setup a variable to hold cached values, initiate as NULL
  cache <- NULL
  
  # set a matrix
  setMatrix <- function(y) {
    x <<- y
    # empty the cache
    cache <<- NULL
  }
  
  # return the stored matrix
  getMatrix <- function() {
    x
  }
  
  # calculate the inverse of the matrix, cache the result
  setInverse <- function(solve) {
    cache <<- solve
  }
  
  # get the cached inverse
  getInverse <- function(){
    cache
  }
  
  #return a list, composed of function for getting/caching 
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
##`cacheSolve` will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

  cache <- x$getInverse()
  if(!is.null(cache)) {
    message("getting cached data")
    return(cache)
  }
  data <- x$getMatrix()
  cache <- solve(data, ...)
  x$setInverse(cache)
  cache
}
