## These two functions create a matrix that will provide an inverse.
## The inverse will be lazily loaded and cached

## Creates a special matrix function/object that can provide a cached inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Solves the the inverse function for the special matrix function/object
## and will will provide the inverse from the cache if needed.
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)){
    message("Getting cached inversed data")
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}
