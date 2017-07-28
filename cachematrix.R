## Overall we want these functions to work together to compute inverted matrices.
## Since this is a computationally expensive proposition, we ensure there is caching functionality.
## In doing so, we avoid repeated calculations of something that has already been called.

## makeCacheMatrix creates a matrix that can cache its inverse.
## This function returns a list that is then solved by cacheSolve.
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


## cacheSolve computes the inverse of a matrix that is created by makeCacheMatrix.
## The function checks if not null, as if this is the case the result already exists and it is returned.
## If it is null, computations continue so that the inverted matrix can be stored for future use.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("i've seen this before. getting cached data")
    return(m)
  }
  ##this if is theoretically redundant, but by keeping it we can test that both situations don't happen on the same run.
  if(is.null(m)) {message("no cached data for this matrix. working on it...")}
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
