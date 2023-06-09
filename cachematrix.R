## Put comments here that give an overall description of what your
## functions do
# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the inverse matrix cache
  inv <- NULL
  
  ## Function to set the matrix
  set <- function(newmat) {
    x <<- newmat  # Use '<<-' to assign in parent environment
    inv <<- NULL  # Reset the inverse cache
  }
  
  ## Function to get the matrix
  get <- function() {
    x
  }
  
  ## Function to get the cached inverse matrix
  getInverse <- function() {
    inv
  }
  
  ##Function to cache the inverse of the matrix
  cacheInverse <- function() {
    if (is.null(inv)) {
      inv <<- solve(x)  # Compute and cache the inverse
    }
    inv
  }
  
  ## Return a list of functions
  list(set = set, get = get, getInverse = getInverse, cacheInverse = cacheInverse)
}

## Function to compute the inverse of a matrix, with caching
cacheSolve <- function(cachedMatrix, ...) {
  # Check if the inverse matrix is already cached
  inv <- cachedMatrix$getInverse()
  if (!is.null(inv)) {
    message("Getting cached inverse matrix")
    return(inv)
  }
  
  ## If not cached, compute the inverse and cache it
  mat <- cachedMatrix$get()
  inv <- solve(mat, ...)
  cachedMatrix$setInverse(inv)
  inv
}

