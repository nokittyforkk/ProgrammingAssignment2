# Create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x) {
  # Initialize the cache for the inverse
  inverse <- NULL
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  # Function to get the matrix
  get <- function() {
    x
  }
  
  # Function to set and cache the inverse of the matrix
  setInverse <- function(solveMatrix) {
    inverse <<- solveMatrix
  }
  
  # Function to get the cached inverse of the matrix
  getInverse <- function() {
    inverse
  }
  
  # Return an object with caching functionality
  list(set = set, get = get,
       setInverse = setInverse, getInverse = getInverse)
}

# Compute the inverse of the cached matrix
cacheSolve <- function(cacheMatrix) {
  # Get the cached inverse of the matrix
  inverse <- cacheMatrix$getInverse()
  
  # If the cached inverse exists, retrieve it
  if (!is.null(inverse)) {
    message("Getting cached inverse.")
    return(inverse)
  }
  
  # Compute the matrix inverse
  matrix <- cacheMatrix$get()
  inverse <- solve(matrix)
  
  # Cache the computed inverse
  cacheMatrix$setInverse(inverse)
  
  return(inverse)
}
