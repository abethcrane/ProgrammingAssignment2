# makeCacheMatrix returns an object that stores both a matrix and its inverse
# This saves time for operations that involve frequent lookups of inverse matrices
makeCacheMatrix <- function(m = matrix()) {
  inverse <- NULL
  isCached <- FALSE
  
  # Initialize our matrix to the parameter, and our inverse to NULL
  set <- function(x) {
    m <<- x
    inverse <<- NULL
    isCached <<- FALSE
  }
  
  # Return the matrix
  get <- function() m
  
  # Set the inverse matrix to the parameter
  setInverse <- function(i) {
    inverse <<- i
    isCached <<- TRUE
  }
  
  # Return our inverse matrix
  getInverse <- function() inverse
  
  # Returns whether or not the inverse has been cached
  getIsCached <- function() isCached
  
  # List out the functions of makeCacheMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse,
       getIsCached = getIsCached)
}

# cacheSolve takes a cacheMatrix and returns the inverse
# the inverse is either pre-cached, or solved and then cached for next time
cacheSolve <- function(m, ...) {
  # Check if the inverse has been cached
  if (m$getIsCached()) {
    inverse <- m$getInverse()
    # If it hasn't been, calculate it and cache it
  } else {
    data <- m$get() # retrieve the matrix from our cacheMatrix parameter
    inverse <- solve(data)
    m$setInverse(inverse)
  }
  
  # Return the inverse
  inverse
}