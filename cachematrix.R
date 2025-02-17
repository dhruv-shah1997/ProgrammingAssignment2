## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the cache for inverse
  
  # Function to set the matrix value
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset cache when a new matrix is set
  }
  
  # Function to get the matrix value
  get <- function() x
  
  # Function to set the inverse value
  setInverse <- function(inverse) inv <<- inverse
  
  # Function to get the inverse value
  getInverse <- function() inv
  
  # Return a list of functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated, it retrieves it from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  # Check if inverse is already cached
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Compute the inverse and cache it
  data <- x$get()
  inv <- solve(data, ...)  # Compute the inverse
  x$setInverse(inv)  # Cache the inverse
  inv  # Return the inverse matrix
}


