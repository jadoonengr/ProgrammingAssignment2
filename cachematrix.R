## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a 
## matrix rather than computing it repeatedly

## -----------------------------------------------------
## This function creates a special "matrix" object that
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  # Buffer initially set to NULL
  inv <- NULL
  
  # Sets the matrix, but not the inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Gets the matrix, but not the inverse
  get <- function() x
  
  # Manually set the inverse
  setinv <- function(inverse) inv <<- inverse
  
  # Get the inverse
  getinv <- function() inv
  
  # List encapsulation
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}

## -----------------------------------------------------
## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # CASE: Inverse already computed
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached inverse data")
    return(i)
  }
  
  # CASE: Inverse not computed yet
  # Get the matrix
  data <- x$get()
  
  # Compute inverse
  i <- solve(data, ...)
  
  # Store the result
  x$setinv(i)
  
  # Return new result
  i
}
