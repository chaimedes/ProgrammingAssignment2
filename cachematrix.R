# cachematrix.R
# Modified by Martin Berlove on 1/20/2015
# For Johns Hopkins "R Programming" via Coursera, 
# Programming Assignment 2
#
# Provides basic tools for handling matrix inverse operations,
# using caching where possible to promote efficiency.


# ------------------------------------------------------------------
# Storage and management for a matrix and its inverse, with caching.
# ------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL # Prepare for storing inverse
  
  # Matrix getter and setter
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() { x }
  
  # Matrix inverse getter and setter
  setInverse <- function(inverse) { inv <<- inverse }
  getInverse <- function() { m }
  
  # Return a list of the functions we've defined.
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
} # End of makeCacheMatrix(1)



# ------------------------------------------------------------------
# Return the inverse of a matrix.
# ------------------------------------------------------------------
cacheSolve <- function(x, ...) {
  
  # Check if an inverse exists and is cached.
  matrix <- x$getInverse()
  if (!is.null(matrix)) {
    message("Retrieving cached matrix...");
    return(matrix)
  }
  
  # If not, get the original matrix and compute inverse.
  data <- x$get()
  matrix <- solve(data)
  x$setInverse(matrix)
  
  # Return the matrix
  matrix
  
} # End of cacheSolve(...)
