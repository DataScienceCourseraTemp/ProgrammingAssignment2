## Put comments here that give an overall description of what your
## functions do

# The first function, makeCacheMatrix takes in a matrix,
# and returns a list of functions to
# 
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  # function to set the value of the matrix
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # function to get the value of the matrix
  get <- function() x
  
  # function to set the inverse of the matrix
  setinverse <- function(solve) m <<- solve
  
  # function to get the inverse of the matrix
  getinverse <- function() m
  
  # return functions as a list 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# This function uses the output of makeCacheMatrix to 
# calculate and return the inverse of the original matrix.
# However, it first checks to see if the inverse has already been calculated.
# If so, it gets the inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse of the matrix and sets the value of the inverse
# in the cache via the solve function.

cacheSolve <- function(x, ...) {
  ## determine if inverse has already been calculated
  m <- x$getinverse()
  
  # if inverse has already been calculated, return inverse from cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # if inverse has not already been calculated
  # calculate, cache, and return inverse
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
