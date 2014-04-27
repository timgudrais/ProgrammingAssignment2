## This script creates two functions:
##
##    (1)   "makeCacheMatrix" - Which creates a 4-in-1 function using lists, allowing us to do the following:
##          * SET the parameters of a matrix
##          * GET the previously stored matrix
##          * SET the inverse function to call in a separate computation function "cacheSolve"
##          * GET the inverse matrix stored using the separate computation function "cacheSolve"
##
##    (2)   "cacheSolve" - Which performs the inverse matrix computations using cached values to save computation time.
##

## FUNCTION (1)

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  
  ## Creates 'SET' function to set the parameters of the matrix
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  
  ## Creates 'GET' function to get the previously stored matrix
  get <- function() {
    x
  }
  
  ## Creates 'SETINVERSE' function to call in a "cacheSolve"
  setInverse <- function(solve) {
    im <<- solve
  }
  
  ## Creates 'GETINVERSE' function to call the stored
  getInverse <- function() {
    im
  }
  
  ## Creates the 4-in-1 function by storing the functions above in a list, making them callable using '$'
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## FUNCTION (2) 

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  im <- x$getInverse()
  
  ## Conditional statement checking if the matrix has changed since the previous computation: 
  ##  * If it hasn't, the function performs the computation using the cached values. 
  ##  * If it has, the function reiterates the computation using the new values.
  if(!is.null(im)) {
    message("Getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setInverse(im)
  im
}
