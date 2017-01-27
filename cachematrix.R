## This file sets a matrix and returns it's inverse value.

## creates a special list containing a function to
## set the value of a matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  M <- NULL
  set <- function(y) {
    x <<- y
    M <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) M <<- solve
  getinverse <- function() M
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates the inverse of the special 
## list created by the previous function. But it first 
## checks to see if the inverse is already cached and uses 
## the cached value if available.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  M <- x$getinverse()
  if(!is.null(M)) {
    message("getting cached data")
    return(M)
  }
  data <- x$get()
  M <- solve(data, ...)
  x$setinverse(M)
  M
}
