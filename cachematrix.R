# makeCacheMatrix creates a special "matrix", which is really a list containing
# the following functions:
# - set: set the value of the matrix
# - get: get the value of the matrix
# - setinverse: set the value of the matrix inverse
# - getinverse: get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


#
# This function takes a "special matrix" created by makeCacheMatrix
# and checks if the inverse has already been computed. If so
# returns the previously computed value, otherwise it will compute
# the inverse and store it to avoid computing it again
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
