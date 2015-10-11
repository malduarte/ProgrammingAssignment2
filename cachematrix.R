# makeCacheMatrix creates a special "matrix", which is really a list containing
# the following functions:

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  # - set: set the value of the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # - get: get the value of the matrix
  get <- function() x
  
  # - setinverse: set the value of the matrix inverse
  setinverse <- function(inverse) i <<- inverse
  
  # - getinverse: get the value of the matrix inverse
  getinverse <- function() i
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

# This function takes a "special matrix" created by makeCacheMatrix
# and checks if the inverse has already been computed. If so
# returns the previously computed value, otherwise it will compute
# the inverse and store it to avoid computing it again
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
      message("returning cached data")
      return(i)
    }
    data <- x$get()
    # compute the inverse
    i <- solve(data, ...)
    
    # store the inverse in the object returned by makeCacheMatrix
    x$setinverse(i)
    i
}
