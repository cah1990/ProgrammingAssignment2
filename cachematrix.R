## Put comments here that give an overall description of what your
## functions do
# makeCacheMatrix creates a list of four functions:
# get, set, getinv, setinv
# These four functions manipulate the values used to form an invertible matrix

## Write a short comment describing this function
# set adds new values to a matrix and clears any existing inverse matrix (m)
# get simply calls up any existing values
# setinv places a value for an inverted matrix into a variable
# getinv retrieves the value stored for the inverterd matrix

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() {
            x
      }
      setinv <- function(nvrt) {
            m <<- nvrt
      }
      getinv <- function() {
            m
      }
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## Write a short comment describing this function
# cacheSolve will read the value of the matrix created via makeCacheMatrix
# If an inverse alredy exists, that value will be returned.  If new
# matrix values exist, then a new inverse matrix is determined and returned.

cacheSolve <- function(x, ...) {
      m <- x$getinv()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinv(m)
      m
}

