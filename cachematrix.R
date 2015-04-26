## The first function (makeCacheMatrix) sets up a list containing the 
##variables needed to run the second function (cacheSolve), which calculates the 
##inverse of the matrix if it doesn't already exist.

## The makeCacheMatrix function is directly modified from the code
## that Prof Peng shared with us for the vector function. It generates
## a list of variables to be passed to the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  }

## This function will check to see if an inverse of the matrix
## already exists and return that if it does, otherwise it will 
## calculate the inverse and return the calculated value.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    return(inv)
  }
