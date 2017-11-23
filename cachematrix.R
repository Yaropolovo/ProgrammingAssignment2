## My functions are pretty similar to "makeVector" and "cachemean" from example. 

## This function is a list of 4 functions for getting and setting our matrix and inversion of it. 
makeCacheMatrix <- function(x = matrix()) {
  inverted <- NULL
  set <- function(y)
  {
    x <<- y
    inverted <<- NULL
  }
  get <- function() x
  setinvert <- function(inverted) inverted <<- inverted
  getinvert <- function() inverted
  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert)
}


## This function check inversion and create it if object is null. 
## If there is inverted cached matrix then we don't recalculate.  
cacheSolve <- function(x, ...) {
  i <- x$getinvert()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinvert(i)
  i     
}