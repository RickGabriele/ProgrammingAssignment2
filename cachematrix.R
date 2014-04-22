# Overall these functions allow one to cache the potentially expensive operation
# of matrix inversion, thereby making possible more efficient processing. 
# Version 6, 4/21/2014

# Sets up a specical "matrix" along with functions to set the matrix,
# get the matrix, set the inverse of the matrix, and get the Inverse of the matrix


makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(Inv) m <<- Inv
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


#This function computes the inverse of the matrix unless the
#inverse has already been computed.  If already computed it is
#retrieved from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
