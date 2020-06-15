## Assignment: Caching the Inverse of a Matrix


##  This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # Initialize cached matrix
  mc <- NULL
  
  # Create matrix
  set <- function(y) {
    x <<- y
    mc <<- NULL
  }
  
  # Get matrix value
  get <- function() x
  
  # Invert matrix & cache it 
  setmat <- function(inver) mc <<- inver
  
  # Get matrix from cache
  getinv <- function() mc
  
  # Return list
  list(set = set, get = get,
       setmat = setmat,
       getinv = getinv)
  
}


## This function computes the inverse of the special "matrix" returned by 
## `makeCacheMatrix` above. If the inverse has already been calculated (and the 
## matrix has not changed), then `cacheSolve` should retrieve the inverse from 
## the cache.
cacheSolve <- function(x, ...) {
  # Get the inverse 
  mc <- x$getinv()
  
  # if exist, get, else create
  if(!is.null(mc)) {
    message("getting cached data")
    return(mc)
  }
  data <- x$get()
  
  # inverse matrix & cache it
  mc <- solve(data, ...)
  x$setmat(mc)
  
  # Return value
  mc
}
