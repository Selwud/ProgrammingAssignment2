## Assignment: Caching the Inverse of a Matrix


##  This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setcache <- function(cache) m <<- cache
  getcache <- function() m
  list(set = set, get = get,
       setcache = setcache,
       getcache = getcache)
  
}


## This function computes the inverse of the special "matrix" returned by 
## `makeCacheMatrix` above. If the inverse has already been calculated (and the 
## matrix has not changed), then `cacheSolve` should retrieve the inverse from 
## the cache.

cacheSolve <- function(x, ...) {
  m <- x$getcache()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setcache(m)
  m
  ## Return a matrix that is the inverse of 'x'
}
