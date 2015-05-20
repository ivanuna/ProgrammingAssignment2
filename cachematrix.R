### Assignment: Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly. 

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  cs <- NULL
  set <- function(y) {
    x <<- y
    cs <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) cs <<- solve
  getsolve <- function() cs
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## computes the inverse of the special
## "matrix" returned by `makeCacheMatrix`
## If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  r <- x$getsolve()
  if(!is.null(r)) {
    message("getting cached data")
    return(r)
  }
  data <- x$get()
  r <- solve(data, ...)
  x$setsolve(r)
  r
}
