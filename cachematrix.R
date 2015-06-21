## The following is a pair of functions that cache and compute the 
## inverse of a matrix.

## This function creates a special "matrix" object
## that can cache its inver.

makeCacheMatrix <- function(matx = matrix()) {
  inver <- NULL
  set <- function(x) {
    matx <<- x;
    inver <<- NULL;
  }
  get <- function() return(matx);
  setinv <- function(inv) inver <<- inv;
  getinv <- function() return(inver);
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(matx, ...) {
  inver <- matx$getinv()
  if(!is.null(inver)) {
    message("Getting the cached data...")
    return(inver)
  }
  data <- matx$get()
  inver <- solve(data, ...)
  matx$setinv(inver)
  return(inver)
}