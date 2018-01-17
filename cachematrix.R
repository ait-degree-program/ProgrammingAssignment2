## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that is used for caching the inverse of the matrix input.
## It checks for a valid matrix with an inverse before computing and caching the inverse value

makeCacheMatrix <- function(x = matrix()) {
  if(!is.matrix(x) || det(x) == 0){ # check input for valid matrix and/with a coputable inverse
    message("Invalid input; a valid matrix with a computable inverse is required")
    return ()
  }
  cachedInverse <- NULL
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL
  }
  get <- function() x
  setCache <- function(inverseVal) cachedInverse <<- inverseVal
  getCache <- function() cachedInverse
  list(set = set, get = get,
       setCache = setCache,
       getCache = getCache)

}

## This function is used for retrieving (it returns) the cached inverse-value of a matrix
## The function checks for a valid-cached-inverse of the matrix, and returns it if present,
## Else, a new cache-value in computed and stored (cached) for future use

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  print(x)
  cachedInverse <- x$getCache()
  if(!is.null(cachedInverse)) {
    message("getting cached data")
    return(cachedInverse)
  }
  message("setting data cache")
  data <- x$get()
  cachedInverse <- solve(data, ...)
  print(cachedInverse)
  x$setCache(cachedInverse)
  cachedInverse
}
