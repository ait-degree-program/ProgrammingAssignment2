#The implementations below contains a pair of functions used for computing and caching (store for reuse) the inverse
#of a matrix.
#The function are set to only work with matrices with computable inverses - this means, providing a matrix which has no
#inverse will fail to compute and will produce a message stating so.

#Here is an easy way of calling/invoking the functions:
# 1. Define your matrix:
# myMatrix <- matrix( c(5, 1, 0,
#                       3,-1, 2,
#                       4, 0,-1), nrow=3, byrow=TRUE)

# 2. invoke the funtion to compute and cahce the inverse for reuse:
#    myMatrixObj <- makeCacheMatrix(myMatrix)

# 3. call the 2nd function to read the cached value of the matrix-inverse:
#    cacheSolve(myMatrixObj)


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
