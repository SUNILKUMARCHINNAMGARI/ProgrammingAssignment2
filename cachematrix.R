## makeCacheMatrix creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
matinv <<- solve(x)
  set <- function(y) {
    x <<- y
    matinv <<- NULL
  }
  get <- function() x
  setmatinv <- function(matinv) matinv <<- matinv
  getmatinv <- function() matinv
  list(set = set, get = get,
       setmatinv = setmatinv,
       getmatinv = getmatinv)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has
## already been calculated (and the matrix has not changed), then cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
         matinv <- x$getmatinv()
  if (!is.null(matinv)) {
    message("getting cached data")
    return(matinv)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatinv(matinv)
  matinv
}
