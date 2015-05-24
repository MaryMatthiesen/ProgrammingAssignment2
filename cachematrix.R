# The purpose of the pair of functions, makeCacheMatrix and cacheSolve, 
# is to cache the inverse of a matrix.

# makeCacheMatrix creates a special "matrix" object that can cache its inverse.
# It contains 4 functions: get, set, setinverse, and getinverse.
# get is a function that returns the matrix x stored in the main function.
# set is a function that changes the matrix stored in the main function.
# setinverse stores the value of the matrix inverse
# getinverse returns the value of the matrix inverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
}
get <- function() {
  x
}
setinverse <- function(inverse) {
  m <<- inverse
}
getinverse <- function() {
  m
}
list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# cacheSolve is a function that computes the inverse of the special "matrix" object
# returned by makeCacheMatrix. If the inverse has already been calculated (and the
# matrix has not changed), cacheSolve retrieves the inverse from the cache. 
#
#Important note: This function only works when the supplied matrix is both square
# invertible. (The matrix must not be singular, with a determinant of 0)

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
