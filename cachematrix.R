## These functions help avoid computing the inverse of the same matrix
## more than once (to save time when working with large matrices).
## Given an invertible square matrix (an object of the matrix class),
## makeCacheMatrix returns the new "matrix" object.
## When this object is passed to cacheSolve for the first time,
## the inverse of the matrix will be computed and cahced.

## Creates a special "matrix" object that can cache its inverse
## and provides the following functions:
## set(), get() - sets/gets the value of the matrix
## setinv(), getinv() - sets/gets the value of the inverse

makeCacheMatrix <- function(X = matrix()) {
  inv <- NULL
  set <- function(Y) {
    X <<- Y
    inv <<- NULL
  }
  get <- function() X
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Either retrieves the cached inverse of a "matrix" object 
## made with makeCacheMatrix or (if the object's cache is empty) 
## computes the inverse and caches the result.

cacheSolve <- function(X, ...) {
  inv <- X$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- X$get()
  inv <- solve(data, ...)
  X$setinv(inv)
  inv
}