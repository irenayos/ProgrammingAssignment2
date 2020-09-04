## Matrix inversion is usually a costly computation, therefore there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly
## We will create two functions called makeCacheMatrix and cacheSolve to present
## caching mechanism go get the inverse of a matrix

## makeCacheMatrix is a function to creates a special "matrix" object that can cache its inverse
## 'x' is a matrix that we assume that the matrix supplied is always invertible

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invs) inv <<- invs
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve is a function to computes the inverse of the special "matrix" returned by makeCacheMatrix 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache
## 'x' is the return value of makeCacheMatrix function
## it will return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
