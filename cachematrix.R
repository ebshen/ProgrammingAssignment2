## Programming assignment 2 - lexical scoping
## Two functions makeCacheMatrix() and cacheSolve() which work together
## to create a solution where the inverse of a matrix, when computed,
## is cached. If the matrix does not change, and the inverse operation is
## called, the cached value is returned. If the matrix is changed, and the
## inverse operation is called, the inverse is computed and cached.

## makeCacheMatrix() is a function which takes as its argument a square
## invertible matrix and creates a "special matrix" object and can
## cache a value which represents the inversion.
## set: sets the value of the matrix
## get: returns the value of the matrix
## setinverse: sets the value of the inverse
## getinverse: gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve() is a function which takes as its argument a
## "special matrix" and computes the inverse. If the inverse
## has already been calculated (and the matrix has not changed)
## the cached value is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
