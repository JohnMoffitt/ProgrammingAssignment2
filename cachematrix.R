## makeCacheMatrix is in the same format as makeVector
## it replaces numeric vector class with matrix R object, it nulls out the matrix, sets the value of the matrix, 
## gets the value of the matrix, sets the value of the inverse matrix, gets the value of the inverse matrix

## makeCacheMatrix creates a special "matrix" R object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  mtx <- NULL
  set <- function(y) {
    x <<- y
    mtx <<- NULL
  }
  get <- function() x
  setInvMatrix <- function(solveMatrix) mtx <<- solveMatrix
  getInvMatrix <- function() mtx
  list(set = set, get = get, setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)
}


## this function mimics cachemean format.  It computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above.  If the inverse has already been calculated (and the matrix has not change), then the
## cachesolve should retrieve the inverse from the cache.  Computing the inverse of a square matrix can be done with
## the solve function in R.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mtx <- x$getInvMatrix()
  if(!is.null(mtx)) {
    message ("getting cached data") 
    return (mtx)
  }
  data <- x$get()
  mtx <- solve(data)
  x$setInvMatrix(mtx)
  mtx
}
