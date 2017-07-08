## R Programming Assignment 2 
## Matrix Inverse Caching and Lexical Scoping example

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## To get the Cached Data if the matrix is not changed else recompute
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

## Code to test the function 
## testMatrix <- makeCacheMatrix(matrix(c(1,2,2,4,3,4,1,1,4),3,3))
## testMatrix$get()
## cacheSolve(testMatrix)
## cacheSolve(testMatrix)
## testMatrix$set(matrix(1:4,2,2))
## cacheSolve(testMatrix)
## cacheSolve(testMatrix)


