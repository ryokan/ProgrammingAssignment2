## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix(x = matrix()) 
## Argument
## x: a square numeric matrix
## Function
## returns a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  getSolve <- function() s
  setSolve = function(solve) s <<- solve
  list(set = set, get = get,
       getSolve = getSolve,
       setSolve = setSolve)
}

## cacheSolve(x, ...)
## Arguments 
## x: a special matrix objevt created by makeCacheMatrix
## Function
## if x has the pre-calcurated inverse matrix, then returns it.
## otherwise calcurate inversion and returns the result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getSolve()
  if (!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  message("calcurating")
  orig <- x$get()
  s <- solve(orig, ...)
  x$setSolve(s)
  s
}
