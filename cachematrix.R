## Creates an invertible matrix and returns it's inverse. Stores the inverse
## in the cache and recalls it, if unavailable calculates and returns.

## Pre-requisites:
## 1. makeCacheMatrix must be assigned to a variable before starting
## 2. an invertible matrix must be supplied to the set element of the variable

## e.g. 
## a <- makeCacheMatrix()
## a$set(matrix(1:4,2,2))
## cacheSolve(a)
##      [,1] [,2]     Sample output
## [1,]   -2  1.5
## [2,]    1 -0.5

## Subsequent runs of the cacheSolve(a) for the same matrix will return the same
## solution, with an additional "Getting cached data" message immediately above.

## Creates a special matrix object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Computes the inverse of makeCacheMatrix function above

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("Getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m ## Return a matrix that is the inverse of 'x'
}
