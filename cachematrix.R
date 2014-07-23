## This is a piece of code for the Programming Assignment 2
## of the Coursera course R Programming of 2014.
## The goal of this program is to learn how to work with
## cached values and computations.

## The function makeCacheMatrix takes a matrix as a parameter
## and returns a vector of functions
## set(), get(), setinverse() and getinverse().
## The names are self-explanatory, hopefully.
## The variable 'inverse' is for storing the inverse of a matrix,
## computed with the help of cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverse <<- solve(x)
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function manipulates the inverse matrix,
## stored in the result of makeCacheMatrix().
## If the inverse already exists (not NULL), 
## then no calculation happens.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data") ## the inverse already exists
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...) ## actually inverse matrix
  x$setinverse(inverse)
  inverse
}
