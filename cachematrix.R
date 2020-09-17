## I wrote this program with two functions to cache the inverse of a matrix

## My first function is makeCacheMatrix and this function creates 
## a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(matrix) {
    x <<- matrix
    inv <<- NULL
  }
  get <- function(){
    x
  } 
  setInverse <- function(inverse){
    inv <<- inverse
  }
  getInverse <- function(){
    inv
  } 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## My next and the last function is cacheSolve and this function computes the inverse 
## of the special "matrix" returned by makeCacheMatrix and if the inverse has already 
## calculate recover its from the cache

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}