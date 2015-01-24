## Put comments here that give an overall description of what your
## functions do

# This class contains 2 functions:
# makeCacheMatrix: to create a cachematrix object from given matrix
# cacheSolve: returns the inverse of a given cachematrix object

## Write a short comment describing this function
# makeCacheMatrix: This function takes a matrix as input and creates 
# a cachematrix object with set, get, setinverse and getinverse functions.
makeCacheMatrix <- function(mat = matrix()) {
  inverse <- NULL
  set <- function(newmatrix) {
    mat <<- newmatrix
    inverse <<- NULL
  }
  get <- function() mat
  setinverse <- function(inverseofmatrix) inverse <<- inverseofmatrix
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
# cacheSolve: This function takes a cachematrix as an input and 
# tries to find its inverse. If the cachematrix object has inverse
# cached then this function will return that. Else it will 
# calculate newly and return.

cacheSolve <- function(mat, ...) {
  inverse <- mat$getinverse()
  if(!is.null(inverse)) {
    message("getting cached inverse")
    return(inverse)
  }
  cachematrix <- mat$get()
  inverse <- solve(cachematrix)
  mat$setinverse(inverse)
  inverse
}
