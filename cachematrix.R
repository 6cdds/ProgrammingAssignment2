## This R file contains the "makeCacheMatrix"
## and the "cacheSolve" functions.
## 
## The "makeCacheMatrix" function is a cache function
## that allows storing and getting of the inverse
## of a matrix.

## The "cacheSolve" function uses "makeCacheMatrix"
## to calculate the inverse of a matrix is it
## does not already exist


## This function accepts a matrix as a parameter
## and creates setter and getter functions
## for the matrix and its inverse.  It returns
## a list containing these functions

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setInv <- function(inverse) inv <<- inverse
  
  getInv <- function() inv
  
  list(set = set, get = get, setInv = setInv, getInv = getInv)
  
}


## This function takes the "makeCacheMatrix" function
## as a parameter. If the inverse has not yet been
## calculated (i.e. the cache is empty), it calculates
## and returns it.  Otherwise, it returns the cached
## inverse.  This avoid unecessary and possibly lengthy
## calculations.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
  
}
