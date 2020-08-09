## cacheSolve function calculates the inverse of a matrix that is defined in the function makeCacheMatrix
## by fetching it through get() function. This inverse is then set in the function setInverse()
## so anytime same inverse is required, cacheSolve gets the results through the getInverse() function
## in the cache of the program.

## This function creates a vector containing a function to set and 
## get the value of the inverse 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv<<-inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function checks to see if the inverse is calculated.  
## If calculated, it returns the inverse from the cache 
## Or else, it calculates the inverse and then sets the value of inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <-x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
