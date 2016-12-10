## The following functions were developed to cache the inverse of a matrix

## A matrix object will be created by the following funcion that can cache 
## the matric inverse

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The following function determines the inverse of the matrix returned by 
## makeCacheMatrix above.

cacheSolve <- function(x, ...) {
inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}
