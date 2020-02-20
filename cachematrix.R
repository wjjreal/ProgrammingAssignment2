## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix`: This function creates a special "matrix" object that can cache its inverse.
## get(): return value of the matrix 
## set(): set value of the matrix
## getinverse(): get the inverse of the matrix
## setinverse(): set the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    x_inv <- NULL
    set <- function(y) {
      x <<- y
      x_inv <<- NULL
    }
    get <- function() x
    setinverse <- function(i) x_inv <<- i
    getinverse <- function() x_inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## generate inverse of the matrix if inverse is null
cacheSolve <- function(x, ...) {
  x_inv <- x$getinverse()
  if(!is.null(x_inv)) {
    message("getting cached data")
    return(x_inv)
  }
  data <- x$get()
  x_inv <- solve(data)
  x$setinverse(x_inv)
  x_inv
## Return a matrix that is the inverse of 'x'
}
