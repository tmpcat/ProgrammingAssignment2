## This program will compute, cache, and retrieve the inverse of a matrix

## This function will create a matrix to set and get the value of the matrix and set and get the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  # set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # get the value of the matrix
  get <- function() x
  # set the value of the inverse
  setinverse <- function(inverse) inv <<- inverse
  # get the value of the inverse
  getinverse <- function() inv
  # return:
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Checks if inverse of matrix has already been calculated and if so, gets that cached value. If not, it calculates the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse() # function() inv
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv) # return ends the function
  }
  data <- x$get() # function() x
  inv <- solve(data, ...) # calculates the inverse and stores it as inv
  x$setinverse(inv) # function(inverse) inv <<- inverse
  inv
}