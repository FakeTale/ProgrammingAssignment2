## Use those two functions to calculate the inverse of a square invertible matrix
## they store result in the cache, so you can get it from there 
## instead of calculating inverse matrix again

## makeCacheMatrix creates a list of functions to set and 
## then get inverse matrix from cache with cachSolve function:
makeCacheMatrix <- function(x = matrix()) {
  ## initialize an empty variable to store cache
  cache <- NULL
  
  ## set the matrix in the working environment
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  ## get the value of the matrix
  get <- function() x
  ## set the inverse matrix and store it in the cache
  setinverse <- function(solve) cache <<- solve
  ## get the invere matrix from cache 
  getinverse <- function() cash
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## cacheSolve calculates the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated for the same matrix it will be retrieved from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cache <- x$getinverse()
  ## returns inverse matrix from cache if it is already there
  ## if not calculate inverse matrix in the working environment
  if(!is.null(cache)) {
    message('getting cached data')
    return(cache)
  }
  data <- x$get()
  cache <- solve(data, ...)
  x$setinverse(cache)
  cache
}

