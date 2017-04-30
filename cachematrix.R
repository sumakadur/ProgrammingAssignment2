## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function provides functions to
## - set the cache for the matrix
## - get the cached matriz
## - set the inverse of the matrix
## - get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## set the   matrix.  x and m are in the parent scope 
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  
  ## get the matrix 
  get <- function() x
  
  ## Set the inverse of the matrix in cache
  setinverse <- function(inverse) m <<- inverse
  
  ## get the cached inverse 
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cachSolve creates the inverse of the matrix x in cache using the funtions in makeCacheMatrix.
## if the data already exists, it returns the same

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  ## check if the inverse of the matrix already exists
  
  m <- x$getinverse()
  
  if(!is.null(m)) {
    message("getting already cached matrix")
    return(m)
  }
  
  ## get the matrix
  data <- x$get()
  
  ## create the inverse matrix
  m <- solve(data, ...)
  
  ## set the cache with inverse data
  x$setinverse(m)
  m
}
