## Put comments here that give an overall description of what your
## functions do

## Compute the inverse of a matrix and cache the result.

## Write a short comment describing this function

## The makeCacheMatrix function sets and gets the value of the matrix and the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                        ## Initialize inverse of the matrix (m) as NULL
  set <- function(y) {
    x <<- y                        ## Set x to value of input matrix in makeCacheMatrix  
    m <<- NULL                     ## Set m (inverse of matrix) to NULL   
  }
  get <- function() x              ## get matrix x
  setinverse <- function(solve) m <<- solve  ## Compute inverse of matrix in environment
  getinverse <- function() m       ## get m (inverse of matrix )   
  list(set = set, get = get,       ## return the list of values from makeCacheMatrix
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

## The cacheSolv function computes the inverse of the matrix created with the above function.
## It first checks to see if the inverse of the matrix is already computed. 
## If already computed, it gets the inverse from the cache and skips the computation. 
## Else, it calculates the inverse and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()      ## If inverse of matrix 'x' already exists then assign to m
  ## if m is not null, i.e. inverse of matrix exists, then show message 'getting from cached data' 
  ## and return m and exit the cacheSolve function with return(m).
  if(!is.null(m)) {    
    message("getting cached data")
    return(m)
  }
  
  ##If m is null, i.e. inverse does not exist (cached), then control passes to the code below 
  data <- x$get()         ## Assign matrix to data
  m <- solve(data, ...)   ## Compute inverse of matrix with SOLVE function and assign to m
  x$setinverse(m)         ## Set inverse of matrix x to m
  m                       ## Return m (inverse of matrix) as result of this function.
}
