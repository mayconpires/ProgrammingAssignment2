## This scripts receive a matrix and do computation of inverse matrix
## with the function solve, after that add the matrix in the cache,
## the next call to function computation the inverse matrix will check if
## the value stay in memory and not computation the matrix again.

## How to use the function:
##
## create a vector and matrix
## set.seed(350)
## randonvector <- runif(9)
## matrixtocomputation <- matrix(randonvector, 3, 3)
##
## call the makeCacheMatrix to create functions to do cache 
## cacheMatrix <- makeCacheMatrix(matrixtocomputation)
##
## First execution of cacheSolve computation inverse matrix
## cacheSolve(cacheMatrix)
##
## Second execution of cacheSolve take matrix of the cache
## cacheSolve(cacheMatrix)

## This function create four function get, set, getinversematrix, 
## setinversematrix for manage the cache.

makeCacheMatrix <- function(x = matrix()) {

	## This variable store of the inverse matrix 
  m <- NULL
  
  ## Store the value of matrix parameter
  set <- function(y) {
    x <<- y
    m <<- NULL
  } 
  
  ## Return the value of matrix parameter 
  get <- function() x 
  
  ## Store the Solve value of inverse Matrix
  setinversematrix <- function(inversematrix) m <<- inversematrix 
  
  ## Return the Solve value of inverse Matrix
  getinversematrix <- function() m 
  
  ## Store the variables in a List
  list(set = set, 
       get = get,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix) 
    
}


## This function inverse the Matrix with function solve

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## Get the value of inverse matrix of cache
  m <- x$getinversematrix()
  
  ## Check if the inverse matrix of cache has value, case has value return the value of cache
  if(!is.null(m)) {
    message("getting cached inverse matrix")
    return(m)
  }
  
  ## Get the matrix 
  data <- x$get()
  
  ## Inverse the matrix
  m <- solve(data)
  
  ## Store in cache the inverse matrix
  x$setinversematrix(m)
  
  ## Return the inverse matrix
  m
  
}
