## Put comments here that give an overall description of what your
## functions do

## Function that has setters and getters for both the matrix, and its inverse 

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL 
  
  set <- function(y) {
    x <<- y 
    m <<- NULL 
  } 
  
  get <- function() x 
  
  setInverse <- function(inverseMatrix) inverse <<- inverseMatrix 
  
  getInverse <- function() inverse 
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Function that either retrieves the inverse from the inputted matrix, or employs R's 'solve()' function to determine x's inverse 

cacheSolve <- function(x, ...) { 
  i <- x$getInverse() 
  
  if(!is.null(i)) {
    message("Getting cached data") 
    
    return(i) 
  } 
  
  data <- x$get() 
 
  i <- solve(data, ...) 
  
  x$setInverse(i) 
  
  i 
        ## Return a matrix that is the inverse of 'x'
}
