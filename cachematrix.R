## Put comments here that give an overall description of what your
## functions do

## Creates an object containing a matrix and it's inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## Create getter and setter functions for the initial matrix
  set <- function(y) {
    x <<- y
    
    ## clear inv so that old calculated values won't be return in future calls
    inv <<- NULL
  }
  get <- function() x
  
  ## Create getter and setter functions for the matrix inverse
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  ## return an "object" with the appropriate getter and setter functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Returns the cached matrix inverse in the object created in the makeCacheMatrix function if it exists.
## Calculates and caches the matrix inverse if one doesn't exists.  Then returns that value.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## Get cached inverse
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    ## If inverse already exists return the inverse
    return(inv)
  }
  
  ## If inverse does not already exist it must be calculated
  
  ## Extract main matrix
  data <- x$get()
  
  ## Solve to get the matrix inverse
  inv <- solve(data, ...)
  
  ## Cache inverse for future lookups
  x$setinverse(inv)
  
  ## Return the matrix inverse stored in inv
  inv
}
