## makeCacheMatrix creates a special matrix that can cache its inverse in a variable called inv
## It also provides nested functions to get and set the value of the cached variable

## set() 
##  (1) intializes a matrix by taking an argument 'y' and coercing it into a matrix 'x'
##  (2) sets the inv of x to null
## get() returns x
## setinverse() populates inv with the value supplied in its argument 'inverse'
## getinverse() returns inv



makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv

  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## The purpose of cacheSolve is to invert a matrix provided as an argument 
## However, because we cache inverted matrices to preserve system resources,
## cacheSolve first calls getinverse() to see if we've got the values we want already
## If a value is present it is returned
## otherwise cacheSolve inverts the matrix and adds it to a cached value

cacheSolve <- function(x, ...) {
        
  ## Return a matrix that is the inverse of 'x'

  # Please note that the funxtion calls below that are prefixed with 'x$' are nested functions in makeCacheMatrix 
  # This applies to the following functions called below:
  # - getinverse()
  # - get()
  # - setinverse(inv)
  
  # Check for a cached value
  inv <- x$getinverse()
  
  # If a cahced value is found: 
  if(!is.null(inv)) {
    # (1) Notify the console the cached inverse is being returned
    message("getting cached inverse")
    # (2) Return it
    return(inv)
  }
  
  # Otherwise:
  # (1) Retrieve the matrix using makeCacheMatrix's get()
  data <- x$get()
  # (2) Invert the matrix
  inv <- solve(data)
  # (3) Set the cache value for the inverted matrix 
  x$setinverse(inv)
  
  inv

}
