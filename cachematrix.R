## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# cache matrix makes a cached matrix object that can potentially save
# some computation time

makeCacheMatrix <- function( x = matrix()){
  #upon creation the variable inv is set to NULL
  #inv is the actual inverted matrix
  inv <- NULL
  
  #teh set function sets the values of the input matrix to be inverted and 
  # clears the inverted matix, since we have not yet inverted this new matrix
  # x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  #get, function that just returns x, the raw input matrix
  get <- function() x
  # setinverse, a function that assigns a value to teh inv variable, should be the inversion of x
  setinverse <- function(inverse) inv <<- inverse
  #get the inverse, simply returns teh inverted matrix 
  getinverse <- function() inv
  #a list, this is whats returned by makeCacheMatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## this function computes teh inverse of x, but it also checks to see if it was alread
## computed, if it was it just uses a cached value instead of calling solve(x)
cacheSolve <- function(x, ...) {
  #get the inverse
  inv <- x$getinverse()
  #if it is null that means it was not yet calculated
  #but if it is not null then we can use the cached value instead of computing it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  #so if we need to calculate it we will still be in this function,
  #first get the non inverted matrix data
  data <- x$get()
  #and then compute the inverse
  inv <- solve(data, ...)
  #and set that in the object
  x$setinverse(inv)
  #and now we still want to return the inverse so 
  inv
}