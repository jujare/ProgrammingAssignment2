## makeCacheMatrix function creates a special matrix object that  
## can cache its inverse. cacheSolve function uses the special  
## matrix object created by makeCacheMatrix to cache a computed   
## inverse, or to retrieve it if the inverse is already computed.

## This function takes a matrix as an argument and returns a list
## of functions that provide the functionality of a special 
## matrix object

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function takes the special matrix object created by 
## makeCacheMatrix function and returns the inverse of the 
## matrix. If a cache of inverse already exists, the inverse
## is returned immediately. And if the inverse does not exist,
## inverse is calculated, cached in the object before returning
## it.

cacheSolve <- function(x, ...) {
  
  inv <- x$getinv()
  
  if (!is.null(inv)) {
      message("fetching cached data")
      return(inv)       
  }
  
  inv <- solve(x$get())
  x$setinv(inv)
  
  ## Return a matrix that is the inverse of 'x'
  inv
}
