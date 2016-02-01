## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## initialize cache
  m <- NULL  
  ## sets the matrix into de special vector
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## reutrns the matrix into de special vector
  get <- function() x 
  ## sets the inverse into de special vector
  setinv <- function(inv) m <<- inv
  ## return the matrix into de special vector
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv) ##return all functions
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$getinv() ##gets the cached inverse
  ## if not empty shows it and warns it is a cached version
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## otherwise
  data <- x$get() ##get the matrix
  m <- solve(data, ...) ##calculate inverse
  x$setinv(m) ##fills the cache
  m ##shows the inverse
}
