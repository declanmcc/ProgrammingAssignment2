## Put comments here that give an overall description of what your
## functions do

# This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # set changes the 'matrix' stored
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # get  returns the 'matrix' x
  get <- function() x
  # setinverse stores the value of the input in the variable m
  setinverse <- function(solve) m <<- solve
  # gettinverse returns the value of the input in the variable m
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# This function computes the inverse of the "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated, then the cache value will be returned
cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  # Check if we have previously cached data
  m <- x$getinverse()
  # if yes (not null) then return the cached value
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # if not the calculate the inverse 'matrix'
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
