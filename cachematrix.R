## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  # Set value of the vector
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  # Get the value of the vector
  get <- function() x
  # Set the value of the mean
  setinv <- function(inv) i <<- inv
  # Get the value of the mean
  getinv <- function() i
  list(set = set, get = get,setinv = setinv,getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  # Check if the inverse has already been calculated
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  # Calculates inverse 
  i <- solve(data, ...)
  # Sets the value of the inverse in the cache via
  x$setinv(i)
  i
}
