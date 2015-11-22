## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # initially nothing to cache; hence set to Null
  cache <- NULL
  
  #storing a matrix
  set <- function(b) {
    x <<- b
    cache <<- NULL
  }
  
  #returning the stored matrix
  get <- function() x
  
  #setting the inverse cached value
  setInverse <- function(inverse) cache <<- inverse
  
  #getting the inverse cached value
  getInverse <- function() cache
  
  #returning the list: note, each element of the list is a function
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  #get the cache value
  cache <- x$getInverse()
  
  #if the cache value exists, returns the value
  if (!is.null(cache)) {
    message("Returning the cached data")
    return(cache)
  }
  
  #when not, get the matrix instead, calculate the inverse and store it in the cache, and returns it
  mat <- x$get()
  cache <- solve(mat, ...)
  x$setInverse(cache)
  cache
}
