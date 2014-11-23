## This file and functions will evaluate and calculate a matrix and its inverse.
## It is developed in a way that it will calculate the inverse matrix only once.
## This is a great example of saving time by restoring values in cache.


## This function will get the values of a matrix and store it in cache. It is time efficient.
## 

makeCacheMatrix <- function(x = matrix()) {
  rev <- NULL
  set <- function(y) {
    x <<- y
    rev <<- NULL
  }
  get <- function() x
  setrev <- function(f) rev <<- f
  getrev <- function() rev
  list(set = set, get = get,
       setrev = setrev,
       getrev = getrev)
}



## This function return the inverse matrix of the given matrix.
## The calculation is done if inverse matrix doesn't exist.

cacheSolve <- function(x, ...) {
  m <- x$getrev()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)  ## Return a matrix that is the inverse of 'x'
  x$setrev(m)
  m      
 
}
