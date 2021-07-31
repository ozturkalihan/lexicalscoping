## These two functions will compute and cache the inverse of
## a matrix

## This function creates a special "matrix" object 
## and then, it is able to cache its inverse.

makeCacheMatrix <- function(x = matrix()){
  s <- NULL
  set <- function(y){
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) s <<- solve
  getinverse <- function() s
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse, 
## if it is already calculated, retrieves it back from the cache.

cacheSolve <- function(x, ...) {
  s <- x$getinverse()
  if(!is.null(s)){
    message("cached data available")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
}