## Contains function to alow caching of matrix inversion

## creates an object 'cacheMatrix' which can be used by function 'cacheSolve' to solve a a matrix while cahing the result
makeCacheMatrix <- function(m = matrix()) {
  # the cached inversion value get stored here
  inv <- NULL
  
  set <- function(new.matrix) {
    m  <<- new.matrix
    inv <<- NULL
  }
  get <- function() m
  setinv <- function(new.inv) inv <<- new.inv
  getinv <- function() inv

  list(set = set, get = get,setinv = setinv,getinv = getinv)
}

## Solves a matrix and reuses a formeerly cached result, if available
## 'cm' need to be an object returned by function 'makeCacheMatrix'
cacheSolve <- function(cm,...) {
  
  # get result from cache
  inv <- cm$getinv()
  
  # check, if inversion was calculated before
  if(!is.null(inv)) {
    return(inv)
  }
  #calculate value
  m <- cm$get()
  inv <- solve(m,...)
  #cache value
  cm$setinv(inv)
  inv
}
