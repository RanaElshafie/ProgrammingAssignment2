## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  cacheM <- NULL
  setM <- function(y) {
    x <<- y
    cacheM <<- NULL
    getM <- function() x
    setCache <- function(inverse) cacheM <<- inverse
    getCache <- function() cacheM
    list(setM = setM,
         getM = getM,
         setCache = setCache,
         getCache = getCache)
    
  }

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cacheM <- x$getCache()
  if (!is.null(cacheM)) {
    message("load cache matrix")
    return(cacheM)
  }
  else {
    dMatrix <- x$getM()
    cacheMatrix <- solve(dMatrix, ...)
    x$setCache(cacheM)
    return(cacheM)
  }
}
