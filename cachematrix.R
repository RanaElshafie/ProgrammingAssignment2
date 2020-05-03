## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# create makeCacheMatrix function with argument x 
makeCacheMatrix <- function(x = matrix()) {

  # create cache matrix from passed matrix
  # 1. initialize the cache Matrix (cacheM) and assign Null value for it
 
  cacheM <- NULL
 #  function to set Matrix (setM)
  setM <- function(y) {
    x <<- y
    cacheM <<- NULL
  }
  # create function getM to return function x
    getM <- function() x
    # create function  to set the cached inverse of 'x'
    setCache <- function(inverse) cacheM <<- inverse
    # create function  to return the cached inverse of 'x'
    getCache <- function() cacheM
    # 6. list the names of all function that will be known to the outside world
    list(setM = setM,
         getM = getM,
         setCache = setCache,
         getCache = getCache)
    
  }




# 'cacheSolve return the inverse of a given matrix utilizing the cache

cacheSolve <- function(x, ...) {
  #  check  cache matrix
  cacheM <- x$getCache()
  #  if the cacheM is not null then: return the result 
  if (!is.null(cacheM)) {
    message("load cache matrix")
    return(cacheM)
  }
  #  if the cacheM is empty then: 
  # get the matrix, create, set, update and return the cache matrix
 
  else {
    dMatrix <- x$getM()
    cacheMatrix <- solve(dMatrix, ...)
    x$setCache(cacheM)
    return(cacheM)
  }
}
