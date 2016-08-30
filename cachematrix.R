## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##The makeCacheMatrix function creates a special type of "matrix" that contains a list of four functions and two objects
##The functions inside are the setters and gtters used to save and load the cache (the matrix used and its inverse)

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) m <<- solve
      getsolve <- function() m
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}


## Write a short comment describing this function

##The cacheSolve function takes the "matrix" created by makeCacheMatrix and either:
##1. Loads the cached solution/inverse of the matrix (if the values haven't changed) from the m object and returns it
##2. Or it calculates the inverse of the matrix (as it could not find it in the cache) and it caches the solution in the object m 
##for future uses

cacheSolve <- function(x, ...) {
      m <- x$getsolve()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      m
        ## Return a matrix that is the inverse of 'x'
}
