## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## the function makeCacheMatrix  create a matrix object which has an ability to cache an inverse matrix value

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinv <- function(inv) i <<- inv
      getinv <- function() i
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}




## Write a short comment describing this function
# cacheSolve solves a matrix invertion using solve() or returnes a cached value
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      i <- x$getinv()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinv(i)
      i
}
