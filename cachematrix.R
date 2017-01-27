## Script helps to cache the inverse of matrix

## makeCacheMatrix creates a special "matrix" object that can cache its inverse, 
## using function Solve()


makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) inv <<- solve
      getinverse <- function() inv
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## Cachesolve computes the inverse of matrix in the environment of function makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cach with the text message 
##"getting cached data"


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
}
