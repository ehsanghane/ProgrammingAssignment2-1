# creates a special "matrix", which is really a list containing a function 
# 1. set the value of the vector
# 2. get the value of the vector
# 3. set the value of the solve
# 4. get the value of the solve

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(variable) m <<- variable
      getinverse <- function() m
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# The following function calculates the inverse of the special "matrix" created
# with the makeCacheMatrix function. However, it first checks to see if the
# inverse has already been calculated. If so, it  gets the inverse from the cache
# and skips the computation. Otherwise, it calculates the inverse of the data and
# sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
