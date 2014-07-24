# creates a special "matrix", which is really a list containing a function 
# 1. set the value of the vector
# 2. get the value of the vector
# 3. set the value of the mean
# 4. get the value of the mean

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setmatrix <- function(variable) m <<- variable
      getmatrix <- function() m
      list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}


# The following function calculates the mean of the special "matrix" created
# with the makeCacheMatrix function. However, it first checks to see if the
# inverse has already been calculated. If so, it  get s the mean from the cache
# and skips the computation. Otherwise, it calculates the mean of the data and
# sets the value of the mean in the cache via the  setmean  function.

cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
              }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
         m
}
