
## The following pair of R functions is used to cache potentially time-consuming computations, in this case matrix inversion.

## The below funtion makeCacheMatrix receives a matrix x as an argument and returns a special matrix object that contains the follwoing functions:
# set:  set the value of the matrix
# get: get the value of the matrix
# setinv: set the value of the matrix inverse
# getinv: get the value of the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function () x
  setinv <- function (inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The below function cacheSolve calculates the inverse of the matrix, returned by makeCacheMatrix above, only if it had not been calculated before.
## If it had already been calculated, it will receive the precalculated value from the cache.
cacheSolve <- function(x, ...) {

        m <- x$getinv()
        if(!is.null(m)){
            message("Getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinv(m)
        m
}
