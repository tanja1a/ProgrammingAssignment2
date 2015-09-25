## if "a" is invertible matrix then cacheSolve(makeCacheMatrix(a)) returns inverse matrix
## either from cash or if it doesn't exist in cash it calculates inverse matrix and cashes it before returning it

## function makeCacheMatrix creates an object that can cache inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## function cacheSolve returns inverse matrix
##its first parameter is the object returned by makeCacheMatrix function
##If the inverse has already been calculated for the given matrix then the function retrieves the inverse from the cache
##If the inverse has not been previously calculated then it is calculated and cashed

cacheSolve <- 
  function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
