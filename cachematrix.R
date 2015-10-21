## R script for caching inverse matrix 

## makeCacheMatrix has list of 4 functions. 
## Given: Matrix is always invertible. A %*% INV(A) gives Identical matrix 
## Initial set of matrix makes, inverse property to NULL


makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve uses list to see if inverse already exist of matrix (not NULL)
## if inverse is computed before, we simply return inverse matrix without recomputing 
## if not, inverse is calculated and stored for future use


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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
