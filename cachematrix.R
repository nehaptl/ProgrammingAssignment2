## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  invx <- NULL
  set <- function(y) {
    x <<- y
    invx <<- NULL
  }
  get <- function() x
  setinvx <- function(inverse) invx <<- inverse
  getinvx <- function() invx
  list(set = set, get = get,
       setinvx = setinvx,
       getinvx = getinvx)

}


## Write a short comment describing this function
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
       
  invx <- x$getinvx()
  
  ## Return a matrix that is the inverse of 'x'
  
  if(!is.null(invx)) {
    message("getting cached data")
    return(invx)
  }
  data <- x$get()
  invx <- solve(data, ...)
  x$setinvx(invx)
  invx
}


B <- matrix(c(10:13),2,2)

B1 <- makeCacheMatrix(B)
cacheSolve(B1)
