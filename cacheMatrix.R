# Caching MATRIX 

makeCacheMatrix <- function(x = matrix()) {

  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## x has to be an invertible matrix
## We get the following from the code:
## first, we set the matrix (by making a new function)
## second, we get the function
## third, we set the inverse
## finally, we get the inverse
## I refer to the final output of this function as matrix x's storage info (so we can retrieve information later)
## then we can use this to cacheSolve()


cacheSolve <- function(x, ...) {
  inv = x$getinv()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  matrix_data = x$get()
  inv = solve(matrix_data, ...)
  x$setinv(inv)
  return(inv)
}

## x: whatever we get from the makeCacheMatrix(). Or we try to solve using "matrix x's storage info"
## if we already have the inverse, we then
## obtain from the "storage" (cache) and skips the computation. 
## if not then proceed to obtain the inverse matrix
## and then set the value obtained of the inverse in the cache  (setinv function)
## return(...) gives us the inverse of the original matrix x


# test:
install.packages("matlib")
library(matlib)
x <-matrix(c(3,2,0,0,0,1,2,-2,1),ncol=3,nrow=3)
x_make<-makeCacheMatrix(x)
x_inverse<-cacheSolve(x_make)
x_inverse
# revert back (caching the inverse matrix -> will get back to the original matrix )
x_inverse_inverse<-makeCacheMatrix(x_inverse)
cacheSolve(x_inverse_inverse)

## rationale: this proccess can be very time-efficient if we try to obtain 
## inverse of a very big matrix
