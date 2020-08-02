## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a makeCacheMatrix object, which includes:
## mat: the matrix provided to the function
## inv: the inverse of the matrix
## set, get: functions that set and get the matrix
## setinv, getinv: functions that set and get the inverse of the stored matrix.

makeCacheMatrix <- function(mat = matrix()) {
  inv <- NULL
  set <- function(newMatrix) {
    mat <<- newMatrix
    inv <<- NULL
  }
  get <- function() mat
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## This function takes as argument a makeCacheMatrix object.
## The function first retrieves the cached value of the inverse of the matrix;
## if this is NULL, then it calculates the inverse and caches it.

cacheSolve <- function(mCMObj, ...) {
  ## Return a matrix that is the inverse of 'mat'
  inv <- mCMObj$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- mCMObj$get()
  inv <- solve(data, ...)
  mCMObj$setinv(inv)
  inv
}
