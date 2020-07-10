##  This functions Caching and then calculates the Inverse of a Matrix

## This first function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  mat_inv <- NULL   #varible for the answer
  set <- function(y) { #function that set the function to caches
    x <<- y
    mat_inv <<- NULL   #cache of this variable mat_inv
  }
  get <- function() x #function that get the function
  set_inv <- function(solve) mat_inv <<- solve  #function that solves the inverse
  ## solve function computes the inverse of the matrix
  get_inv <- function() mat_inv  #function that return the list of functions from makeCacheMatrix
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  mat_inv <- x$get_inv()
  if(!is.null(mat_inv)) {
    message("getting cached data")
    return(mat_inv)
  }
  data <- x$get()
  mat_inv <- solve(data, ...)
  x$set_inv(mat_inv)
  mat_inv
}
