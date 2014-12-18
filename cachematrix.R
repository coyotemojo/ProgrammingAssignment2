## These functions create a special object when passed a matrix.  That object
## has methods that can be used to change the matrix values, get the matrix
## values, get the inverse if it is has been calculated and set the inverse
## on the matrix


## This function (basically a copy of the provided example) takes a matrix object
## and defines 4 methods for manipulating that object

makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL
  set <- function(y) {
    x <<- y
    x_inv <- NULL
  }
  get <- function() x
  setinv <- function(inv) x_inv <<- inv
  getinv <- function() x_inv
  list(set = set, get = get, setinv = setinv, getinv=getinv)
}

## This function checks to see if the object's inverse has already been calculated,
## and if it has it just returns that cacheed result.  If the inverse has not been
## calculated, it will use the methods defined for the object to caculate the 
## inverse and set the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message('getting cached inverse')
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinv(inv)
  inv
}
