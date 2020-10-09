## makeCacheMatrix(matrix)
## This function creates a special matrix which is really a list containing a function to
##   1. set the value of the matrix
##   2. get the value of the matrix
##   3. set the value of the inverse
##   4. get the value of the inverse
##
## cacheSolve(cachedMatrix)
## This function calculates the inverse of the special matrix created with the above function.
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setInverse function.


## This function creates a cacheable "matrix"

makeCacheMatrix <- function(m = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
    m <<- y
    inverseMatrix <<- NULL
  }
  get <- function() m
  setInverse <- function(inverse) inverseMatrix <<- inverse
  getInverse <- function() inverseMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function calculates the inverse of the cacheable matrix

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'm'
  inv <- m$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- m$get()
  inv <- solve(data, ...)
  m$setInverse(inv)
  inv
}
