#makeCacheMatrix is a function that creates a matrix object which can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL 
  }
  get <- function() x 
  setMatrix <-function(inv) inverse <<- inv
  getMatrix <-function() inverse
  list(set = set, get = get, setMatrix = setMatrix, getMatrix = getMatrix)
}

#cacheSolve is a function that creates the inverse of the matrix created by the makeCacheMatrix above
#If the inverse of the matrix is not the same as the original matrix, it will acquire the inverse from the cache

cacheSolve <- function(x, ... ) {
  inverse <- x$getMatrix()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <-inv(data, ...)
  x$setMatrix(inverse)
  inverse
}
  

