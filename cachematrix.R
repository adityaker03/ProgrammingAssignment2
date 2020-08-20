## This function will cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inve <- NULL
  set <- function(y) {
    x <<- y
    inve <<- NULL
  }
  get <- function() {x}
  setinverse <- function(inverse) {inve <<- inverse}
  getinverse <- function() {inve}
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function will return the inverse of matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inve <- x$getinverse()
  if(!is.null(inve)) {
    message("getting cached data")
    return(inve)
  }
  matr <- x$get()
  inve <- solve(matr,...)
  x$setinverse(inve)
  inve
  }
