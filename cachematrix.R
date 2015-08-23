# The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
# The function creates a list that has a function to perform setting the value of a matrix and 
# getting the value of a matrix, as well as setting the value of the inverse of a matrix and getting
# the value of the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  myInverse <- NULL

  setIt <- function(y) {
    x <<- y
    myInverse <<- NULL
  }

  getIt <- function() x
  setInverse <- function(inverse) myInverse <<- inverse
  getInverse <- function() myInverse

  #Creates a list
  list(setIt=setIt, getIt=getIt, setInverse=setInverse, getInverse=getInverse)
}


# The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# The function first checks to see if the inverse has already been calculated, or not. If the 
# inverse has already been calculated (and the matrix has not changed), then cacheSolve should 
# retrieve the inverse from the cache. I'm also assuming that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  myInverse <- x$getInverse()

  if(!is.null(myInverse)) {
    message("Hey! I'm getting cached data, cool? Cool!")
    return(myInverse)
  }

  data <- x$getIt()
  myInverse <- solve(data)
  x$setInverse(myInverse)

  myInverse
}
