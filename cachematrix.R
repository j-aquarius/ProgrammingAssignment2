## If matrix is big,  it may take too long to compute value of inverse of matrix
## value of inverse can be computed into cache
## when we need value of inverse again, it can be looked up in the cache

## This function creates a list with functions, that
##sets the value of the matrix
##gets the value of the matrix
##sets the value of the inverse of matrix
#gets the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
i <- NULL
  set <- function(y) {
          x <<- y
          i <<- NULL
  }
  get <- function() x
        setinverse <- function(inverse) i <<- inverse 
        getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#№Function returns the inversed matrix

cacheSolve <- function(x, ...) {
i <- x$getinverse()
  ##it cheks if data already was inveresed (if it was already computed). 
  ##If yes-it takes this data 
  if (!is.null(i)) {
          message("getting cached data")
          return(i)
  } 
 ##if not- it computes the inverse and sets the value in the cache
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
