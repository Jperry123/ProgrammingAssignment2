## These functions store the inverse of a matrix defined in makeCacheMatrix into a cache, so that if that 
## is required again, instead of wasting valuable time it can be retrieved from the cache to save computer calculations.

## makeCacheMatrix is a function that creates a special vector which lists the functions to:
## 1.set the layout of the matrix
## 2.get the layout of the matrix
## 3.set the layout of the inverse
## 4.get the layout of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## The function cacheSolve creates the inverse of the matrix produced with the function above,
## however it first checks in the cache whether this inverse has been made before to remove unnecessary calculations

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

