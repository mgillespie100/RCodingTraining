## makeCacheMatrix allows matrix to be stored in cache
## 



makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv<<- inverse
  getinverse <- function() inv
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
  
}




## cacheSolve returns the inverse of a matrix "x"

cacheinverse <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix_to_invert <- x$get()
  inv <- solve(matrix_to_invert, ...)
  x$setinverse(inv)
  inv
}
