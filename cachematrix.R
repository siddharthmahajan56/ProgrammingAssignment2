
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(a, ...) {
  b<-makeCacheMatrix(a)
  i <- b$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- b$get()
## Return a matrix that is the inverse of 'x'
  i <- solve(data)
  b$setInverse(i)
  i
}
