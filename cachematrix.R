## cachematrix function has two functions inside. First, makeCacheMatrix makes the
## matrix and stores in cache. Then cacheSolve checks wheather m is NULL or not.
## Then it returns an inverse matrix.
 

## makeCacheMatrix creates a matrix and then stores it in cache.

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


## cacheSolve computes the inverse matrice. Before computing it checkes whether
## the inverse matrice has already been calcultated (it means m is not NULL). If it is, 
## the function returns m. If not, it uses solve function to compute the inverse matrix 
## then also returns m.

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
