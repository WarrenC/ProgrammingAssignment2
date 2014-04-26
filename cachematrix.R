#Two functions that cache the inverse of a matrix

## The function below creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setSolve <- function(Solve) s <<- Solve
  getSolve <- function() s
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}

## The function below computes the inverse of a matrix

cacheSolve <- function(x, ...) {
  s <- x$getSolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- Solve(data, ...)
  x$setSolve(s)
  s
  ## Return a matrix that is the inverse of 'x'
}
