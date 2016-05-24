## May 24th, 2016 - R Programming Assignment 2
## Functions take an invertible matrix and cache the inverse
## per instructions, the function assumes the matrix is solvable.

#makeCacheMatrix creates a list that contains functions to set and get a solvable matrix (the set and get calls) and 
  # solve the matrix using the setinv and getinv functions, that solve the inverse of the matrix 
  # and return the solved matrix (getinv)
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

#cacheSolve checks to see if the inverse matrix has already been solved and is sitting in the cache
  # if it is in the cache, it returns the solution, otherwise it solves the inverse, returns it and puts it in the cache

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
