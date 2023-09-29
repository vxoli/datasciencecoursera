## Function to compute and cache the inverse of a matrix.

## makeCacheMatrix contains a function that 
## - sets the value of the matrix
## - gets the values of the matrix
## - sets the inverse matrix
## - gets the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## cacheSolve computes the inverse matrix with makeCacheMatrix. 
## If the inverse exists in cache this is retrieved from cache and computation skipped.
## If the inverse isn't cached, then inverse computed and stored in cache using setmatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
  
}
