## Functions demonstrating first-class functions and caching of calculated items

## makeCacheMatrix - accepts a matrix as input, returns a list of available functions
makeCacheMatrix <- function(x = matrix()) {
  ## Initialise m as null
  m <- NULL
  ## set function clears cache
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## returns original matrix
  get <- function() x
  ## caches inverted matrix
  setInverse <- function(invertedmatrix) m <<- invertedmatrix
  ## returns inverted matrix
  getInverse <- function() m
  ## list of functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve - client function for makeCacheMatrix, returns solved matrix from cache if available
cacheSolve <- function(x, ...) {
  ## Retrieve the inverted matrix if it is already calculated
  m <- x$getInverse()
  ## If the inverted matrix is cached, print confirmation and return it
  if(!is.null(m)) {   
    message("getting cached data")
    return(m)
  }
  ## Subsequent code is only executed if result is not in the cache
  ## Get the input matrix 
  data <- x$get()
  ## Solve the matrix
  m <- solve(data, ...)
  ## Cache the results of the solved matrix
  x$setInverse(m)
  m
}
