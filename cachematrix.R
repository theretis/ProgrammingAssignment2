## Assignment: caching the inverse of a matrix
## Functions that cache the inverse of a matrix

## example usage:
## inverseMatrix <- makeCacheMatrix()
## inverseMatrix$set(matrix(1:4,2,2))
## cacheSolve(inverseMatrix)
## - any subsequent calls to cacheSolve(inverseMatrix) will be returned the cached 
## inverse matrix

## makeCacheMatrix
## Creates a special "matrix" object that can cache its inverse
## using 'solve'
## @set - set the value of the vector
## @get - get the value of the vector
## @setMatrix - set the inverse matrix m
## @getMatrix - get the inverse matrix m
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setMatrix <- function(solve) m <<- solve
  getMatrix <- function() m
  list(set = set, get = get,
       setMatrix = setMatrix,
       getMatrix = getMatrix)
}

## cacheSolve
## computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not 
## changed), then cacheSolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setMatrix(m)
  m
}
