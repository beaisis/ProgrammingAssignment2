## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## Assumes Matrix is in invertible.

## This function creates a special "matrix" object that can cache its inverse by
#  creating a list containing a function to
#    set the value of the vector
#    get the value of the vector
#    set the inverse of the vector
#    get the inverse of the vector


makeCacheMatrix <- function(x = matrix()) {
  #cache initialized to Null
    CachedMatrix    <- NULL                  
  
  #In working environ, create the matrix  
  set  <- function(y) {          
              x <<- y
              m <<- NULL
          }

  #get the matrix
  get  <- function() x
  
  #Use inverse to invert the matrix.  Store in cachedmatrix
  setInverseMatrix <- function(inverse) CachedMatrix <<- inverse
  
  getInverseMatrix <- function() CachedMatrix
  
  #functions into working environ
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  CachedMatrix <- x$getInverseMatrix()
  
  ## Is matrix already cached?  If so, return from cache.
  if (!is.null(CachedMatrix)) {
    message("getting cached matrix")
    return(CachedMatrix)
  }
  
  ## If not cached, then get matrix, solve and return (and place in cache)
  CachedMatrix <- solve(matrix, ...)
  x$setInverseMatrix(CachedMatrix)
  CachedMatrix
  
}
