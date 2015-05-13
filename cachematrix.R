## These functions save a matrix and computes its inverse. They are optimized 
## in order to reduce the computational time of obtaining an inverse of a 
## matrix by recycling previous computations.


## makeCacheMatrix is a representation of a matrix build in order to save
## computational time. It returns a list of functions about the matrix:
## set(newMatrix):             sets the value of the matrix to newMatrix
## get():                      gets the matrix
## setInverse(newInverse):     set the inverse of the matrix to newInverse
## getInverse():               gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix())
{
      inverse <- NULL
      set <- function(newMatrix)
      {
            x <<- newMatrix
            inverse <<- NULL
      }
      get <- function(){x}
      setInverse <- function(newInverse) {inverse <<- newInverse}
      getInverse <- function() {inverse}
      list(set=set, get=get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve returns the inverse of a (makeCacheMatrix) matrix. If the matrix 
## has already an inverse, no computation is done. Otherwise, a computation is performed.

cacheSolve <- function(x, ...)
{
      ## Return a matrix that is the inverse of 'x'
      
      inverse <- x$getInverse()
      
      if(!is.null(inverse))
      {
            message("Inverse matrix recovered")
            return(inverse)
      }
      
      matrix <- x$get()
      inverse <- solve(matrix)
      x$setInverse(inverse)
      inverse
      
}
