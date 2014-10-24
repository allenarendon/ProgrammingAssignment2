## This programming assignment primarily is geared to take advantage of caching results 
## through the use of scoping rules of the R Language. The first function (makeCacheMatrix)
## handles the basic operations of setting and storing the input matrix and its inverse.
## The second function (cacheSolve) is designed for computing the inverse of the matrix
## if its not yet computed and stored in cache. An extra function (toSquareMatrix) has
## added to simplify the creation of a square matrix from a series of numeric values.


## The makeCacheMatrix function serves 4 special purposes:
## (1) get the elements of the cached matrix
## (2) set the elements of the matrix to be inversed
## (3) get the inverse of the matrix, if already stored in cache
## (4) directly set the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL                     ## initialize matrix inverse to null
     set <- function(y) {          ## assign new values for input matrix
          x <<- y
          i <<- NULL
     }
     get <- function() x           ## return the input matrix
     setinv <- function(inv) i <<- inv  ## assign new values for matrix inverse
     getinv <- function() i        ## return the matrix inverse
     list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## The cacheSolve function returns the inverse of an input matrix. However, if the
## inverse of a matrix has already been previously computed and stored into the cache,
## it will return instead the cached matrix inverse rather than compute it again.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     i <- x$getinv()               ## fetch the matrix inverse
     if(!is.null(i)) {             ## check if matrix inverse is available 
          message("retrieving cached inverse matrix data")
          return(i)
     }
     d <- x$get()                  ## fetch the input matrix
     i <- solve (d, ...)           ## compute for matrix inverse
     x$setinv(i)                   ## set the inverse matrix
     return(i)
}


## The toSquareMatrix function takes a set of numeric values and turns it to a square 
## matrix, if it has the appropriate number of values. Otherwise, it would return an
## error message stating that the number of elements is unappropriate to form a 
## square matrix.

toSquareMatrix <- function(z = numeric()) {
     ncolrow = sqrt(length(z))     ## get dimension of square matrix
     if(!ncolrow%%1==0) {          ## check if dimension is a whole number
          return(message("Error: The number of elements cannot form a square matrix.."))
     }
     return(matrix(z, nrow=ncolrow, ncol=ncolrow, byrow=TRUE))
}
