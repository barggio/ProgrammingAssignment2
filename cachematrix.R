## Title: Week 3 Assignment
## Objectives:  
## Caching the Inverse of a Matrixless 
## Matrix inversion is usually a costly computation and there 
## may be some benefit to caching the inverse of a matrix rather 
## than compute it repeatedly (there are also alternatives to 
## matrix inversion that we will not discuss here). 

## Your assignment is to write a pair of functions that cache the inverse of a matrix.
## Write the following functions:
## 1) makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2) cacheSolve: This function computes the inverse of the special "matrix" 
##                returned by makeCacheMatrix above. If the inverse has already been 
##                calculated (and the matrix has not changed), then the cachesolve 
##                should retrieve the inverse from the cache.
##

## Function:
## variable x  : input
## variable inv: inverse
makeCacheMatrix <- function(x = matrix()) {
      ## Initialization
      inv <- NULL
      
      ## Setting the internal input parameter
      ## Every time matrix is changed, the inv is wiped clean
      ## If the matrices are identical, leave the inverse alone, whether it was already calculated or not
      ## Sometimes, the matrix is just set and inverse is not calculated.
      set <- function(y){
          x <<- y
          if (identical(x,y) == FALSE)
          {
            inv <<- NULL
          }
      }
      
      ## Getting the internal input parameter
      get <- function() x
      
      ## Setting inverse of the specified inverse
      ## This is with the assumption that original Matrix and the inverse matrix are set from within the same function like "cacheSolve"
      ## Other use cases are not covered here.
      setInverse <- function(in_inverse) {
        inv <<- in_inverse
      }
      ## Getting inverse of the specified input parameter
      getInverse <- function() inv

      ## Create list of function to make it accessible from outside
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## Function: Find out if the matrix [x] has "Inverse" already "Solved"
##           If so, retrieve "Cache"
##           If not, compute and store in "Cache"
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)){
        message("getting Cached version of the inverse")
        return(inv)
    }
    ## Calculate the inverse and then store in cache
    input_data <- x$get()
    inv <- solve(input_data, ...)
    x$setInverse(input_data, inv)
    inv 
}

#Unit Test
#--------------------------------------------------------------------------------------
#MatInput = t(matrix(c(3,0,2,2,0,-2,0,1,1),ncol = 3, nrow = 3))
#MatUnitTest = makeCacheMatrix(MatInput)
#cacheSolve(MatUnitTest)
#message("Original Input Matrix")
#MatUnitTest$get()
#message("Inverse Output Matrix")
#MatUnitTest$getInverse()
#message("Multiplication of two matrices is identity matrix (Please round to 3-4 digits :D)")
#MatUnitTest$get() %*% MatUnitTest$getInverse()
#Input
# 3.0   0.0   2.0
# 2.0   0.0  -2.0
# 0.0   1.0   1.0

#Inverse
# 0.2   0.2   0.0
#-0.2   0.3   1.0
# 0.2  -0.3   0.0

#Ref for example: http://www.mathsisfun.com/algebra/matrix-inverse-minors-cofactors-adjugate.html
