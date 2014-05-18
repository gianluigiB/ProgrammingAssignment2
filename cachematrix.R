## Programming Assignment 2
## Caching the Inverse of a Matrix
## Course: R Programming
## Johns Hopkins Public School of Health via Coursera
## G. Bardelloni (c)

## These two functions calculate and cache the inverse of a matrix
## See section 10.7 Scope of "An Introduction to R" in the RStudio Help
## to learn about lexical scoping of free variables and the <<- superassignment operator 
## See also section 10.4 to learn about the '...' argument for a function and realize
## we do not need to use it as in the cachemean() example.

## Usage:
## 1. Pass a matrix and cache it by passing it to the makeCacheMatrix function
## > myCachedMatrix <- makeCacheMatrix(matrix(c(0, -5, -4, 0), nrow=2, ncol=2, byrow=TRUE))
## 2. Verify that the matrix X is available in the myCachedMatrix environment 
##    but not in the global environment:
## > myCachedMatrix$getMatrix()         => you will see a the matrix passed in 1.
## > X                                  => you will get Error object 'X' not found, or something else
## 4. Verify that the inverse of X is not yet calculated
## > myCachedMatrix$getInverseMatrix()  => you will get NULL 
##  Calculate and get its inverse with the cacheSolve function
## >  cacheSolve(myCachedMatrix)        => you will get the inverse matrix of X
## 5. Verify the content of the cache
## > myCachedMatrix$getInverseMatrix()  => you will get the very same inverse matrix of X
## 6. Verify that the matrix product of X and its inverse returns the identity matrix
## > myCachedMatrix$getMatrix() %*% myCachedMatrix$getInverseMatrix()
## 7. Invoke again cacheSolve to verify Inverse matrix is coming from the cache 
##    and it is not calculated
## > cacheSolve(myCachedMatrix)         => you will get the very same inverse matrix of X
## 8. Set a new matrix and verify the cache for the inverse matrix has been cleared.
## > myCachedMatrix$setMatrix(matrix(c(0, -3, -2, 0), nrow=2, ncol=2, byrow=TRUE))
## > myCachedMatrix$getMatrix()         => you will get the new matrix
## > myCachedMatrix$getInverseMatrix()  => you will get NULL
## > cacheSolve(myCachedMatrix)         => you will get another inverse matrix, because the matrix has changed


## This function actually creates a cache object with two variables X and invX and 4 functions.
## The 4 functions are embedded in a list and they:
## 1. set a matrix (matrix passed as formal variable)
## 2. get a matrix (returns matrix as set by function 1)
## 3. set the inverse of a matrix (inverse matrix passed as formal variable)
## 4. get the inverse of a matrix (returns inverse matrix as set by function 3)
makeCacheMatrix <- function(X = matrix()) {
  #allocate the cache for the inverse matrix invX (local variable)
  invX <- NULL
  #create a list with the functions 
  list(
    
    #sets the matrix passed as argument  
    setMatrix = function(Y) {
      X <<- Y #makes X (local variable) available also to the function calling it (i.e. makeCacheMatrix)
      invX <<- NULL #clears the cache for the inverse matrix and makes it available to function calling it (i.e. makeCacheMatrix)
    },
    
    #returns the matrix X which is assigned either by calling makeCacheMatrix(<aMAtrix>) or by calling setMatrix(<aMatrix>)
    getMatrix = function() {
      X #assigned via lexical scoping, either by calling makeCacheMatrix(<aMAtrix>) or by calling setMatrix(<aMatrix>)
    },
    
    #sets the matrix passed as argument 
    setInverseMatrix = function(invY) {
      invX <<- invY #makes invX available also to the function calling it (i.e. makeCacheMatrix)
    },
    
    #returns the inverse matrix invX, 
    getInverseMatrix = function() {
      invX #assigned via lexixal scoping, thus NULL if setMatrix has been invoked and until setInverseMatrix is NOT invoked
    }
    
  )
}
## If you in the functions above change the superassignement operators <<- with <- then (from top to bottom):
## 1. the matrix you set with setMatrix WON'T be the same you get with getMatrix
## 2. the cached inverse Matrix invX WON'T be cleared if you change matrix with setMatrix
## 3. the inverse matrix you set with setMatrix WON'T be the same you get with getMatrix

## This funtion creates the inverse of a matrix (the formal variable x) 
## if and only if 
##  - the matrix is the same as the one in the cache and
##  - its inverse matrix is not yet available in the cache. 
## In case inverse calculation is done, the cache is populated as well
cacheSolve <- function(x) {

  # get first the inverse matrix from the cachedMatrix object x
  invX <- x$getInverseMatrix()
  
  #in case it is not still NULL get the inverse matrix from the cachedMAtrix object
  if(!is.null(invX)) {
    cat("Getting cached inverse matrix...\n\n")
    
    #print it on console and return to calling funciton
    return(invX)
  }
  
  #otherwise let's calculate it for the matrix stored inthe cachedMatrix
  invX <- solve(x$getMatrix())
  cat("Caching inverse matrix...\n\n")
  
  #store the calculation in the cachedMatrix object 
  x$setInverseMatrix(invX)
  
  #print it on the console
  invX
}
