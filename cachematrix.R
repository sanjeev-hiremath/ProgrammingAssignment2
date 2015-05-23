## This function creates a special "matrix" object
## that can cache its inverse.

## uaes sub functions set, get, setMatrix and getMatrix for matrix and its inverse

makeCacheMatrix<- function(x = matrix()) {
          
          invMatrix<- NULL
          
          set<- function(y) {
                    x <<- y
                    invMatrix<<- NULL       
          }
          get<- function() x
          setMatrix<- function(solve) invMatrix <<- solve
          getMatrix<- function() invMatrix
          
          list(set= set, get= get,
               setMatrix= setMatrix,
               getMatrix= getMatrix)
}          
          
## following function checks if the stored invMatrix is a NULL and calculates inverse if NULL.
## returns the stored storedMatrix through getMatrix function in the useCacheMatrix

cacheSolve<- function(x, ...) {
          
          invMatrix<- x$getMatrix()
          
          if(!is.null(invMatrix)) {
                    message("getting cached data")
                    return(invMatrix)
          }
          
          matrix<-x$get()
          invMatrix<- solve(matrix, ...)
          x$setMatrix(invMatrix)
          invMatrix
      
        ## Return a matrix that is the inverse of 'x'
}

