## Gives the invert of a defined matrix 
## if the original matrix avide by the rules regarding inversion

## makeCacheMatrix enables to set and get both a matrix and its invert
## it uses two objects matrixFinal and x, that can be used in other environments
## matrixFinal is the test to check if the matrix is the same or not
## in order to decide whether you use the cache data or calculate again

makeCacheMatrix <- function(x = matrix()) {
    
    matrixFinal <- NULL
      
    set <- function(values, numbRow, numbCol){
            x<<-matrix(values, numbRow, numbCol)
            matrixFinal <<- NULL
        }
  
    get <- function() {
            x
        }
      
    setInvert <- function(x){
        
            if (nrow(x$get())==ncol(x$get()) && det(x$get())!=0) {
              
              matrixFinal <<- solve(x$get())
              matrixFinal
            } else {
              message("Matrix not invertible")
              matrixFinal <- NULL
            }
       }
  
    getInvert <- function(){
            matrixFinal
       }
      
    list(set = set, get = get, setInvert = setInvert, getInvert = getInvert)
}


## cacheSolve determines whether or not you need to go through calculation
## by checking if the invert has been calculated already == are we using the same matrix
## that has been set before or can we use the cache data
## It returns the invert of that matrix

cacheSolve <- function(x, ...) {
    
    test <- x$getInvert()  
    if (!is.null(test)) {
        message("Getting cached data")
        return(test)
    }
    
    x$setInvert(x)
    test <- x$getInvert()
    test
  }
  