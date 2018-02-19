## These functions calculate the inverse of a matrix and caches it

## This function creates a matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        ## This part assigns the input argument to the x object in the parent 
        ## environment and assigns the value of NULL to the inv object
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        
        ## This part retrieves the value of x
        get <- function() x
        

        ## This part defines the setter for the inverse
        setInverse <- function(solveMatrix) inv <<- solveMatrix
        
        ## This part defines the getter for the inverse
        getInverse <- function() inv
        
        ## This part assigns each of these functions as elements in a list
        ## and returns them to the parent environment
        list(set = set, get = get, setInverse = setInverse, getInverse = 
                     getInverse)
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix.
cacheSolve <- function(x, ...) {
        
        ## This part returns a matrix that is the inverse of 'x'
        cacheSolve <- function(x, ...) {
                inv <- x$getInverse()
                if(!is.null(inv)){
                        message("getting cached data ")
                        return(inv)
                }
                data <- x$get()
                inv <- solve(data)
                x$setInverse(inv)
                inv      
        }
}