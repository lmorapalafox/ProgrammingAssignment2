## Functions that calculates the Matrix inversion

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        # Initialize Objects
        inv <- NULL
        
        # Define functions for objects
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        
        # Get function
        get <- function() x
        
        # Set the Inverse of a matrix
        setInv <- function(inv) inv <<- inv
        
        # Get the inverse
        getInv <- function() inv
        
        # Create obnject with functions
        list(set = set,
             get = get,
             setInv = setInv,
             getInv = getInv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        # Get the Inverse
        inv <- x$getInv()
        
        # Check for data of the Inverse
        if(!is.null(inv)){
                message("Getting Inverse Matrix from cached data")
                return(inv)
        }

        # Calculate de Invers
        matrix <- x$get()
        inv <- solve(matrix)
        x$setInv(inv)
        inv
}
