## Computes the inverse of square matrix (that is invertible) and caches it
## In case the inverse has already been calculated, then retrives the information
## cached instead of re-calculating.

## Create a special vector that contain the functions set (to set the matrix
## in the parent environm.), get (to ge the stored matrix), setsolve (to 
## solve the inverse matrix), and getsolve (to get the stored inverse matrix)
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                          ## Set local variable m 

        set <- function(y) {               ## Function to set (parent environm.)
                x <<- y                    ##    variables
                m <<- NULL
        }
        
        get <- function() x                ## Function to get (and return) the 
                                           ##    variable matrix
        
        setsolve <- function(solve){       ## Function to set (parent environ.) 
                m <<- solve                ##    variable with solve
        }
        
        getsolve <- function() m           ## Function to get (and return) a 
                                           ##    variable solve
        
        list(set = set, get = get,         ## Return a list of functions
             setsolve = setsolve,
             getsolve = getsolve)
}


## Return a matrix that is the inverse of 'x'
## If an inverse matrix has already been calculated, it returns the cached value
## Otherwise, solves for the inverse matrix
cacheSolve <- function(x, ...) {
        m <- x$getsolve()                               ## Set local variable m
                                                        ##     to the prior result
                                                        ##     of solve for inverse
        
        if(!is.null(m)) {                               ## Checks if there is a prior
                message("getting cached data")          ##     cached result, returns 
                return(m)                               ##     it, and ends function
        }
        
        data <- x$get()                                 ## Otherwise, gets the matrix
        m <- solve(data, ...)                           ## Solves for its inverse
        x$setsolve(m)                                   
        m

}