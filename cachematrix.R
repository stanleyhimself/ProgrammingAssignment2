# -----------------------------------------------------------------------------
# In short, the cacheSolve function returns an inverse of a matrix 
# input by the user, saving calculation time when run for the 2nd time 
# with the same input data (due to caching). The input is realised 
# via makeChacheMatrix function, which is creating the matrix object 
# in global environment. 

# -----------------------------------------------------------------------------
# makeChacheMatrix function:
# - allows the user to define the input matrix to be used in further 
# calculations;
# - defines the sub-functions (methods) for accessing the input matrix 
# and storing/retrieving/resetting the inverse matrix 
# (which is calculated and returned by cacheSolve function).

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get, setInverse = setInverse, 
             getInverse = getInverse)
}

# -----------------------------------------------------------------------------
# When run for the first time with a given input matrix, 
# the cacheSolve function does the following: 
# - calculates and returns the inverse of the input matrix;
# - stores the calculated inverse matrix in cache memory 
# via makeCacheMatrix function method.
# When run again with the same input matrix, the cacheSolve function
# retrieves and returns the cached value of the inverse matrix 
# via makeCacheMatrix function method and communicates 
# the appropriate message.

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("Getting cached data...")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m) 
        m
}
