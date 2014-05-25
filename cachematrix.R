# These 2 functions allow computation of an inverse matrix 
# or checking the cache for an existing (same) computation

# this function creates a 'special' matrix to be inversed
# it should receive a vector that translates to a square matrix
# (i.e. with a length of 2, 4, 9 etc.)
makeCacheMatrix <- function(x = matrix()) {
        
        m <- matrix() # initial matrix
        
        # set square matrix from length of a given vector
        setMatrix <- function (m) 
        {       
                x <<- matrix (m, nrow = sqrt(length(m)), ncol = sqrt(length(m)))        
                m <<- matrix (NA)
        }
        
        # returning our matrix
        getMatrix <- function () x
        
        # setting inverse 
        setInverse <- function(inverse) m <<- inverse
        
        # returning our inverse matrix
        getInverse <-function() m
        
        # returning a list with things that we can do
        list (setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


## This function computes or caches the inverse matrix, depending on the previous results
cacheSolve <- function (x, ...) {
        
        # get the last value of the inverse matrix
        m2 <-x$getInverse ()
        
        # check if cache usable
        if (!is.na((m2[1,1]))) 
        {
                message ("Returning cached data")
                return (m2)
        }
        
        # getting our matrix, as cache isn't usable
        data <- x$getMatrix ()
        
        # compute the inverse
        m2 <- solve(data, ...)
        
        # setting new inverse matrix
        x$setInverse (m2)
        
        
}
