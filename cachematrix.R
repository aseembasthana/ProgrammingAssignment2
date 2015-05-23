# The code here describes R function that is able to cache potentially 
# time-consuming computations. 
# The example here is finding the inverse of a matrix which is a typically a
# time consuming operation for a large matrix.
# We have 2 main functions defined here
# 1. makeCacheMatrix - a function that sets up the getters and setters for
#    the matrix and its inverse
# 2. cacheSolve - a function that actually calculates the matrix inverse. 
#    It checks to see if the inverse of a matrix already exists in which case
#    the function returns the inverse from the cache, else it calculates the 
#    inverse of the matrix.  

#The function, makeCacheMatrix creates a list containing a function to
#1.set the matrix (setMatrix function)
#2.get the matrix (getMatrix function)
#3.set the inverse of the matrix (setMatrixInverse function)
#4.get the inverse of the the matrix (getMatrixInverse function)
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        setMatrix <- function(y) {
                x <<- y
                m <<- NULL
        }
        getMatrix <- function() x
        setMatrixInverse <- function(inverseMatrix) m <<-inverseMatrix 
        getMatrixInverse <- function() m
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setMatrixInverse = setMatrixInverse,
             getMatrixInverse = getMatrixInverse)
}


# The function cacheSolve calculates the Inverse Matrix of the  "list" 
# created with the function makeCacheMatrix
# It first checks to see if the inverse matrix has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the matrix  
# and sets the value of the inverse Matrix in the cache via the 
# setMatrixInverse function.
cacheSolve <- function(x, ...) {
        m <- x$getMatrixInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$getMatrix()
        m <- solve(data, ...)
        x$setMatrixInverse(m)
        m
}

