#R programming - Assignment 2
## Created by Luis Boza

## Creat a function called "makeMatrix" that returns a list of functions
makeMatrix <- function(x = matrix()) {
## Sendind a warning massage if the "x" is not square matrix 
        if(ncol(x)!=nrow(x)) {stop("The matrix is not square")}
## * setmatrix      set the value of a matrix
## * getmatrix      get the value of a matrix
## * cacheInverse   get the cahced value (inverse of the matrix)
## * getInverse     get the cahced value (inverse of the matrix)     
        m <- NULL
        setmatrix <- function(y) {
                x <<- y
                m <<- NULL
        }
        getmatrix <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)
}

# The  function called "makeInverse" calculates the inverse of a "special" matrix created with 
#the function "makeCacheMatrix"

cacheInverse <- function(x, ...) {
        # geting the cached value
        m <- x$getinverse()
        # if a cached value exists return it
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        # otherwise get the matrix, caclulate the inverse and store it in
        # the cache
        data <- x$getmatrix()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
