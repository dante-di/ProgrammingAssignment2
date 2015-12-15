##Matrix inversion is usually a costly computation and there may be some
##benefit to caching the inverse of a matrix rather than compute it repeatedly.
##
## Usage:
##  My_Matrix <- matrix(c(1, 2, 3, 4), nrow=2, ncol=2)
##  cacheMatrix <- makeCacheMatrix(My_Matrix)
##  cacheSolve(cacheMatrix)
##
##  cacheMatrix$set(My_Matrix)      # Change the cached matrix.
##  My_Matrix <- cacheMatrix$get()  # Returns the cached matrix.


## Creates a "CacheMatrix" that encapsulates the matrix x and its inverse i
## the CacheMatrix is a list of getter/setter functions
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## Returns the inverse of a matrix encapsulated in a CacheMatrix object
cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(is.null(inverse)) {
        #the inverse matrix has not been calculated yet 
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
    }
    #now the inverse matrix should be ok
    inverse
}