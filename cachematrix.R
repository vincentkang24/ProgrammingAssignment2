# makeCacheMatrix is a function creates a special "matrix" object that 
# can cache its inverse.
# Contains the following functions:
# * setMatrix      set the value of a matrix
# * getMatrix      get the value of a matrix
# * cacheInverse   get the cahced value (inverse of the matrix)
# * getInverse     get the cahced value (inverse of the matrix)

makeCacheMatrix <- function(x = matrix()) {
        invs <- NULL
        set <- function(y) {
                x <<- y
                invs <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) invs <<- inverse
        getInverse <- function() invs
        list(set = set, 
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        invs <- x$getInverse()
        if(!is.null(invs)) {
                message("getting cached data")
                return(invs)
        }
        matr <- x$get()
        invs <- solve(matr, ...)
        x$setInverse(invs)
        invs
}