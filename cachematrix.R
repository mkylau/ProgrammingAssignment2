## Below functions combine to takes a matrix and tries to inverse it if it does not already have it cached. If it is already cached, returns cached solution.

## This function creates the cache of the matrix

makeCacheMatrix <- function(x = matrix()) {
        Inverse <- NULL
        set <- function(y) {
                x <<- y
                Inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(InverseMatrix) Inverse <<- InverseMatrix
        getInverse <- function() Inverse
        list(set = set, get = get,
            setInverse = setInverse,
            getInverse = getInverse)
}


## This function computes the inverse of the matrix from the makeCacheMatrix function. If solved matrix is cached, return cached matrix.

cacheSolve  <- function(x, ...) {
        Inverse <- x$getInverse()
        if(!is.null(Inverse)) {
                message("getting cached Inversed matrix")
                return(Inverse)
        }
        data <- x$get()
        Inverse <- solve(data, ...)
        x$setInverse(Inverse)
        Inverse
}
