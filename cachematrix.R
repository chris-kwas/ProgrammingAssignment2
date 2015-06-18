## Special wrapper functions for caching the inverse results of a solved matrix
## 

## makeCacheMatrix takes a square invertible matrix, stores it, and defines
## four functions: set() to store the original matrix, get() to return the
## original matrix, setinverse to store a copy of the inverse of the matrix,
## getinverse() to get the copy of the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(aMatrix) i <<- aMatrix
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve returns the inverse of a matrix, either by calculating it, or if
## returns a cached result if this  has been called previously

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}