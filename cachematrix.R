## Special wrapper functions for caching the inverse results of a solved matrix
## 

## makeCacheMatrix takes a square invertible matrix, stores it, and defines
## four functions: set() to store the original matrix, get() to return the
## original matrix, setinverse to store a copy of the inverse of the matrix,
## getinverse() to get the copy of the inverse of the matrix 

makeCacheMatrix <- function(originalMatrix = matrix()) {  
    inversedMatrix <- NULL
    set <- function(newMatrix) { 
        originalMatrix <<- newMatrix
        inversedMatrix <<- NULL
    }
    get <- function() originalMatrix
    setinverse <- function(aMatrix) i <<- aMatrix
    getinverse <- function() inversedMatrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve returns the inverse of a matrix, either by calculating it, or it
## returns a cached result if this has been called previously

cacheSolve <- function(cachedMatrix, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- cachedMatrix$getinverse()
    if(!is.null(i)) {                  
        message("getting cached data")
        return(i)
    }
    # if we get here means no cached results, calculate and save for next time
    data <- cachedMatrix$get()
    i <- solve(data, ...)
    cachedMatrix$setinverse(i)
    i
}