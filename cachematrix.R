## Special wrapper functions for caching the inverse results of a solved matrix
## 

## makeCacheMatrix takes a square invertible matrix, stores it, and defines
## four functions to store and retrive the data

makeCacheMatrix <- function(originalMatrix = matrix()) {  
    inversedMatrix <- NULL  #set to NULL as inverse hasen't been created
    # to store the original matrix
    set <- function(newMatrix) { 
        originalMatrix <<- newMatrix
        inversedMatrix <<- NULL
    }
    # to return the original matrix
    get <- function() originalMatrix
    #to store a copy of the inverse of the matrix
    setinverse <- function(aMatrix) inversedMatrix <<- aMatrix
    # to get the copy of the inverse of the matrix
    getinverse <- function() inversedMatrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve returns the inverse of a matrix, either by calculating it, or it
## returns a cached result if this has been called previously

cacheSolve <- function(cachedMatrix, ...) {
    ## Return a matrix that is the inverse of the original matrix
    i <- cachedMatrix$getinverse()
    if(!is.null(i)) { #if not null means we have cached data and can save CPU time                 
        message("getting cached data")
        return(i)
    }
    # if we get here means no cached results, calculate and save for next time
    data <- cachedMatrix$get()
    i <- solve(data, ...)
    cachedMatrix$setinverse(i)
    i
}