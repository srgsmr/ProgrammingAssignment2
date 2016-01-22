## Programmimng Assignment 2 - Lexical Scoping
## functions to calculate the inverse of a matrix from a cached matrix


## function makeCacheMatrix creates a list of function to get/set a matrix 
##                                  and to get/set the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) i <<- solve
    getsolve <- function() i
    
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## function cacheSolve calculates the inverse of the matrix 
##          or return the inversed matrix if it is alread solved in cache
cacheSolve <- function(x, ...) {

    i <- x$getsolve()
    if(!is.null(i)) {
        message("getting cached data")
        return (i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setsolve(i)
    i                   ## Return a matrix that is the inverse of 'x'
}
