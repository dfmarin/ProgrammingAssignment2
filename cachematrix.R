## Coursera: R Programming
## Programming Assignment 2: Lexical Scoping
## Author: Diego Fernando Marin <dfmarin@gmail.com>

## Matrix inversion is usually a costly computation and caching the
## inverse of a matrix rather than compute it every time is a good idea

## makeCacheMatrix(): returns a list of functions used to 
## set/get the matrix, and set/get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    ## the cached version of inverse of the matrix
    inv <- NULL
    ## set/get for the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() { x }
    ## set/get for the inverse of the matrix
    setinverse <- function(inverse) { inv <<- inverse }
    getinverse <- function() { inv }
    # Return a list with defined functions
    list( set=set, get=get, setinverse=setinverse, getinverse=getinverse )
}

## cacheSolve(): returns the inverse of the matrix.
## if the inverse has been computed before, return the cached version. 
## else compute the inverse, save it in cache and return it
cacheSolve <- function(x, ...) {
    ## Try to get the cached inverse of the matrix
    inv <- x$getinverse()
    ## Success! return the cached version
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }   
    ## No cached version, so get the matrix and solved it
    data <- x$get()
    inv <- solve(data)
    ## Save it in the cache
    x$setinverse(inv)
    ## Return a matrix that is the inverse of 'x'
    inv 
}
