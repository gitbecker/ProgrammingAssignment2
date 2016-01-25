
## Peter Becker - Programming Assignment 2
## Two functions to assist with caching the inverse of a matrix
## 2015-01-25

## Function makeCacheMatrix creates a list containing a function to get/set the value and inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Function cachesolve calculates the mean of the special matrix created with the above function. 
## It either returns the cashed mean if previously calculated, otherwise it calculates it before returning.

cacheSolve <- function(x, ...) {
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
