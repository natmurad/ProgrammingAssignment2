## First function creates a cache matrix and the second access this matrix and calculate
## its inverse


## makeCacheMatrix takes in an invertible square matrix and returns a list of functions.
## set set caches the passed in data to x and sets the inv value to Null
## get returns the cached x value
## setinv sets the inv variable to the passed in inverse Matrix
## getinv returns the inverse Matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
        x <<- y
        inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Computes the inverse of the matrix returned by makeCacheMatrix.
## If the inverse has already been calculated, cachesolve should retrieve the inverse from the 
## cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
